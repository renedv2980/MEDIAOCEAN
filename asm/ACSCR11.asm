*          DATA SET ACSCR11    AT LEVEL 047 AS OF 06/24/20                      
*PHASE T60C11A,+0                                                               
*&&ONLIN SET   Y                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 44 AS OF 03/26/12         *         
*                                                                     *         
***********************************************************************         
*                                                                               
* PID  LVL DATE    COMMENTS                                                     
* ---------------------------------                                             
* SMAN 042 07SEP11 PR000122 ESTIMATE PHASE 1                                    
* SMAN 043 04OCT11          UK/US MERGE                                         
* MPEN 044 26MAR12 PR002113 RELINK FOR LARGER COBLOCK                           
* GHOA 046 27JUN18 SPEC-20151 MISSING TIME STATUS                               
* GHOA 047 25JUN19 SPEC-37360 EXCLUDE EMPLOYEES WITH NO HOURS                   
*                                                                               
         TITLE 'PERSON (COST) PROFILE ELEMENTS EXTRA MAINTANCE'                 
T60C11   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C11,RA,RR=RE,CLEAR=YES                                       
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
         USING RESRECD,R2                                                       
*                                                                               
         L     RC,APALOCAL                                                      
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         CLC   ATWA,ACURSOR        INSURE CURSOR WAS IN A FIELD                 
         BE    SCR01               NO,  SET CURSOR                              
         CLI   TWALREC,RECCSTPF    IF   FIRST TIME IN, THEN SET CURSOR          
         BE    SCR02               NO,  SKIP                                    
*                                                                               
SCR01    LA    RE,CSTCODEH         FORMAT CODE FIELD                            
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
         MVI   TUPPROF,0                                                        
         MVI   PTMSTAT,PTMSALL     ALL TIME ARE VALID                           
         MVC   DEFTIMST,SPACES     DEFAULT TIME STATUS                          
         USING CAPRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   CAPKEY,SPACES                                                    
         MVI   CAPKTYP,CAPKTYPQ    COST ALLOCATION RECORD TYPE                  
         MVI   CAPKSUB,CAPKSUBQ    COST ALLOCATION RECORD SUB TYPE              
         MVC   CAPKCPY,CUABIN      COMPANY LEVEL ONLY                           
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   VALKEY07                                                         
*                                                                               
         USING OPDELD,R3                                                        
         L     R3,AIOAREA2                                                      
         AH    R3,DATADISP                                                      
         SR    RF,RF                                                            
VALKEY04 CLI   OPDEL,0                                                          
         BE    VALKEY07                                                         
         CLI   OPDEL,OPDELQ        X'A4' - OPTIONS DATA ELEMENT                 
         BNE   *+8                                                              
         CLI   OPDNUM,COTUP#       TUP PROFILE NUMBER                           
         BNE   *+14                                                             
         MVC   TUPPROF,OPDDATA     SAVE OFF TUP PROFILE                         
         B     VALKEY06                                                         
*                                                                               
         IC    RF,OPDLN                                                         
         AR    R3,RF                                                            
         B     VALKEY04                                                         
         DROP  R3                                                               
*                                                                               
VALKEY06 CLI   TUPPROF,C' '        ANY TUP SETTING FROM COST PROFILE            
         BNH   VALKEY07            NO - SHOW ALL TIME                           
         CLI   TUPPROF,COSAVED                                                  
         BE    VALKEY07                                                         
*                                                                               
         CLI   TUPPROF,COFUAPR     FULLY APPROVED ?                             
         BNE   *+8                                                              
         MVI   DEFTIMST,C'F'                                                    
         CLI   TUPPROF,COLMAPR     PART APPROVED (LINE MANAGER) ?               
         BE    *+12                                                             
         CLI   TUPPROF,COCLAPR     PART APPROVED (CLIENT)?                      
         BNE   *+10                                                             
         MVC   DEFTIMST(3),=C'F,P'                                              
         CLI   TUPPROF,COSUBMD     SUBMITTED ?                                  
         BNE   VALKEY07                                                         
         MVC   DEFTIMST(5),=C'F,P,S'                                            
*                                                                               
VALKEY07 CLI   TUPPROF,C' '        ANY SETTING?                                 
         BH    *+8                 YES                                          
         MVI   DEFTIMST,C'F'                                                    
*        CLI   APREPJCL,REPJCL1    PERSON TYPE                                  
*        BNE   VALKEY08            YES - ALL TIME STATUS ARE VALID              
*        MVI   PTMSTAT,PTMSALL     ALL TIME ARE VALID                           
*                                                                               
VALKEY08 LA    R2,IOKEY                                                         
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         GOTO1 AFVAL,CSTCODEH                                                   
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY15                                                         
         TM    CSTCODE+4,FVITHIS   ANY INPUT?                                   
         BZ    *+10                NO                                           
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY                                                         
         MVC   CSTCODE,SAVFORM                                                  
         OI    CSTCODEH+6,FVOXMT   TRANSMIT                                     
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
         MVC   SCRTYP(L'APREPCDE),APREPCDE      MAKE TYPE CST                   
         OI    SCRTYPH+6,FVOXMT                                                 
         CLC   APREPCDE,AC@COST                                                 
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
         BO    VALKEY99            OK TO ADD RECORD                             
         MVI   APINDS,APIOKADD     RECORD NOT ON FILE, SO OK TO ADD             
         L     R2,AIOAREA1                                                      
         XC    RESKEY(256),RESKEY  RESET AIO AREA                               
*                                                                               
VALKEY98 MVC   FVMSGNO,=AL2(FVFOK)                                              
         BAS   RE,DSPOFF           DISPLAY OFFICE FIELD HEADER                  
*                                                                               
VALKEY99 B     EXIT                                                             
         EJECT ,                                                                
DISKEY   NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         EJECT ,                                                                
VALREC   DS    0H                                                               
         L     R2,AIOAREA1                                                      
         MVI   SECBIT,0                                                         
         GOTO1 AFVAL,CSTNMEH                                                    
         BNE   VALREC05            NAME HAS  NOT  BEEN INPUT                    
         GOTO1 ADDNAME,APPARM,(R2),CSTNMEH   ADD  FORMAT NAME                   
         BNE   VALREC99            ON   ERROR,    EXIT                          
*                                                                               
VALREC05 GOTO1 AFVAL,CSTTYPEH                                                   
*                                                                               
         USING REPTABD,R3                                                       
         L     R3,ACTYPTAB                                                      
VALREC10 CLI   REPCODE,EOT                                                      
         BE    IVALTYPE            INVALID TYPE USED                            
         LR    R1,R3                                                            
         BAS   RE,EXPRPTY                                                       
         CLC   APREPCDE,CSTRPCDE   MATCH REPORT TYPE                            
         BNE   VALREC15            NO, SO GET NEXT REPORT TYPE                  
         SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         EXCLC R1,CSTRPTYP,FVIFLD                                               
         BNE   VALREC15            NO, SO GET NEXT REPORT TYPE                  
         TM    REPFLAG,REPDDS      DDS ONLY?                                    
         BZ    VALREC20            NO, SO OK                                    
         TM    CUSTAT,CUSDDS       IS IT A DDS TERMINAL?                        
         BNZ   VALREC20            YES, SO OK                                   
*                                                                               
VALREC15 LA    R3,REPLNQ(,R3)      BUMP TO NEXT REPORT TYPE                     
         B     VALREC10                                                         
         SPACE 1                                                                
***********************************************************************         
*  CREATE C5 UNIT/LEDGER ELEMENT                                      *         
***********************************************************************         
         SPACE 1                                                                
VALREC20 MVC   APREPNUM,REPNUM                                                  
         MVC   APREPCDE,CSTRPCDE                                                
         GOTO1 ADDREPTY,APPARM,AIOAREA1                                         
         BNE   VALREC99            ON  ERROR, EXIT                              
         DROP  R3                                                               
*                                                                               
         GOTO1 GETTYPE,(R2)        RESET APREP VALUES                           
         MVI   LDGFLDH,10          SET DEFAULT LENGTH                           
         MVC   LDGFLD,APREPUL                                                   
         GOTO1 MAKELIST,APPARM,('RFLLDG',LDGFLDH),(R2),                X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         GOTO1 ADDEL,(R2)          ADD X'C5' LEDGER FILTER                      
         BNE   VALREC99            ON  ERROR, EXIT                              
         SPACE 1                                                                
***********************************************************************         
*  BUILD PROFILE DATA ELEMENT                                         *         
***********************************************************************         
         SPACE 1                                                                
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
         BNE   VALREC22                                                         
         SR    RF,RF                                                            
         IC    RF,1(,R1)                                                        
         BCTR  RF,0                                                             
         EXMVC RF,APELEM,0(R1)                                                  
         GOTO1 DELEL,(R2)                                                       
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
*  PROCESS OPTIONS                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALREC22 XC    RPFMETHD,RPFMETHD                                                
         NI    RPFROPT,TURNOFF-RPFXLOCK-RPFOLOCK                                
         NI    RPFPOPT3,TURNOFF-RPFTEDIT-RPFSZERO                               
         NI    RPFOPT5,TURNOFF-RPFISVTM-RPFOSVTM                                
*&&UK*&& NI    RPFOPT6,TURNOFF-RPFFPDRD                                         
*&&US*&& NI    RPFOPT7,TURNOFF-RPFFPDRD                                         
*                                                                               
         CLC   CSTLOCK,APNO                                                     
         BNE   *+8                                                              
         OI    RPFROPT,RPFXLOCK    EXCLUDE LOCKED ACCOUNTS                      
         CLC   CSTLOCK,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFROPT,RPFOLOCK    ONLY LOCKED ACCOUNTS                         
*                                                                               
         CLC   CSTSVTM,APYES                                                    
         BNE   *+8                                                              
         OI    RPFOPT5,RPFISVTM    INCLUDE SAVED TIME SHEETS                    
         CLC   CSTSVTM,APONLY                                                   
         BNE   *+8                                                              
         OI    RPFOPT5,RPFOSVTM    ONLY SAVED TIME SHEETS                       
*                                                                               
         CLC   CSTCERO,APYES                                                    
         BNE   *+8                                                              
         OI    RPFPOPT3,RPFSZERO   SHOW ZERO HR TIMESHEET                       
*                                                                               
         CLC   CSTTEDT,APYES                                                    
         BNE   *+8                                                              
         OI    RPFPOPT3,RPFTEDIT   TIMESHEET EDIT REPORTING                     
*                                                                               
         CLC   CSTFPDD,APYES                                                    
         BNE   *+8                                                              
*&&UK*&& OI    RPFOPT6,RPFFPDRD    FILTER PERIOD DATE RANGE BY DAY              
*&&US*&& OI    RPFOPT7,RPFFPDRD    FILTER PERIOD DATE RANGE BY DAY              
*                                                                               
         GOTO1 AFVAL,CSTOFSCH                                                   
         MVI   RPFCLIOF,C'P'                                                    
         CLI   CSTOFSC,C'P'        PERSON OFFICE    SECURITY (DEFAULT)          
         BE    VALREC25                                                         
         OI    SECBIT,STYSOFF      X'01'  OFFICE REPORTING BY CLIENT            
         MVI   RPFCLIOF,C'C'                                                    
         CLI   CSTOFSC,C'C'        CLIENT OFFICE    ONLY                        
         BE    VALREC25                                                         
         MVI   RPFCLIOF,C'B'                                                    
         CLI   CSTOFSC,C'B'        PERSON OR CLIENT OFFICE                      
         BNE   IVALIPUT                                                         
*                                                                               
VALREC25 XC    RPFFLT1(4),RPFFLT1  CLEAR FILTER AREA                            
         MVI   RPFFLT5,0           CLEAR FILTER 5                               
         LA    R4,5                FOR FILTER TYPES                             
         LA    R3,RPFFLT1                                                       
         LA    R1,CSTF1H                                                        
*                                                                               
VALREC27 GOTO1 AFVAL                                                            
         BNE   VALREC28                                                         
         MVC   0(1,R3),FVIFLD                                                   
         CLI   FVIFLD,C'*'         EXCLUDE FILTER?                              
         BNE   VALRC27A                                                         
         CLI   FVILEN,2            IS   THE  LENGTH 2 ?                         
         BNE   IVALIPUT            NO,  INVALID INPUT                           
         MVC   0(1,R3),FVIFLD+1                                                 
         NI    0(R3),TURNOFF-X'40' MAKE LOWER CASE                              
         B     VALREC28                                                         
*                                                                               
VALRC27A CLI   FVILEN,1            IS   THE  LENGTH 1 ?                         
         BNE   IVALIPUT            NO,  INVALID INPUT                           
*                                                                               
VALREC28 SR    RE,RE                                                            
         IC    RE,0(,R1)           BUMP TO PROTECTED FIELD                      
         AR    R1,RE                                                            
         IC    RE,0(,R1)           BUMP TO NEXT INPUT FILTER                    
         AR    R1,RE                                                            
         LA    R3,1(,R3)           BUMP TO NEXT FILTER IN PROFILE ELEM          
         CHI   R4,2                SEE IF WE DONE F1-F4                         
         BNE   *+8                 NO NOT YET                                   
         LA    R3,RPFFLT5          POINT TO PROFILE F5 AREA                     
         BCT   R4,VALREC27                                                      
*                                                                               
         MVI   SAVEMTHD,C' '                                                    
         MVI   APELCODE,RPFELQ     X'C4'                                        
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  PROFILE   ELEMENT X'C4'                 
         BNE   VALREC99            ON   ERROR,    EXIT                          
         BAS   RE,DSPOFF           DISPLAY OFFICE FIELD HEADER                  
         MVC   SAVEKEY1,IOKEY                                                   
         SPACE 1                                                                
***********************************************************************         
*  VALIDATE SPECIFIC ACCOUNT(S)                                       *         
***********************************************************************         
         SPACE 1                                                                
         GOTO1 MAKELIST,APPARM,('RFLACC',CSTACCTH),(R2),               X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC34            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 VALLIST,APPARM,APREPUL,(CSTBLOCK,CSTBLOCK+12),1                  
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
         LA    R4,CSTBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
VALREC32 DS    0H                                                               
         CLI   0(R4),LACCOUNT      IS   THE LENGTH OF ACCOUNT > 12 ?            
         BH    IVALACCT            YES, INVALID ACCOUNT                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
         MVC   ACTKUNT(2),APREPUL  UNIT LEDGER                                  
         MVC   CURUL,APREPUL       CURRENT UNIT/LEDGER                          
         MVC   ACTKACT,12(R4)      ACCOUNT IS IN CSTBLOCK                       
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    VALREC33            VALID, CHECK NEXT BLOCK                      
*                                  NOT   VALID, CHECK FOR WILD CARD             
         MVI   BYTE,C'N'           NO    UNIT/LEDGER CHECK                      
         BAS   RE,WILDCARD         CHECK FOR WILD CARD                          
         BNE   IVALACCT            INVALID ACCOUNT                              
*                                                                               
VALREC33 DS    0H                  CHECK NEXT BLOCK                             
         LA    R4,32(,R4)          NEXT BLOCK IN CSTBLOCK                       
         BCT   R6,VALREC32                                                      
         DROP  R2                                                               
*                                                                               
VALRC33A DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  SPECIFIC  ACCOUNT ELEMENT               
         BNE   VALREC99            ON   ERROR,    EXIT                          
         SPACE 1                                                                
***********************************************************************         
*  VALIDATE CONTRA ACCOUNT(S)                                         *         
***********************************************************************         
         SPACE 1                                                                
VALREC34 GOTO1 MAKELIST,APPARM,('RFLCNTR',CSTCTRH),(R2),               X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC38            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         MVC   APWORK(2),SPACES    INDICATE CONTRA ACCOUNT                      
         GOTO1 VALLIST,APPARM,APWORK,(CSTBLOCK,CSTBLOCK+12),1                   
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC35            NO   LIST                                    
         BP    VALRC34A            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BE    VALREC37            YES, ADD CONTRA LIST                         
         B     IVAL2MNY            NO,  TOO MANY LISTS                          
*                                                                               
VALRC34A DS    0H                  SHOW WHICH CONTRA CODE IS BAD                
*                                  INSERT NAME                                  
         MVC   FVXTRA(20),CSTBLOCK+12                                           
         B     VALREC99            GENERATE INVALID LIST                        
*                                                                               
VALREC35 DS    0H                  VALIDATE CONTRA ACCOUNT(S)                   
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,CSTBLOCK                                                      
*                                                                               
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
VALREC36 DS    0H                  VALIDATE A CONTRA ACCOUNT                    
         CLI   0(R4),0             ANY  ACCOUNT REQUESTED ?                     
         BE    VALRC36H            NO,  CHECK NEXT BLOCK                        
         CLI   0(R4),LULACNT       IS   THE LENGTH OF CONTRA  > 14 ?            
         BH    IVALCNTR            YES, INVALID CONTRA ACCOUNT                  
         CLI   0(R4),LUNLG         IS   THE LENGTH OF CONTRA  <  2 ?            
         BL    IVALCNTR            YES, INVALID CONTRA ACCOUNT                  
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
*                                                                               
         USING CNTRTBLD,RF         CONRA ACCOUNT TABLE                          
         L     RF,ACCNTTAB         ADDR OF CONTRA TABLE                         
         CLI   APREPJCL,REPJCLV    IS   IT PRODUCTION  TYPE ?                   
         BE    VALRC36F            YES, SO ASSUME GOOD U/L                      
         CLI   APREPJCL,REPJCLX    IS   IT EXPENSE     TYPE ?                   
         BE    VALRC36F            YES, SO ASSUME GOOD U/L                      
         CLI   APREPJCL,REPJCLP    IS   IT PAYABLES    TYPE ?                   
         BE    VALRC36F            YES, SO ASSUME GOOD U/L                      
*                                                                               
VALRC36B DS    0H                                                               
         CLC   APREPNUM,CNTRRTYP   MATCHING REPORT TYPE ?                       
         BNE   VALRC36D            NO,  TRY NEXT ENTRY                          
         CLC   CNTRUNLG,12(R4)     MATCHING UNIT/LEDGER ?                       
         BE    VALRC36F            YES, GOOD UNIT/LEDGER                        
*                                                                               
VALRC36D DS    0H                                                               
         LA    RF,CNTRLNQ(,RF)     NEXT TABLE ENTRY                             
         CLI   0(RF),EOT           END  OF CONTRA TABLE ?                       
         BNE   VALRC36B            NO,  TRY AGAIN                               
         B     IVALCNTR            YES, INVALID CONTRA UNIT/LEDGER              
         DROP  RF                                                               
*                                                                               
VALRC36F DS    0H                  VALID CONTRA UNIT/LEDGER                     
         MVC   ACTKUNT(14),12(R4)  CONTRA ACCOUNT IS IN CSTBLOCK                
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   IVALCNTR            NOT VALID, INVALID CONTRA ACCOUNT            
*                                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
*                                  ! NOTE: WILD CARD CHARACTERS ARE   !         
*                                  !       NOT VALID FOR CONTRA       !         
*                                  !       ACCOUNTS                   !         
*                                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
*                                                                               
VALRC36H DS    0H                  CHECK NEXT BLOCK                             
         LA    R4,32(,R4)          NEXT BLOCK IN CSTBLOCK                       
         BCT   R6,VALREC36                                                      
         DROP  R2                                                               
*                                                                               
VALREC37 DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  SPECIFIC CONTRA ACCOUNT ELEMENT         
         BNE   VALREC99            ON   ERROR,   EXIT                           
         SPACE 1                                                                
***********************************************************************         
*  VALIDATE CLIENT ACCOUNT(S)                                         *         
***********************************************************************         
         SPACE 1                                                                
VALREC38 DS    0H                                                               
*&&US                                                                           
         GOTO1 MAKELIST,APPARM,('RFLCLI',CSTCLIH),(R2),                X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC43            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
*        CLI   APREPNUM,REP#CST    *** MAYBE USED FOR LATER ***                 
*        BNE   IVALIPUT                                                         
         GOTO1 VALLIST,APPARM,=C'SJ',(CSTBLOCK,CSTBLOCK+12),0                   
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC39            NO   LIST                                    
         BP    VALREC99            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,2            ONLY ALLOWED ON FOR A LIST                   
         BL    VALREC42            ADD  THE VALID LIST                          
         BH    IVAL2MNY                                                         
         GOTO1 VALLIST,APPARM,=C'SJ',                                  X        
               (CSTBLOCK+L'CSTBLOCK,CSTBLOCK+L'CSTBLOCK+12),0                   
         LTR   RF,RF               TEST THE RETURN CODE                         
*                                  GOOD LIST                                    
         BZ    VALREC42            ADD  THE VALID LISTS                         
         BP    VALREC99            BAD  LIST                                    
*                                  NO   LIST                                    
*                                  BUT  - MUST BE A LIST                        
         B     IVALLIST            THEREFORE, THIS IS A BAD LIST                
*                                                                               
VALREC39 DS    0H                                                               
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,CSTBLOCK                                                      
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
VALREC41 DS    0H                                                               
         LA    R4,32(,R4)          NEXT BLOCK IN CSTBLOCK                       
         BCT   R6,VALREC40                                                      
         DROP  R2                                                               
*                                                                               
VALREC42 DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  CLIENT    LIST                          
         BNE   VALREC99            ON   ERROR,    EXIT                          
*&&                                                                             
         SPACE 1                                                                
***********************************************************************         
*  VALIDATE OFFICE                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALREC43 GOTO1 MAKELIST,APPARM,('RFLOFF',CSTOFFH),(R2),                X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC45            NO   INPUT                                   
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
         GOTO1 VOFFICE,APPARM,('VOFTYPEB',CSTBLOCK),(BYTE,(R4))                 
         BNE   VALREC99            BAD    INPUT                                 
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD    OFFICE      LIST                      
         BNE   VALREC99            ON     ERROR,      EXIT                      
         SPACE 1                                                                
***********************************************************************         
*  VALIDATE CLEINT OFFICE                                             *         
***********************************************************************         
         SPACE 1                                                                
VALREC45 GOTO1 MAKELIST,APPARM,('RFLCOFF',CSTCOFFH),(R2),              X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
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
         GOTO1 VOFFICE,APPARM,('VOFTYPEB',CSTBLOCK),(BYTE,(R4))                 
         BNE   VALREC99            BAD    INPUT                                 
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD    OFFICE      LIST                      
         BNE   VALREC99            ON     ERROR,      EXIT                      
         SPACE 1                                                                
***********************************************************************         
*  VALIDATE WORK CODE(S)                                              *         
***********************************************************************         
         SPACE 1                                                                
VALREC50 GOTO1 MAKELIST,APPARM,('RFLWC',CSTWCH),(R2),                  X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC60            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         GOTO1 VALLIST,APPARM,=C'SJ',(CSTBLOCK,CSTBLOCK+12),LITTWRK             
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC51            NO   LIST                                    
         BP    VALREC99            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BE    VALREC58            YES, ADD WORK CODE LIST                      
         B     IVAL2MNY            NO,  TOO MANY LISTS                          
*                                                                               
VALREC51 DS    0H                  VALIDATE WORK CODE(S)                        
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,CSTBLOCK                                                      
*                                                                               
*                                  *** SPECIAL CODE FOR CASE IN WHICH           
*                                  *** THE FIRST WORK CODE IS *X                
         CLI   0(R4),1             ONE     CHARACTER WORK CODE ?                
         BNE   VALREC52            NO,     SKIP                                 
*                                                                               
         USING RFLELD,R9                                                        
         LA    R9,APELEM           ->      FILTER DATA ELEMENT                  
         TM    RFLIND,RFLXCLD      EXCLUDE EXCEPTION ?                          
         BZ    IVALTASK            YES,    INVALID TASK                         
*                                                                               
*                                  *** FOOL MAKELIST BY INSERTING               
*                                  *** AN EXTRA '*' IN FRONT OF THE             
*                                  *** INITIAL INPUT FIELD                      
*                                  INIT   DUMMY WORK CODE FIELD HDR             
         MVI   DWCFLDH,L'DWCFLDH+L'DWCFLD                                       
         MVC   DWCFLD,SPACES       CLEAR  DUMMY WORK CODE FIELD                 
         MVI   DWCFLD,C'*'         INSERT EXTRA '*'                             
         ZIC   RE,FVXLEN           GET    EXMVC LENGTH                          
         EXMVC RE,DWCFLD+1,CSTWC   INSERT INITIAL INPUT                         
         GOTO1 MAKELIST,APPARM,('RFLWC',DWCFLDH),AIOAREA1,             X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         NI    RFLIND,TURNOFF-RFLXCLD     TURN OFF EXCLUDE EXCEPTION            
         DROP  R9                                                               
*                                                                               
         USING WCORECD,R2                                                       
VALREC52 DS    0H                  VALIDATE A WORK CODE                         
         CLI   0(R4),L'WCOKWRK     IS   THE LENGTH OF WORK CODE = 2 ?           
         BNE   IVALTASK            NO,  INVALID TASK                            
         CLC   12(2,R4),=C'99'     SPECIAL WORK CODE  ?                         
         BE    VALREC56            YES, SKIP VALIDATION                         
         CLC   12(2,R4),=C'**'     SPECIAL WORK CODE  ?                         
         BE    VALREC56            YES, SKIP VALIDATION                         
*                                                                               
         LA    R2,IOKEY                                                         
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ    X'0A'                                        
         MVC   WCOKCPY,CUABIN      COMPANY                                      
         MVC   WCOKUNT(2),=C'SJ'                                                
         MVC   WCOKWRK,12(R4)      WORKCODE                                     
         GOTO1 AIO,IORD+IOACCDIR+IO2                                            
         BNE   IVALTASK                                                         
*                                                                               
VALREC56 DS    0H                                                               
         LA    R4,32(,R4)          NEXT WORK CODE                               
         BCT   R6,VALREC52                                                      
         DROP  R2                                                               
*                                                                               
VALREC58 DS    0H                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  WORK CODE LIST                          
         BNE   VALREC99            ON   ERROR,    EXIT                          
         SPACE 1                                                                
***********************************************************************         
*  VALIDATE LOCATION STATUS                                           *         
***********************************************************************         
         SPACE 1                                                                
VALREC60 GOTO1 MAKELIST,APPARM,('RFLLOCS',CSTLOCSH),(R2),              X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC65            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         BAS   RE,VALLOCS          TEST FOR VALID                               
         BNE   VALREC99            NOT  VALID, EXIT                             
*                                                                               
         USING RFLELD,R1                                                        
         LA    R1,APELEM           ->   ELEMENT AREA                            
         MVI   RFLLN,RFLLNQ+1      CHANGE LENGTH                                
         MVC   RFLDATA(1),SAVELOCS USED CODE FOR LANGUAGE SOFT                  
         DROP  R1                                                               
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  LOCATION  STATUS                        
         BNE   VALREC99            ON   ERROR,    EXIT                          
         SPACE 1                                                                
***********************************************************************         
*  VALIDATE PAY CODES AND TRANSLATE                                   *         
***********************************************************************         
         SPACE 1                                                                
VALREC65 GOTO1 MAKELIST,APPARM,('RFLPCDE',CSTPCDEH),(R2),              X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC70            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         BAS   RE,PAYCODES         TEST FOR VALID                               
         BNE   VALREC99            NOT  VALID, EXIT                             
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  LOCATION  STATUS                        
         BNE   VALREC99            ON   ERROR,    EXIT                          
         SPACE 1                                                                
***********************************************************************         
*  VALIDATE METHOD TYPE                                               *         
***********************************************************************         
         SPACE 1                                                                
VALREC70 GOTO1 MAKELIST,APPARM,('RFLMTHD',CSTMETHH),(R2),              X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC75            NO   INPUT                                   
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
         SPACE 1                                                                
***********************************************************************         
*  VALIDATE CONTRA FILTERS                                            *         
***********************************************************************         
         SPACE 1                                                                
*                                  ************************************         
VALREC75 DS    0H                  * CONTRA FILTER 1                  *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLCFLT1',CSTCF1H),(R2),              X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC76            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         LA    R4,CSTBLOCK                                                      
         CLI   0(R4),1             IS   THE  LENGTH 1 ?                         
         BNE   IVALIPUT            NO,  INVALID INPUT                           
*                                                                               
         L     R2,AIOAREA1         ->   RECORD                                  
         GOTO1 ADDEL,(R2)          ADD  THE  ELEMENT                            
         BNE   VALREC99            ON   ERROR,    EXIT                          
*                                                                               
*                                  ************************************         
VALREC76 DS    0H                  * CONTRA FILTER 2                  *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLCFLT2',CSTCF2H),(R2),              X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC77            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         LA    R4,CSTBLOCK                                                      
         CLI   0(R4),1             IS   THE  LENGTH 1 ?                         
         BNE   IVALIPUT            NO,  INVALID INPUT                           
*                                                                               
         L     R2,AIOAREA1         ->   RECORD                                  
         GOTO1 ADDEL,(R2)          ADD  THE  ELEMENT                            
         BNE   VALREC99            ON   ERROR,    EXIT                          
*                                                                               
*                                  ************************************         
VALREC77 DS    0H                  * CONTRA FILTER 3                  *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLCFLT3',CSTCF3H),(R2),              X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC78            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         LA    R4,CSTBLOCK                                                      
         CLI   0(R4),1             IS   THE  LENGTH 1 ?                         
         BNE   IVALIPUT            NO,  INVALID INPUT                           
*                                                                               
         L     R2,AIOAREA1         ->   RECORD                                  
         GOTO1 ADDEL,(R2)          ADD  THE  ELEMENT                            
         BNE   VALREC99            ON   ERROR,    EXIT                          
*                                                                               
*                                  ************************************         
VALREC78 DS    0H                  * CONTRA FILTER 4                  *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLCFLT4',CSTCF4H),(R2),              X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC79            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         LA    R4,CSTBLOCK                                                      
         CLI   0(R4),1             IS   THE  LENGTH 1 ?                         
         BNE   IVALIPUT            NO,  INVALID INPUT                           
*                                                                               
         L     R2,AIOAREA1         ->   RECORD                                  
         GOTO1 ADDEL,(R2)          ADD  THE  ELEMENT                            
         BNE   VALREC99            ON   ERROR,    EXIT                          
*                                                                               
*                                  ************************************         
VALREC79 DS    0H                  * CONTRA FILTER 5                  *         
*                                  ************************************         
         GOTO1 MAKELIST,APPARM,('RFLCFLT5',CSTCF5H),(R2),              X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC80            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         LA    R4,CSTBLOCK                                                      
         CLI   0(R4),1             IS   THE  LENGTH 1 ?                         
         BNE   IVALIPUT            NO,  INVALID INPUT                           
*                                                                               
         L     R2,AIOAREA1         ->   RECORD                                  
         GOTO1 ADDEL,(R2)          ADD  THE  ELEMENT                            
         BNE   VALREC99            ON   ERROR,    EXIT                          
***********************************************************************         
*  VALIDATE TIME STATUS CODES AND TRANSLATE                           *         
***********************************************************************         
         SPACE 1                                                                
VALREC80 GOTO1 MAKELIST,APPARM,('RFLTSTA',CSTTSTAH),(R2),              X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC85            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         BRAS  RE,VALTST           TEST FOR VALID                               
         BNE   VALREC99            NOT  VALID, EXIT                             
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  LOCATION  STATUS                        
         BNE   VALREC99            ON   ERROR,    EXIT                          
         SPACE 1                                                                
***********************************************************************         
*  VALIDATE MISSING TIME STATUS CODES AND TRANSLATE                   *         
***********************************************************************         
         SPACE 1                                                                
VALREC85 GOTO1 MAKELIST,APPARM,('RFLMISST',CSTMTSTH),(R2),             X        
               ('MAXPARM',CSTBLOCK),APELEM                                      
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALREC90            NO   INPUT                                   
         BP    VALREC99            BAD  INPUT                                   
*                                  GOOD INPUT                                   
         BRAS  RE,VALTST           TEST FOR VALID                               
         BNE   VALREC99            NOT  VALID, EXIT                             
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD  LOCATION  STATUS                        
         BNE   VALREC99            ON   ERROR,    EXIT                          
         SPACE 1                                                                
***********************************************************************         
*  SET PROFILE SECURITY                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING STYELD,R1                                                        
VALREC90 MVC   IOKEY,SAVEKEY1      RESTORE THE KEY                              
*        CLI   SECBIT,0                                                         
*        BE    VALREC95                                                         
         L     R1,AIOAREA1                                                      
         MVI   APELEM,STYELQ       X'25' REPORT TYPE ELEMENT                    
         GOTO1 GETEL                                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   STYSEC#5,SECBIT     SET PROFILE SECURITY                         
         DROP  R1                                                               
***********************************************************************         
*  UPDATE THE RECORD WITH ALL THE NEW ELEMENTS                        *         
***********************************************************************         
         SPACE 1                                                                
VALREC95 GOTO1 ADDID,APPARM,AIOAREA1                                            
         BNE   VALREC99            ON     ERROR,     EXIT                       
         LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD     ADD    A     RECORD ?                        
         BO    VALREC97                                                         
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA     CHANGE A     RECORD ?                        
         BO    VALREC97                                                         
         DC    H'0'                WHAT   THE   HELL -    ABEND !               
*                                                                               
VALREC97 DS    0H                                                               
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD    WRITE OR   SOMETHING DUDE             
*                                                                               
VALREC99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    DISREC                                                           
         B     IVALEXIT                                                         
         EJECT ,                                                                
         USING RPFELD,R1                                                        
DSPOFF   NTR1                                                                   
         L     R2,AIOAREA1                                                      
         LA    R4,CSTCS08H         GET HEADER ADDRESS                           
         OI    6(R4),FVOXMT        TRANSMIT FIELD                               
         MVI   APELCODE,RPFELQ     PROFILE ELEMENT X'C4'                        
         LA    R3,1740             "PERSON OFFICE" (DEFAULT)                    
         GOTO1 GETEL,(R2)                                                       
         BNE   DSPOFF20                                                         
         CLI   RPFCLIOF,C'C'       CONTRA SECURITY ?                            
         BNE   *+8                                                              
         LA    R3,1741             "CONTRA OFFICE"                              
         CLI   RPFCLIOF,C'B'       PERSON OR CONTRA SECURITY ?                  
         BNE   *+8                                                              
         LA    R3,1742             "1R OR 1C OFFICE"                            
*                                                                               
DSPOFF20 GOTO1 TEXTGET,APPARM,(R3),(R4),0                                       
         B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*  VALIDATE PAYCODES                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
         USING PAYRECD,R2                                                       
PAYCODES NTR1                                                                   
         BAS   RE,GETPAY                                                        
         LA    R1,APELEM           ->   ELEMENT AREA                            
         LA    R2,RFLDATA                                                       
         LA    R4,CSTBLOCK         ->   SCAN BLOCK     AREA                     
         SR    RF,RF                                                            
         IC    RF,NPARMS                                                        
*                                                                               
PAYCDE10 LTR   R0,R6               ANY PAYCODES ?                               
         BZ    PAYCDER                                                          
         LA    R3,PAYTAB                                                        
*                                                                               
PAYCDE15 CLC   12(5,R4),0(R3)      MATCH ON PAYCODE                             
         BE    PAYCDE20                                                         
         LA    R3,L'PAYNUM+L'PAYCODE(,R3)                                       
         BCT   R0,PAYCDE15                                                      
         B     PAYCDER                NO MATCH                                  
*                                                                               
PAYCDE20 MVC   0(1,R2),L'PAYCODE(R3)  MOVE IN NUMBER                            
         LA    R2,1(,R2)                                                        
         LA    R4,32(,R4)                                                       
         BCT   RF,PAYCDE10                                                      
         IC    RF,NPARMS                                                        
         LA    RF,RFLLNQ(,RF)      LENGTH OF ELEMENT                            
         STC   RF,RFLLN                                                         
         B     PAYCXIT                                                          
*                                                                               
PAYCDER  MVC   FVMSGNO,=AL2(5)     INVALID CODE                                 
*                                                                               
PAYCXIT  CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                 EXIT                                         
         DROP  R1                                                               
         EJECT                                                                  
GETPAY   NTR1                      ->   KEY  AREA                               
         SR    R6,R6               COUNT THE NUBMER OF PAYCODES                 
         LA    R2,IOKEY            ->   KEY  AREA                               
         MVC   PAYKEY,SPACES                                                    
         MVI   PAYKTYP,PAYKTYPQ    X'3E'                                        
         MVI   PAYKSUB,PAYKSUBQ    X'03'                                        
         MVC   PAYKCPY,CUABIN                                                   
         MVI   PAYKSEQ,0           FIRST RECORD                                 
         LA    R1,IO3+IOACCFIL+IOHI                                             
         LA    R3,PAYTAB                                                        
*                                                                               
GETPAY10 GOTO1 AIO                                                              
         L     R2,AIOAREA3                                                      
         CLC   PAYKTYP(3),IOKEY                                                 
         BNE   GETPAYX                                                          
         AH    R2,DATADISP                                                      
*                                                                               
         USING PAYELD,R2                                                        
GETPAY12 CLI   0(R2),0             EOR ?                                        
         BE    GETPAY16                                                         
         CLI   PAYEL,PAYELQ        X'84', PAYROLL ELEMENT                       
         BNE   GETPAY15                                                         
         LA    R6,1(,R6)           COUNT THE NUMBER OF PAYCODES FOUND           
         MVC   0(L'PAYCODE,R3),PAYCODE                                          
         MVC   L'PAYCODE(L'PAYNUM,R3),PAYNUM                                    
         LA    R3,L'PAYNUM+L'PAYCODE(,R3)                                       
         MVI   0(R3),EOT           MARK END                                     
*                                                                               
GETPAY15 SR    RF,RF                                                            
         IC    RF,PAYLN                                                         
         AR    R2,RF                                                            
         B     GETPAY12                                                         
         DROP  R2                                                               
*                                                                               
GETPAY16 LA    R1,IO3+IOACCFIL+IOSEQ                                            
         B     GETPAY10            NEXT RECORD                                  
*                                                                               
GETPAYX  XIT1  REGS=(R6)                                                        
         EJECT                                                                  
***********************************************************************         
*  VALIDATE METHOD TYPE                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
         USING CAHRECD,R2                                                       
VALMTHD  NTR1                                                                   
         LA    R1,APELEM           ->   ELEMENT AREA                            
         LA    R2,IOKEY            ->   KEY  AREA                               
         LA    R4,CSTBLOCK         ->   SCAN BLOCK     AREA                     
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
VALMTH10 DS    0H                                                               
         CLI   0(R4),L'CMTKMTHD    MORE THAN 3    CHARACTERS ?                  
         BH    VALMTHER            YES, INVALID   METHOD                        
         MVI   CMTKSUB,CMTKSUBQ    CODE      =    X'02'                         
         MVC   CMTKMTHD,12(R4)     INSERT    CODE                               
*                                                                               
VALMTH20 DS    0H                                                               
         GOTO1 AIO,IO3+IOACCFIL+IORD                                            
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
         MVC   CSTMETH,METCODE     DISPLAY   THE  CODE                          
         OI    CSTMETHH+6,FVOXMT   TRANSMIT                                     
         B     VALMTHEX                                                         
*                                                                               
VALMTHE1 DS    0H                  TOO  MANY PARAMETERS                         
         MVC   FVMSGNO,=AL2(ACE2MANY)                                           
         B     VALMTHEX                                                         
*                                                                               
VALMTHER DS    0H                                                               
         MVC   FVMSGNO,=AL2(1650)  INVALID   METHOD                             
*                                                                               
VALMTHEX DS    0H                  SET  CC                                      
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                 EXIT                                         
         DROP  R1,R2                                                            
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE LOCATION STATUS                                           *         
***********************************************************************         
         SPACE 1                                                                
VALLOCS  NTR1                                                                   
         CLI   NPARMS,1            ONLY 1 LOCATION STATUS REQUESTED ?           
         BNE   VALLOCE1            NO,  TOO MANY PARAMETERS                     
*                                                                               
         LA    R4,CSTBLOCK         ->   SCAN BLOCK     AREA                     
         CLI   0(R4),L'CSTLOCS-1   MORE THAN 8 CHARACTERS OF INPUT ?            
         BH    VALLOCER            YES, INVALID INPUT                           
*                                                                               
         ZIC   R3,0(,R4)           GET  INPUT LENGTH                            
         BCTR  R3,0                SUBTRACT ONE FOR EXECUTE                     
         L     R6,ACLOSTAB         LOCATION STATUS TABLE                        
*                                                                               
VALLOC10 DS    0H                                                               
         CLI   0(R6),EOT           END  OF TABLE ?                              
         BE    VALLOCER            YES, INVALID INPUT                           
         MVC   LOSTATUS,1(R6)      MOVE LOCATION STATUS TO WORK AREA            
*                                  GET  LOCATION STATUS VALUE                   
         GOTO1 VDICTAT,APPARM,C'SU  ',('LLOSTATU',LOSTATUS),0                   
         EX    R3,VALLOCCL         COMPARE INPUT TO LOC STATUS VALUE            
         BE    VALLOC20            MATCH, PROCESS FOUND MATCH                   
*                                                                               
         LA    R6,LLOSTATU+1(,R6)  NEXT TABLE ENTRY                             
         B     VALLOC10            TRY  AGAIN                                   
*                                                                               
VALLOCCL CLC   LOSTATUS(0),12(R4)  COMPARE TABLE ENTRY TO INPUT DATA            
*                                                                               
VALLOC20 DS    0H                                                               
         MVC   SAVELOCS,0(R6)      SAVE LOCATION STATUS                         
*                                                                               
*                                  RERUN THE LOOP SINCE THERE MAY BE            
*                                       AN EARLIER ENTRY WITH THE               
*                                       THE SAME CODE, E.G. SEE                 
*                                       AC#LOA AND AC#LEAVE                     
*                                                                               
         L     R6,ACLOSTAB         LOCATION STATUS TABLE                        
*                                                                               
VALLOC30 DS    0H                                                               
         CLI   0(R6),EOT           END  OF TABLE ?                              
         BNE   *+6                 NO,  CONTINUE                                
         DC    H'0'                YES, ABEND                                   
         CLC   SAVELOCS(1),0(R6)   COMPARE SAVED LOC STATUS TO TABLE            
         BE    VALLOC40            MATCH, PROCESS FOUND MATCH                   
*                                                                               
         LA    R6,L'CSTLOCS(,R6)   NEXT TABLE ENTRY                             
         B     VALLOC30            TRY  AGAIN                                   
*                                                                               
         USING RFLELD,R1                                                        
*                                                                               
VALLOC40 DS    0H                                                               
         MVC   LOSTATUS,1(R6)      MOVE LOCATION STATUS TO WORK AREA            
*                                  GET  LOCATION STATUS VALUE                   
         GOTO1 VDICTAT,APPARM,C'SU  ',('LLOSTATU',LOSTATUS),0                   
         LA    R1,APELEM           ->     ELEMENT AREA                          
         MVC   CSTLOCS,SPACES      CLEAR  INPUT FIELD                           
*                                  INSERT EXPANDED INPUT                        
         MVC   CSTLOCS(L'CSTLOCS-1),LOSTATUS                                    
         TM    RFLIND,RFLXCLD      EXCLUDE REQUESTED ?                          
         BZ    VALLOC50            NO,  SKIP                                    
         MVI   CSTLOCS,C'*'        INSERT '*'                                   
*                                  FOLLOWED BY EXPANDED INPUT                   
         MVC   CSTLOCS+1(L'CSTLOCS-1),LOSTATUS                                  
*                                                                               
VALLOC50 DS    0H                                                               
         OI    CSTLOCSH+6,FVOXMT   TRANSMIT                                     
         B     VALLOCEX            EXIT                                         
*                                                                               
VALLOCE1 DS    0H                  TOO  MANY PARAMETERS                         
         MVC   FVMSGNO,=AL2(ACE2MANY)                                           
         B     VALLOCEX            EXIT                                         
*                                                                               
VALLOCER DS    0H                  INVALID INPUT                                
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         MVC   FVXTRA(20),12(R4)   IDENTIFY THE FAILING ELEMENT                 
*        B     VALLOCEX            EXIT                                         
*                                                                               
VALLOCEX DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK) SET  CONDITION CODE                          
         B     XIT                 EXIT                                         
*                                                                               
         DROP  R1                                                               
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
         USING RFLELD,R1                                                        
*                                                                               
WILDYES  LA    R1,APELEM                                                        
         OI    RFLIND,RFLWILD      TURN ON THE BIT                              
         CR    RE,RE               WILDCARD IS GOOD                             
         B     XIT                                                              
*                                                                               
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*   DISPLAY CST PROFILE DATA                                          *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1         PROFILE ELEMENT                              
         MVC   SAVEKEY1,IOKEY                                                   
         TWAXC CSTNMEH,CSTTABH                                                  
         GOTO1 GETNAME,APPARM,(R2),CSTNMEH                                      
         GOTO1 GETPER,APPARM,(R2),CSTOWNH                                       
         XC    SAVEPCDE,SAVEPCDE                                                
*                                                                               
         MVC   CSTTYPE(L'APREPTYP),APREPTYP                                     
         MVI   SAVEMTHD,C' '                                                    
         MVI   SAVELOCS,C' '                                                    
         MVC   CSTLOCK,APYES                                                    
         MVC   CSTTEDT,APNO                                                     
         MVC   CSTSVTM,APNO                                                     
         MVC   CSTFPDD,APNO                                                     
         MVI   CSTOFSC,C'P'        PERSON OFFICE SECURITY (DEFAULT)             
         MVI   CSTCERO,C'N'        SHOW ZERO HOUR TIMESHEET                     
*        MVI   CSTTSTA,C'U'        BRANDOCEAN TIME STATUS DEFAULT               
*                                                                               
         USING RFLELD,R1                                                        
         MVI   APELCODE,RFLELQ     FILTER LIST ELEMENT X'C5'                    
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC30                                                         
*                                                                               
DISREC10 CLI   RFLSEQ,0            HIGH LEVEL FILTERS ONLY                      
         BNE   DISREC28            LOOP                                         
         SR    RF,RF                                                            
         SR    R6,R6                                                            
         IC    R6,RFLLN                                                         
         AHI   R6,-(RFLLNQ+1)                                                   
         BM    DISREC28            SKIP ELEMENT IF LENGTH TOO SMALL             
*                                                                               
         CLI   RFLTYPE,RFLACC      ACCOUNT FILTER LIST                          
         BNE   *+8                                                              
         LA    RF,CSTACCTH                                                      
*                                                                               
         CLI   RFLTYPE,RFLCFLT1    CONTRA  FILTER 1                             
         BNE   *+8                                                              
         LA    RF,CSTCF1H                                                       
*                                                                               
         CLI   RFLTYPE,RFLCFLT2    CONTRA  FILTER 2                             
         BNE   *+8                                                              
         LA    RF,CSTCF2H                                                       
*                                                                               
         CLI   RFLTYPE,RFLCFLT3    CONTRA  FILTER 3                             
         BNE   *+8                                                              
         LA    RF,CSTCF3H                                                       
*                                                                               
         CLI   RFLTYPE,RFLCFLT4    CONTRA  FILTER 4                             
         BNE   *+8                                                              
         LA    RF,CSTCF4H                                                       
*                                                                               
         CLI   RFLTYPE,RFLCFLT5    CONTRA  FILTER 5                             
         BNE   *+8                                                              
         LA    RF,CSTCF5H                                                       
*                                                                               
         CLI   RFLTYPE,RFLCNTR     CONTRA  ACCOUNT FILTER LIST                  
         BNE   *+8                                                              
         LA    RF,CSTCTRH                                                       
*                                                                               
         CLI   RFLTYPE,RFLOFF      OFFICE  FILTER LIST                          
         BNE   *+8                                                              
         LA    RF,CSTOFFH                                                       
*                                                                               
         CLI   RFLTYPE,RFLCOFF     CLIENT OFFICE  FILTER LIST                   
         BNE   *+8                                                              
         LA    RF,CSTCOFFH                                                      
*                                                                               
         CLI   RFLTYPE,RFLWC       WORKCODE                                     
         BNE   *+8                                                              
         LA    RF,CSTWCH                                                        
*                                                                               
*&&US                                                                           
         CLI   RFLTYPE,RFLCLI      CLIENT FILTER LIST                           
         BNE   *+8                                                              
         LA    RF,CSTCLIH                                                       
*&&                                                                             
*                                                                               
         CLI   RFLTYPE,RFLPCDE        PAY CODES                                 
         BNE   DISREC18                                                         
         SR    RE,RE                                                            
         IC    RE,RFLLN                                                         
         BCTR  RE,0                                                             
         EXMVC RE,SAVEPCDE,RFLEL      SAVE THE DATA                             
*                                                                               
DISREC18 CLI   RFLTYPE,RFLLOCS        LOCATION STATUS                           
         BNE   DISREC20                                                         
         MVC   SAVELOCS,RFLDATA                                                 
         TM    RFLIND,RFLXCLD                                                   
         BZ    DISREC20                                                         
         NI    SAVELOCS,TURNOFF-X'40'                                           
*                                                                               
DISREC20 CLI   RFLTYPE,RFLMTHD     METHOD                                       
         BNE   *+10                                                             
         MVC   SAVEMTHD,RFLDATA                                                 
*                                                                               
         CLI   RFLTYPE,RFLTSTA     TIME STATUS                                  
         BNE   *+8                                                              
         LA    RF,CSTTSTAH                                                      
*                                                                               
         CLI   RFLTYPE,RFLMISST    MISSING TIME STATUS                          
         BNE   *+8                                                              
         LA    RF,CSTMTSTH                                                      
*                                                                               
         LTR   RF,RF                                                            
         BZ    DISREC28                                                         
         OI    6(RF),FVOXMT        TRANSMIT                                     
         TM    RFLIND,RFLXCLD                                                   
         BZ    *+12                                                             
         MVI   8(RF),C'*'                                                       
         LA    RF,1(,RF)                                                        
         EXMVC R6,8(RF),RFLDATA    INSERT THE DATA                              
*                                                                               
DISREC28 GOTO1 NEXTEL                                                           
         BE    DISREC10                                                         
*                                                                               
         USING RPFELD,R9                                                        
DISREC30 MVI   APELCODE,RPFELQ                                                  
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC60                                                         
         LR    R9,R1                                                            
*                                                                               
         TM    RPFROPT,RPFXLOCK    EXCLUDE LOCKED ACCOUNTS                      
         BZ    *+10                                                             
         MVC   CSTLOCK,APNO                                                     
         TM    RPFROPT,RPFOLOCK    LOCKED ACCOUNTS ONLY                         
         BZ    *+10                                                             
         MVC   CSTLOCK,APONLY                                                   
         OI    CSTLOCKH+6,FVOXMT   TRANSMIT                                     
*                                                                               
         TM    RPFOPT5,RPFISVTM    INCLUDE SAVED TIME SHEETS                    
         BZ    *+10                                                             
         MVC   CSTSVTM,APYES                                                    
         TM    RPFOPT5,RPFOSVTM    ONLY SAVED TIME SHEETS                       
         BZ    *+10                                                             
         MVC   CSTSVTM,APONLY                                                   
         OI    CSTSVTMH+6,FVOXMT   TRANSMIT                                     
*                                                                               
         TM    RPFPOPT3,RPFTEDIT   TIME WORKSHEET                               
         BZ    *+10                                                             
         MVC   CSTTEDT,APYES                                                    
         OI    CSTTEDTH+6,FVOXMT   TRANSMIT                                     
*                                                                               
         TM    RPFPOPT3,RPFSZERO   SHOW ZERO HR TIMESHEETS                      
         BZ    *+10                                                             
         MVC   CSTCERO,APYES                                                    
         OI    CSTCEROH+6,FVOXMT   TRANSMIT                                     
*                                                                               
         CLI   SAVEMTHD,C' '       WAS  METHOD SUPPLIED IN X'C5' EL ?           
         BH    *+10                YES, SKIP                                    
         MVC   SAVEMTHD,RPFMETHD   NO,  INSERT METHOD FROM X'C4' EL             
*                                  NOTE: IN OLD RECORDS METHOD WAS IN           
*                                        THE X'C4' ELEMENTS - IN NEW            
*                                        RECORDS ITS IN X'C5' ELEMENTS          
*                                                                               
         CLI   RPFCLIOF,C'P'       PERSON OFFICE                                
         BNE   *+8                                                              
         MVI   CSTOFSC,C'P'                                                     
         CLI   RPFCLIOF,C'C'       CLIENT OFFICE                                
         BNE   *+8                                                              
         MVI   CSTOFSC,C'C'                                                     
         CLI   RPFCLIOF,C'B'       PERSON OR CLIENT OFFICE                      
         BNE   *+8                                                              
         MVI   CSTOFSC,C'B'                                                     
         OI    CSTOFSCH+6,FVOXMT   TRANSMIT                                     
*&&UK*&& TM    RPFOPT6,RPFFPDRD    FILTER PERIOD DATE RANGE BY DAY              
*&&US*&& TM    RPFOPT7,RPFFPDRD    FILTER PERIOD DATE RANGE BY DAY              
         BZ    *+10                                                             
         MVC   CSTFPDD,APYES                                                    
         OI    CSTFPDDH+6,FVOXMT   TRANSMIT                                     
         LA    R4,5                FIVE FILTER TYPES                            
         LA    R3,RPFFLT1                                                       
         LA    R1,CSTF1H                                                        
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
         CHI   R4,2                                                             
         BNE   *+8                                                              
         LA    R3,RPFFLT5          POINT TO PROFILE F5 INSTEAD                  
         BCT   R4,DISREC32                                                      
*                                                                               
DISREC60 DS    0H                                                               
         BAS   RE,DISMTHD          DISPLAY METHOD TYPE                          
*                                                                               
         BAS   RE,DISPCDE          DISPLAY PAY CODES                            
*                                                                               
         BAS   RE,DISLOCS          DISPLAY LOCATION STATUS                      
*                                                                               
DISREC99 DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   DISRC999                                                         
         CLI   APACTN,ACTDIS       IN   DISPLAY MODE ?                          
         BE    DISRC999            YES, SKIP                                    
         CLI   TWALREC,RECCSTPF    IF   FIRST TIME IN, THEN SET CURSOR          
         BE    DISRC999            NO,  SKIP                                    
*                                                                               
         LA    RE,CSTCODEH         FORMAT CODE FIELD                            
         ST    RE,APCURSOR         SET  APPLICATION CURSOR                      
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
         MVC   CSTMETH,METCODE                                                  
         OI    CSTMETHH+6,FVOXMT                                                
         DROP  R1                                                               
*                                                                               
DISMTHDX B     XIT                                                              
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY PAYCODES                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
DISPCDE  NTR1                                                                   
         LA    R1,SAVEPCDE                                                      
         CLI   RFLEL,0                                                          
         BE    DISPCDEX                                                         
         BAS   RE,GETPAY           RETURNS R6, # OF PAYCODS IN TABLE            
         LA    R2,RFLDATA          POINT TO DATA                                
         SR    RF,RF                                                            
         IC    RF,RFLLN            GET NUMBER OF CODES TO PROCESS               
         AHI   RF,-RFLLNQ                                                       
         LA    R4,APWORK                                                        
         MVC   APWORK,SPACES                                                    
         TM    RFLIND,RFLXCLD      EXCLUDE THESE PAYCODES ?                     
         BZ    DISPCD10            NO                                           
         MVI   0(R4),C'*'          EXCLUDE MARKER                               
         LA    R4,1(,R4)                                                        
*                                                                               
DISPCD10 LR    R0,R6               R6 = # PAYCODES IN TABLE                     
         LA    R3,PAYTAB           TABLE OF PAYCODES                            
*                                                                               
DISPCD12 CLC   5(L'PAYNUM,R3),0(R2)      MATCH ON PAY NUMBER                    
         BE    DISPCD20                                                         
         LA    R3,L'PAYNUM+L'PAYCODE(,R3)                                       
         BCT   R0,DISPCD12                                                      
         B     DISPCD28                  TRY NEXT, THIS ONE NOT FOUND           
*                                                                               
DISPCD20 MVC   0(L'PAYCODE,R4),0(R3)     MOVE IN PAYCODE                        
         LA    RE,L'PAYCODE                                                     
DISPCD22 CLI   0(R4),C' '                                                       
         BNH   DISPCD24            FOUND NEXT AVIALABLE SPOT                    
         LA    R4,1(,R4)                                                        
         BCT   RE,DISPCD22                                                      
*                                                                               
DISPCD24 MVC   0(1,R4),SCCOMMA     MOVE IN LANG. SOFT COMMA                     
         LA    R4,1(,R4)           NEXT OPEN SPOT                               
*                                                                               
DISPCD28 LA    R2,1(,R2)           NEXT NUMBER TO PROCESS                       
         BCT   RF,DISPCD10         PROCESS NEXT                                 
         BCTR  R4,0                                                             
         MVI   0(R4),C' '          REMOVE LAST COMMA                            
         MVC   CSTPCDE,APWORK                                                   
         OI    CSTPCDEH+6,FVOXMT   TRANSMIT                                     
DISPCDEX B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*  DISPLAY LOCATION STATUS                                            *         
***********************************************************************         
         SPACE 1                                                                
DISLOCS  NTR1                                                                   
         CLI   SAVELOCS,C' '       ANY  LOCATION STATUS ?                       
         BNH   DISLOCEX            NO,  EXIT                                    
*                                                                               
         L     R6,ACLOSTAB         ->   LOCATION STATUS TABLE                   
         MVC   BYTE,SAVELOCS                                                    
         OI    BYTE,X'40'          RESTORE BECAUSE OF EXCLUDE                   
*                                                                               
DISLOC10 DS    0H                                                               
         CLI   0(R6),EOT           END  OF TABLE ?                              
         BNE   *+6                 NO,  CONTINUE                                
         DC    H'00'               YES, ABEND                                   
         CLC   BYTE,0(R6)          FOUND MATCH ?                                
         BE    DISLOC20            YES, CONTINUE                                
*                                                                               
         LA    R6,L'CSTLOCS(,R6)   NEXT TABLE ENTRY                             
         B     DISLOC10                                                         
*                                                                               
DISLOC20 DS    0H                                                               
         MVC   LOSTATUS,1(R6)      MOVE LOCATION STATUS TO WORK AREA            
*                                  GET  LOCATION STATUS VALUE                   
         GOTO1 VDICTAT,APPARM,C'SU  ',('LLOSTATU',LOSTATUS),0                   
         LA    RF,CSTLOCS                                                       
         MVC   CSTLOCS,SPACES      CLEAR OUTPUT FIELD                           
         TM    SAVELOCS,X'40'                                                   
         BO    *+12                                                             
         LA    RF,CSTLOCS+1                                                     
         MVI   CSTLOCS,C'*'                                                     
*                                  INSERT THE LOCATION STATUS                   
         MVC   0(LLOSTATU,RF),LOSTATUS                                          
         OI    CSTLOCSH+6,FVOXMT   TRANSMIT                                     
*                                                                               
DISLOCEX DS    0H                  EXIT                                         
         B     XIT                                                              
         EJECT ,                                                                
         USING REPTABD,R1                                                       
EXPRPTY  MVC   CSTRPCDE,REPCODE                                                 
         MVC   CSTRPTYP,REPSTYPE                                                
         CLI   CSTRPCDE,ESCHIGHQ   TEST FOR ESCAPE SEQUENCE                     
         BNLR  RE                                                               
         DROP  R1                                                               
*                                                                               
         STM   RE,R1,SVREGS                                                     
         GOTO1 VDICTAT,APPARM,C'TU  ',('CSTRPLNQ',CSTRPCDE),0                   
         LM    RE,R1,SVREGS                                                     
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE BRANDOCEAN TIME SHEET CODES                               *         
***********************************************************************         
         SPACE 1                                                                
VALTST   NTR1                                                                   
         LA    R4,CSTBLOCK         ->   SCAN BLOCK     AREA                     
         SR    RF,RF                                                            
         IC    RF,NPARMS                                                        
*                                                                               
VALTST10 CLI   0(R4),1                                                          
         BH    ERRTSTA                                                          
*                                  CHECK VALID TIME STATUS                      
         USING RFLELD,R1                                                        
         LA    R1,APELEM           ->   ELEMENT AREA                            
         CLI   12(R4),C'R'         REJECT TIME                                  
         BE    VALTST20                                                         
         CLI   12(R4),C'U'         UPDATE TIME FROM COST TUP PROFILE            
         BNE   VALTST18                                                         
         CLC   DEFTIMST,SPACES     ANY DEFAULT TIME STATUS                      
         BNH   ERRTSTA             NO - ERROR                                   
         MVC   RFLDATA(L'DEFTIMST),DEFTIMST                                     
         LHI   RF,RFLLNQ+L'DEFTIMST                                             
         LA    RE,DEFTIMST+L'DEFTIMST-1                                         
VALTST14 CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   RF,VALTST14                                                      
         STC   RF,RFLLN                                                         
         B     VALTSTK                                                          
*                                                                               
VALTST18 CLI   RFLTYPE,RFLMISST    MISSING TIME STATUS?                         
         BE    VALTST19            DON'T TAKE FULLY APPROVED                    
         CLI   12(R4),C'F'         FULLY APPROVED TIME                          
         BE    VALTST20                                                         
VALTST19 CLI   12(R4),C'P'         PART APPROVED                                
         BNE   *+16                                                             
         TM    PTMSTAT,PTMSPART                                                 
         BZ    ERRTSTA                                                          
         B     VALTST20                                                         
         CLI   12(R4),C'S'         SUBMITTED TIME                               
         BNE   *+16                                                             
         TM    PTMSTAT,PTMSSUBT                                                 
         BZ    ERRTSTA                                                          
         B     VALTST20                                                         
         CLI   12(R4),C'I'         TIME IN PROCESS                              
         BNE   *+16                                                             
         TM    PTMSTAT,PTMSSAVT                                                 
         BZ    ERRTSTA                                                          
         B     VALTST20                                                         
         B     ERRTSTA             INVALID TIME STATUS                          
*                                                                               
VALTST20 LA    R4,32(,R4)                                                       
         BCT   RF,VALTST10                                                      
         B     VALTSTK                                                          
*                                                                               
ERRTSTA  MVC   FVMSGNO,=AL2(ACEINSTA)  INVALID TIME STATUS                      
*                                                                               
VALTSTK  CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         DROP  R1                                                               
***********************************************************************         
*  INVALID ERRORS TO DISPLAY ON TOP OF SCREEN                         *         
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
IVALTASK MVC   FVMSGNO,=AL2(ACEWKCDE)     INVALID TASK                          
         MVC   FVXTRA(20),12(R4)                                                
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
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALLIST MVC   FVMSGNO,=AL2(ACEIVLT)      INVALID LIST USED                     
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALIPUT MVC   FVMSGNO,=AL2(FVFNOTV)      INVALID INPUT                         
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALCNTR MVC   FVMSGNO,=AL2(ACEACTLV)     INVALID ACCOUNT OR LEVEL              
         MVC   FVXTRA(20),12(R4)          SHOW BAD CONTRA                       
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEXIT DS    0H                                                               
         XC    APCURSOR,APCURSOR          DON'T SET APPLICATION CURSOR          
         B     EXIT                                                             
         EJECT ,                                                                
SCRTXT   DS    0F                                                               
         DC    AL4(CSTRC02H-TWAD,1652)                                          
         DC    AL4(CSTCS08H-TWAD,1740)                                          
         DC    AL4(CSTCS11H-TWAD,1646)                                          
         DC    AL4(CSTCS12H-TWAD,1673)                                          
         DC    AL4(CSTCS13H-TWAD,1674)                                          
         DC    AL4(CSTCS14H-TWAD,1675)                                          
*&&UK*&& DC    AL4(CSTCS15H-TWAD,1676)                                          
         DC    X'FF'                                                            
*                                                                               
         SPACE 4                                                                
         LTORG                                                                  
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
CSTRPCDE DS    CL(L'REPCODE)                                                    
CSTRPTYP DS    CL(L'REPSTYPE)                                                   
CSTRPLNQ EQU   *-CSTRPCDE                                                       
CSTBLOCK DS    (MAXPARM)CL32                                                    
ONEXONLY DS    XL1                 ONE TIME ONLY FLAG                           
PARM_N   DS    XL1                 CURRENT PARAMETER WORKING ON                 
SAVEKEY1 DS    CL(L'RESKEY)                                                     
SVELEM   DS    CL(L'APELEM)        SAVE APELEM                                  
SAVEMTHD DS    CL1                                                              
SAVELOCS DS    CL1                                                              
SAVEPCDE DS    CL(RFLLNQ+20)       SAVE RFLEL FOR PAYCODE TYPE                  
PCDE#    DS    XL1                 NUMBER OF PAYCODES                           
LOSTATUS DS    CL(L'CSTLOCS-1)     LOCATION STATUS WORK AREA                    
LLOSTATU EQU   L'LOSTATUS          LENGTH OF LOCATION STATUS WORK AREA          
TEMPCHAR DS    CL1                                                              
TYPECDE  DS    CL4                                                              
CURUL    DS    CL2                                                              
LDGFLDH  DS    CL8                                                              
LDGFLD   DS    CL2                                                              
DUB      DS    D                                                                
WORK     DS    CL20                                                             
BYTE     DS    CL1                                                              
SECBIT   DS    XL1                                                              
DWCFLDH  DS    CL8                 DUMMY WORK CODE FIELD                        
TUPPROF  DS    CL1                 TUP SETTING IN CODE PROFILE RECORD           
DEFTIMST DS    CL7                 DEFAULT TIME STATUS FROM TUP PROFILE         
PTMSTAT  DS    XL1                 VALID PRODUCTION TIME STATUS                 
PTMSALL  EQU   X'F8'               ALL TIME                                     
PTMSSAVT EQU   X'80'               SAVED TIME                                   
PTMSSUBT EQU   X'40'               SUBMITTED TIME                               
PTMSREJT EQU   X'20'               REJECTED TIME                                
PTMSPART EQU   X'10'               PART APPROVED                                
PTMSFAPT EQU   X'08'               FULLY APPROVED TIME                          
DWCFLD   DS    CL(L'CSTWC+1)                                                    
PAYTAB   DS    256XL6                                                           
LWSX     DS    0C                                                               
         EJECT ,                                                                
*        ACSCRWRK                                                               
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR CST PROFILE DEFINITIONS                                  *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCREBD                                                       
         PRINT ON                                                               
*ACCAPBLOCK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCAPBLOCK                                                     
         PRINT ON                                                               
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047ACSCR11   06/24/20'                                      
         END                                                                    
