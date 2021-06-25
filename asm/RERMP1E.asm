*          DATA SET RERMP1E    AT LEVEL 185 AS OF 01/26/10                      
*PHASE T8101EC                                                                  
*INCLUDE REBKLSTB                                                               
*INCLUDE HEXOUT                                                                 
         TITLE 'T8101E - RERMP1E - INV REP TRANSFER'                            
*                                                                               
***********************************************************************         
*                                                                     *         
*- RERMP1E -- INV REP TRANSFER                                        *         
*                                                                     *         
*  MOD LOG:                                                           *         
*  --------                                                           *         
*                                                                     *         
*  NOV20/00 (FJD) ---  1.) BEGAN USING MODIFICATION LOG               *         
*                      2.) DROPPED 2ND BASE REGISTER, MOVED SOME      *         
*                          FIELDS OUT OF SYSSPARE (THEY WERE EXCEEDING*         
*                          ALLOCATED LENGTH),INTO NEW WORKING STORAGE.*         
*                      3.) INTRODUCED RELATIVE BRANCHING TO COMPENSATE*         
*                          FOR LOST 2ND BASE REGISTER.                *         
*  AUG02/01 (BU ) --- ADDED UT -> SZ TO TABLE                         *         
*                                                                     *         
*  AUG06/01 (BU ) --- ADDED J1 -> SZ TO TABLE                         *         
*                                                                     *         
*  OCT03/01 (BU ) --- ADDED SZ -> FB TO TABLE                         *         
*                                                                     *         
*  OCT31/01 (BU ) --- ADDED UT -> FB TO TABLE                         *         
*                                                                     *         
*  DEC06/01 (BU ) --- ADDED PV -> 3M AND 3M -> PV TO TABLE            *         
*                                                                     *         
*  MAY13/02 (BU ) --- ADDED FB -> KH                                  *         
*                                                                     *         
*  JUN06/02 (BU ) --- ADDED B1 -> NB                                  *         
*                                                                     *         
*  JUN27/02 (BU ) --- ADDED B1 -> NB                                  *         
*                                                                     *         
*  NOV30/04 (BU ) --- ADDED NB -> B1                                  *         
*                                                                     *         
*  JUL05/06 (BU ) --- ADDED NB -> MW                                  *         
*                                                                     *         
*  APR14/09 (KUI) --- SUPPORT NEW INVENTORY RECORD                    *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
         SPACE 2                                                                
T8101E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 OVERWRKQ,T8101E**,RR=RE,CLEAR=YES                                
*                                                                               
*                                                                               
         LR    R7,RC                                                            
         USING OVERWRKD,R7                                                      
*                                                                               
         L     RC,0(R1)            ESTABLISH GENCON WORKAREA                    
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA             ESTABLISH SCREEN                             
         USING CONHEADH-64,RA                                                   
*                                                                               
         L     R8,ASPOOLD          ESTABLISH SPOOL WORKAREA                     
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD            ESTABLISH SYSTEM WORKAREA                    
         USING SYSD,R9                                                          
*                                                                               
*        L     R7,ACOMFACS         ESTABLLISH COMMON SUBROUTINES                
*        USING COMFACSD,R7                                                      
*                                                                               
         ST    RE,RELO             SAVE RELOCATION FACTOR                       
*                                                                               
         L     RF,=V(BKLST)                                                     
         L     RE,RELO                                                          
         AR    RF,RE               RELOCATE ADDRESS                             
         ST    RF,VREBKLST                                                      
*                                                                               
         MVC   REPFILE,=C'REPFILE '    INIT FILENAME                            
*                                       (IN WORKING STORAGE)                    
         CLI   MODE,VALKEY                                                      
         BNE   RP2                                                              
         BAS   RE,VALREQ                                                        
         B     EXXMOD                                                           
         SPACE 1                                                                
RP2      CLI   MODE,PRINTREP                                                    
         BNE   EXXMOD                                                           
         BAS   RE,REPMODE                                                       
         B     EXXMOD                                                           
*                                                                               
EXXMOD   XIT1                                                                   
         EJECT                                                                  
*                                                                               
*-- EDIT THE REQUEST SCREEN                                                     
*                                                                               
VALREQ   NTR1                                                                   
         SPACE 1                                                                
*                                                                               
         LA    R2,TITREPH                                                       
         L     RE,=A(REPTAB)                                                    
         A     RE,RELO                                                          
*                                                                               
VREP10   CLI   0(RE),X'FF'                                                      
         BE    REPERR                                                           
         CLC   AGENCY,0(RE)        CURRENT AGENCY  = TABLES                     
         BNE   VREP20                                                           
         CLC   8(2,R2),2(RE)       CHANGE AGENCY = TABLES                       
         BE    VREP30                                                           
VREP20   LA    RE,4(RE)                                                         
         B     VREP10                                                           
*                                                                               
VREP30   MVC   NEWREP,2(RE)                                                     
                                                                                
         LA    R2,TITSTAH          STATION FIELD                                
         XC    WORK,WORK                                                        
         GOTO1 VALISTA             VALIDATE STATION                             
         CLI   WORK+40,X'40'                                                    
         BNH   *+10                                                             
         MVC   CSTAT+4(1),WORK+40                                               
*                                                                               
*        VALIDATE START DATE                                                    
*                                                                               
         LA    R2,TITSDTH          POINT TO START DATE                          
         XC    STRTOPT,STRTOPT     INIT START DATE OPTIONS                      
         XC    STRTOPTC,STRTOPTC   INIT START DATE OPTIONS                      
*                                                                               
         CLI   5(R2),0             OKAY IF NO START DATE ENTERED                
         BE    VREND                                                            
*                                                                               
         CLC   8(3,R2),=C'ALL'     OKAY IF 'ALL' ENTERED                        
         BE    VREND                                                            
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK   VALIDATE ENTERED DATE               
*                                                                               
         CLI   DMCB+3,0                                                         
         BNE   VRSTRT1                                                          
         B     DATERR                                                           
*                                                                               
VRSTRT1  GOTO1 DATCON,DMCB,(0,WORK),(3,STRTOPT)  SAVE START DATE                
         GOTO1 DATCON,DMCB,(0,WORK),(2,STRTOPTC) SAVE START DATE                
*                                                                               
*        VALIDATE END DATE                                                      
*                                                                               
VREND    LA    R2,TITEDTH          POINT TO END DATE                            
         MVC   ENDOPT,=X'FFFFFF'   INIT INTERNAL END DATE                       
*                                                                               
         CLI   5(R2),0             OKAY IF NO END DATE ENTERED                  
         BE    VREND5                                                           
*                                                                               
         CLC   8(3,R2),=C'ALL'     IKAY IF 'ALL' ENTERED                        
         BE    VREND5                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK  VALIDATE ENTERED DATE                
         CLI   DMCB+3,0                                                         
         BNE   VREND1                                                           
         B     DATERR                                                           
*                                                                               
VREND1   GOTO1 DATCON,DMCB,(0,WORK),(3,ENDOPT) SAVE END DATE                    
*                                                                               
VREND5   CLC   STRTOPT,ENDOPT      CHECK START LESS THEN END                    
         BH    DATERR                                                           
*                                                                               
*  VALIDATE INVENTORY NUMBER                                                    
*                                                                               
         XC    INVSTART,INVSTART                                                
         XC    INVEND,INVEND                                                    
         LA    R2,TITINVH          INVENTORY NUMBER                             
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         LA    R4,BLOCK                                                         
*                                                                               
* SET UP INVENTORY START                                                        
         CLI   0(R4),0                                                          
         BE    VRTXT1                                                           
         ZIC   RE,0(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   INVSTART(0),12(R4)                                               
         OC    INVSTART,SPACES                                                  
*                                                                               
* SET UP INVENTORY END                                                          
         CLI   1(R4),0                                                          
         BNE   *+14                                                             
         MVC   INVEND,INVSTART                                                  
         B     VRTXT1                                                           
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   INVEND(0),22(R4)                                                 
         OC    INVEND,SPACES                                                    
         CLC   INVSTART,INVEND                                                  
         BH    INVINPT                                                          
         B     VRTXT1                                                           
*                                                                               
VRTXT1   LA    R2,TITTXTH                                                       
         MVI   TEXTOPT,C'Y'                                                     
         CLI   5(R2),0                                                          
         BE    ESTBK1                                                           
         CLI   8(R2),C'Y'                                                       
         BE    ESTBK1                                                           
         MVI   TEXTOPT,C'N'                                                     
         CLI   8(R2),C'N'                                                       
         BNE   INVINPT                                                          
*                                                                               
ESTBK1   LA    R2,TITEBKH                                                       
         MVI   ESTBOPT,C'Y'                                                     
         CLI   5(R2),0                                                          
*!!      BE    VREX                                                             
         BE    INCBK1                                                           
*                                                                               
         CLI   8(R2),C'Y'                                                       
*!!      BE    VREX                                                             
         BE    INCBK1                                                           
*                                                                               
         MVI   ESTBOPT,C'N'                                                     
         CLI   8(R2),C'N'                                                       
         BNE   INVINPT                                                          
*                                                                               
INCBK1   DS    0H                                                               
         NI    BOOKFLG,X'FF'-INCBK                                              
         NI    BOOKFLG,X'FF'-EXCBK                                              
*                                                                               
         LA    R2,TITINCH                                                       
         CLI   5(R2),0             ANY INCLUDE BOOKS?                           
         BE    EXCBK1              NO                                           
         CLI   TITEXCH+5,0         ANY EXCLUDE BOOKS?                           
         BNE   INEXERR             CAN'T HAVE BOTH                              
         OI    BOOKFLG,INCBK                                                    
         BAS   RE,BLDBKTAB         BUILD TABLE FOR INCLUDE BOOKS                
         B     VREX                                                             
*                                                                               
EXCBK1   LA    R2,TITEXCH                                                       
         CLI   5(R2),0             ANY EXCLUDE BOOKS?                           
         BE    VREX                                                             
         OI    BOOKFLG,EXCBK                                                    
         BAS   RE,BLDBKTAB         BUILD TABLE FOR EXCLUDE BOOKS                
*                                                                               
VREX     B     EXXMOD                                                           
*                                                                               
DATERR   MVC   RERROR,=AL2(INVDATE) ERROR - INVALID DATE                        
         B     ERREXIT                                                          
*                                                                               
REPERR   MVC   RERROR,=AL2(INVALID) ERROR - INVALID REP                         
         B     ERREXIT                                                          
*                                                                               
MISSERR  MVC   RERROR,=AL2(MISSING) ERROR - MISSING INPUT                       
         B     ERREXIT                                                          
*                                                                               
INVINPT  MVC   RERROR,=AL2(INVALID) ERROR - INVALID INPUT                       
         B     ERREXIT                                                          
*                                                                               
INEXERR  MVC   RERROR,=AL2(INCOREX) ERROR - INCLUDE OR EXCLUDE BOOKS            
         B     ERREXIT                                                          
*                                                                               
INCOREX  EQU   820                 EITHER INCLUDE OR EXCLUDE BOOKS              
*                                                                               
ERREXIT  DS    0H                                                               
         GOTO1 MYERROR                                                          
*                                                                               
BLDBKTAB NTR1                                                                   
         LA    R2,TITINCH                                                       
         LA    R3,INCBKTAB         INCLUDE BOOK TABLE                           
*                                                                               
         TM    BOOKFLG,EXCBK       EXCLUDE BOOK?                                
         BZ    BLDBK10             YES                                          
         LA    R2,TITEXCH                                                       
         LA    R3,EXCBKTAB         EXCLUDE BOOK TABLE                           
*                                                                               
BLDBK10  DS    0H                                                               
         XC    TRBKLIST,TRBKLIST                                                
         GOTO1 VREBKLST,DMCB,(R2),(C'B',TRBKLIST),BOOKVAL,SCANNER,     X        
               ACOMFACS                                                         
         CLI   DMCB,0                                                           
         BE    INVINPT                                                          
*                                                                               
         LA    R5,TRBKLIST                                                      
*                                                                               
BLDBK20  DS    0H                                                               
         CLI   0(R5),0             ANY MORE BOOKS TO PROCESS                    
         BE    BLDBK40                                                          
*                                                                               
         MVI   0(R3),C'N'                                                       
         MVC   2(1,R3),0(R5)                                                    
         MVC   3(1,R3),3(R5)                                                    
         MVC   4(2,R3),1(R5)       MOVE BOOK INTO TABLE                         
*                                                                               
         LA    R3,INCBKTLN(R3)     NEXT ENTRY IN BK TABLE                       
         LA    R5,8(R5)            NEXT ENTRY IN INPUT BOOK LIST                
         B     BLDBK20                                                          
*                                                                               
BLDBK40  DS    0H                                                               
         MVI   0(R3),X'FF'                                                      
*                                                                               
BLDBKX   DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
*                                                                               
*-- EDIT THE REQUEST SCREEN                                                     
*                                                                               
REPMODE  NTR1                                                                   
*                                                                               
         L     R5,AIO1                                                          
         USING REINVREC,R5                                                      
*                                                                               
         XC    LASTINV#,LASTINV#        INIT LAST INVENTORY ITEM#               
*                                                                               
*  DO A GETFACT CALL TO GET THE ADDRESS OF THE UTL                              
*                                                                               
         GOTO1 GETFACT,DMCB,(0,0)                                               
         L     R4,DMCB                                                          
         USING FACTSD,R4                                                        
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   AUTL,FAAUTL                                                      
*                                                                               
*  READ THE CONTROL FILE RECORD TO GET THE SE NUMBER                            
*  FOR THE RECEIVING AGENCY.                                                    
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'                                            
         IC    R0,USEIO                                                         
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CT5KEY,R2                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,NEWREP    RECEIVING AGENCY (2 CHAR)                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         STC   R0,USEIO                                                         
         XC    FILENAME,FILENAME                                                
         L     R2,AIO                                                           
         LA    R2,CT5DATA                                                       
         SR    R0,R0                                                            
         DROP  R2                                                               
*                                                                               
RPM030   CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'21'         SYSTEM ELEMENT?                              
         BNE   *+12                 NO                                          
         CLI   2(R2),X'08'         REP SYSTEM?                                  
         BE    *+14                 YES                                         
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     RPM030                                                           
*                                                                               
         ICM   RE,15,AUTL                                                       
         MVC   OLDSE,4(RE)         SAVE ORIGINAL SE NUMBER                      
         MVC   NEWSE,3(R2)         SAVE NEW SE NUMBER                           
         MVC   4(1,RE),3(R2)       MOVE REP SE NUMBER                           
*                                                                               
*  OPEN THE FILES FOR THE RECEIVING AGENCY                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',FLIST,AIO                        
*                                                                               
*  MAIN LOGIC FLOW                                                              
*                                                                               
RPM100   ICM   RE,15,AUTL                                                       
         MVC   4(1,RE),OLDSE       SET UP TO OLD SE NUMBER                      
         BAS   RE,GTINV            READ INVENTORY RECORD                        
         CLI   KEY,X'FF'                                                        
         BE    RPMEX               END OF PROCESSING                            
*                                                                               
*  ADJUST FOR RECEIVING FILE                                                    
*                                                                               
         ICM   RE,15,AUTL                                                       
         MVC   4(1,RE),NEWSE       SET UP TO NEW SE NUMBER (REC AGENCY)         
*                                                                               
*  ENSURE ALL RECEIVING FILE ITEMS OF THIS # ARE DELETED!                       
*                       (FJD 11/00)                                             
HKEYD    USING RINVKEY,HOLDKEY                                                  
         CLC   LASTINV#,HKEYD.RINVKINV (HAVE WE DONE THIS INV#?)                
         BE    RPM150              YES, SKIP DELETION ROUTINE                   
         BAS   RE,DELINVS          NO, DELETE ALL ITEMS WITH THIS #             
         MVC   LASTINV#,HKEYD.RINVKINV AND SET LASTINV# TRANSFERRED             
*                                                                               
*  CHECK IF WE ARE ADDING NEW RECS OR CHANGEING EXISTING RECS                   
*                                                                               
RPM150   MVC   KEY,HOLDKEY                                                      
INVKEYD  USING RINVKEY,KEY                                                      
         MVC   INVKEYD.RINVKREP,NEWREP                                          
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     INV HEADER RECORD                            
         BE    RPM200                                                           
*                                                                               
*  RECORDS DON'T EXIST CHANGE RECORDS                                           
*                                                                               
         BAS   RE,ADDINVS          ADD NEW INVENTORY                            
         B     RPM100                                                           
*                                                                               
*  RECORDS EXIST DELETE ALL OLD RECORDS, RECREATE NEW RECORDS                   
*                                                                               
RPM200   BAS   RE,PUTINVS          WRITE NEW INVENTORY                          
         B     RPM100                                                           
*                                                                               
RPMEX    ICM   RE,15,AUTL                                                       
         MVC   4(1,RE),OLDSE       SET UP TO OLD SE NUMBER                      
         B     EXXMOD                                                           
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
FLIST    DC    CL8'UREPFILE'                                                    
         DC    CL8'UREPDIR '                                                    
         DC    CL8'UREPRCV '                                                    
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
         EJECT                                                                  
**********************************************************************          
*                                                                    *          
*  GTINV -- READ AND FILTER INVENTORY RECORDS                        *          
*                                                                    *          
**********************************************************************          
                                                                                
GTINV    NTR1                                                                   
         MVI   KEYLEN,16           DEFAULT LENGTH OF KEY CHECK                  
*                                                                               
         LA    R4,KEY                                                           
         USING REINVREC,R4                                                      
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   NEXTKEY,RINVKTYQ    FIRST TIME                                   
         BNE   *+14                NO GET NEXT RECORD                           
         MVC   KEY(27),NEXTKEY                                                  
         B     GTINV50                                                          
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         MVI   RINVKTYP,RINVKTYQ                                                
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,CSTAT                                                   
         MVC   RINVKINV,INVSTART                                                
*                                  MOVE DATE IN                                 
         MVC   RINVKSTD,STRTOPT                                                 
         MVC   COMPKEY,KEY                                                      
GTINV50  GOTO1 HIGH                                                             
         CLC   KEY(RINVKINV-RINVKEY),COMPKEY                                    
         BNE   GTINV70                                                          
         OC    INVEND,INVEND     WAS INV # SPECIFIED                            
         BZ    GTINV100                                                         
         CLC   RINVKINV,INVEND   CHECK IF INV # WITHIN RANGE                    
         BNH   GTINV100                                                         
GTINV70  MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
*                                                                               
GTINV100 CLI   RINVKRTP,C'M'       IS RECORD A MARKET FACT                      
         BE    GTINV500                                                         
         CLI   RINVKRTP,C'S'       IS RECORD A STATION FACT                     
         BE    GTINV500                                                         
         CLI   RINVKRTP,0          IS RECORD A HEADER                           
         BE    GTINV300                                                         
         B     GTINV350            SKIP TO NEXT RECORDS                         
*                                                                               
*  FILTER THE HEADER AGAINST THE DATE RANGE                                     
*                                                                               
GTINV300 CLC   RINVKSTD,ENDOPT     IS INV START > REQ END                       
         BH    GTINV400            END OF PROCESSING                            
*                                                                               
         L     R4,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         CLC   RINVKSTD,STRTOPT    IS INV START >= REQ START                    
         BNL   GTINV500            RECORD CAN BE PROCESSED                      
*                                                                               
         CLC   RINVPEFF+2,STRTOPTC IS INV END >= REQ START                      
         BNL   GTINV500            RECORD CAN BE PROCESSED                      
*                                                                               
*  CURRENT HEADERS DATE OUT OF RANGE SKIP READ TO NEXT DATE                     
*                                                                               
GTINV350 LA    R4,KEY                                                           
         ICM   RE,7,RINVKSTD                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,7,RINVKSTD                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(RINVKINV-RINVKEY),COMPKEY                                    
         BNE   GTINV380                                                         
         OC    INVEND,INVEND     WAS INV # SPECIFIED                            
         BZ    GTINV400                                                         
         CLC   RINVKINV,INVEND   CHECK IF INV # WITHIN RANGE                    
         BNH   GTINV400                                                         
GTINV380 MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
GTINV400 CLI   RINVKRTP,0                                                       
         BE    GTINV300                                                         
         B     GTINV350            SKIP TO NEXT DATE                            
*        DC    H'0'                                                             
*                                                                               
*  CURRENT HEADER DOES NOT QUALIFY EXIT                                         
*                                                                               
GTINV450 MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
*                                                                               
*  VALID HEADER SAVE DISK ADDRESSES TILL NEXT HEADER                            
*                                                                               
GTINV500 MVC   HOLDKEY,KEY                                                      
         MVC   DISKADDR(5),KEY+27                                               
         MVI   DISKADDR+5,X'FF'                                                 
*                                                                               
*        GOTO1 =V(PRNTBL),DMCB,=C'KEY',KEY,C'DUMP',30,=C'1D'                    
*                                                                               
         LA    R5,DISKADDR+5       END OF LOOKUP                                
         LA    R6,199                                                           
GTINV550 GOTO1 SEQ                                                              
         CLC   KEY(RINVKSPR-RINVKEY),KEYSAVE                                    
         BNE   GTINV800                                                         
*        GOTO1 =V(PRNTBL),DMCB,=C'BEFED',KEY,C'DUMP',30,=C'1D'                  
*                                                                               
         CLI   RINVKRTP,X'FF'      IS THIS A TEXT RECORD?                       
         BNE   GTINV580                                                         
*                                                                               
         CLI   TEXTOPT,C'N'        ADD TEXT RECORD?                             
         BNE   GTINV600            YES                                          
         B     GTINV550                                                         
*                                                                               
GTINV580 DS    0H                                                               
         CLI   RINVKRTP,C'M'       MARKET FACT?                                 
         BE    GTINV600                                                         
         CLI   RINVKRTP,C'S'       STATION FACT?                                
         BE    GTINV600                                                         
         CLI   RINVKRTP,C'Z'       RATE RECORD?                                 
         BE    GTINV600                                                         
*                                                                               
INVKEYD  USING RINVKEY,KEY                                                      
         CLI   INVKEYD.RINVKQLF,X'60'  NSI/ESTIMATE?                            
         BNE   GTINV582                                                         
         CLI   INVKEYD.RINVKBTP,0                                               
         BNE   GTINV582                                                         
*                                                                               
GTINV581 DS    0H                                                               
         CLI   ESTBOPT,C'N'        INCLUDE ESTIMATED BOOKS ?                    
         BE    GTINV550            NO                                           
         B     GTINV600                                                         
*                                                                               
GTINV582 DS    0H                                                               
         LA    RF,INCBKTAB         INCLUDE BOOK TABLE                           
         TM    BOOKFLG,INCBK       INCLUDE BOOKS?                               
         BO    GTINV585            YES                                          
*                                                                               
         TM    BOOKFLG,EXCBK       EXCLUDE BOOKS?                               
         BZ    GTINV600            DO ALL BOOKS                                 
*                                                                               
         LA    RF,EXCBKTAB         EXCLUDE BOOK TABLE                           
         B     GTINV590            YES                                          
*                                                                               
GTINV585 DS    0H                                                               
         CLI   0(RF),X'FF'         ANY MATCH FOUND?                             
         BE    GTINV550            NO - EXCLUDE THIS BOOK                       
*                                                                               
*                                  SAME SRC AND BOOK?                           
         CLC   RINVKRTP(RINVLEN-RINVKRTP),0(RF)                                 
         BE    GTINV600            YES - INCLUDE THIS BOOK                      
         LA    RF,INCBKTLN(RF)     CHECK NEXT ENTRY IN TABLE                    
         B     GTINV585                                                         
*                                                                               
GTINV590 DS    0H                                                               
         CLI   0(RF),X'FF'         ANY MATCH FOUND?                             
         BE    GTINV600            NO - INCLUDE THIS BOOK                       
*                                                                               
*                                  SAME RTG/QLF/BOOK?                           
         CLC   RINVKRSR,0(RF)                                                   
         BNE   GTINV595                                                         
*                                                                               
* SKIP CHECKING DATA SOURCE                                                     
*                                                                               
         CLC   RINVKQLF(RINVLEN-RINVKQLF),2(RF)                                 
         BE    GTINV550            YES - EXCLUDE THIS BOOK                      
*                                                                               
GTINV595 DS    0H                                                               
         LA    RF,INCBKTLN(RF)     CHECK NEXT ENTRY IN TABLE                    
         B     GTINV590                                                         
*                                                                               
GTINV600 MVC   0(5,R5),KEY+27                                                   
         MVI   5(R5),X'FF'                                                      
*                                                                               
*        GOTO1 =V(PRNTBL),DMCB,=C'AFTED',KEY,C'DUMP',30,=C'1D'                  
         LA    R5,5(R5)                                                         
         BCT   R6,GTINV550                                                      
         DC    H'0'                                                             
*                                                                               
GTINV800 CLI   KEY,RINVKTYQ        READ ALL THE INVENTORY                       
         BNE   GTINV450                                                         
         MVC   NEXTKEY,KEY                                                      
NKEYD    USING RINVKEY,NEXTKEY                                                  
         XC    NKEYD.RINVKRTP(RINVLEN-RINVKRTP),NKEYD.RINVKRTP                  
         B     EXXMOD                                                           
KEYCHK   CLC   KEY(0),COMPKEY                                                   
         DROP  R4                                                               
*                                                                               
*--ADD THE INVENTORY RECORDS                                                    
*                                                                               
ADDINVS  NTR1                                                                   
         LA    R3,DISKADDR                                                      
         L     R5,AIO1                                                          
         USING REINVREC,R5                                                      
*  CHECK IF MARKET OR STATION FACT RECORD                                       
         CLI   HOLDKEY+RINVKRTP-RINVKEY,C'M'                                    
         BE    ADIN050                                                          
         CLI   HOLDKEY+RINVKRTP-RINVKEY,C'S'                                    
         BE    ADIN050                                                          
*  FIRST ADD THE HEADER                                                         
         MVC   KEY+27(5),DISKADDR                                               
         ICM   RE,15,AUTL                                                       
         MVC   4(1,RE),OLDSE       SET UP TO OLD SE NUMBER (ORG AGENCY)         
         MVC   KEY+27(5),DISKADDR                                               
         GOTO1 GETREC                                                           
         ICM   RE,15,AUTL                                                       
         MVC   4(1,RE),NEWSE       SET UP TO NEW SE NUMBER (REC AGENCY)         
*                                                                               
         MVC   RINVKREP,NEWREP                                                  
         BAS   RE,CVTDAYDP                                                      
         BAS   RE,FLADD                                                         
         OC    BSVDA,BSVDA                                                      
         BNE   *+6                                                              
         DC    H'0'                IF RECORD EXISTS ERROR                       
*  ADD THE PASSIVE POINTERS                                                     
         GOTO1 INVPTR,DMCB,0(R5),WORK2                                          
         GOTO1 NWPT,DMCB,WORK2                                                  
         LA    R3,5(R3)            BUMP TO NEXT DISK ADDRESS                    
*                                                                               
*  ADD THE COMMENT AND TEXT RECORDS                                             
*                                                                               
ADIN050  LA    R4,200                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
ADIN100  CLI   0(R3),X'FF'                                                      
         BE    ADINEX                                                           
         ICM   RE,15,AUTL                                                       
         MVC   4(1,RE),OLDSE       SET UP TO OLD SE NUMBER (ORG AGENCY)         
         MVC   KEY+27(5),0(R3)                                                  
         GOTO1 GETREC                                                           
         ICM   RE,15,AUTL                                                       
         MVC   4(1,RE),NEWSE       SET UP TO NEW SE NUMBER (REC AGENCY)         
         MVC   RINVKREP,NEWREP                                                  
         BAS   RE,FLADD                                                         
         LA    R3,5(R3)                                                         
         BCT   R4,ADIN100                                                       
         DC    H'0'                TABLE IS BLOWN DUMP                          
*                                                                               
ADINEX   B     EXXMOD                                                           
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
* DELINVS -- DELETE ALL INVENTORY RECORDS OF A SPECIFIC INV# ON                 
*            RECEIVING FILE, AND ASSOCIATED PASSIVE POINTERS                    
*                                                                               
**********************************************************************          
DELINVS  NTR1                                                                   
                                                                                
         LA    R2,KEY                                                           
         USING RINVKEY,R2                                                       
                                                                                
         L     R4,AIO              AIO ADDRESS FOR INVPTR ROUTINE               
                                                                                
         XC    KEY,KEY             CLEAR KEY                                    
*                                  MOVE IN KEY FOR THIS INV #                   
         MVC   KEY(RINVKSTD-RINVKEY),HOLDKEY                                    
         MVC   RINVKREP,NEWREP     REPLACE REPCODE WITH RECEIVER'S              
                                                                                
*  START MAIN LOOP                                                              
                                                                                
DLI050   GOTO1 HIGH                READ HIGH                                    
*                                  CHECK UP THROUGH INV#                        
         CLC   KEY(RINVKSTD-RINVKEY),KEYSAVE                                    
         BNE   DELX                IF NO MATCH, EXIT                            
         MVC   KEYHOLD1,KEY         ELSE, REMEMBER THIS KEY                     
*                                                                               
*      WE DON'T DUMP AFTER NEXT TEST, BECAUSE SOME UNNATTACHED                  
*      RECORDS ARE KNOWN TO EXIST, AND WILL BE TAKEN CARE OF                    
*      WITH A FILEFIX IN THE FUTURE.  FOR NOW, JUST SKIP THEM                   
*                        (FJD, 11/00)                                           
*                                                                               
         CLI   RINVKRTP,0          IS IT A HEADER?                              
*        BE    *+6                           * YES, CONTINUE                    
*        DC    H'0'                          * NO,  DIE                         
                                                                                
         BNE   DELNXT               NO,  SHOULD BE,BUT JUST SKIP                
                                                                                
         GOTO1 GETREC               YES, READ THE OLD INV HEADER                
         LA    R3,200                                                           
*                                                                               
                                                                                
*      START SUB LOOP (DELETE SUBSIDIARY RECORD KEYS)                           
                                                                                
*                                                                               
         B     DLI110              BYPASS TEST ON 1ST ITERATION                 
*                                  ELSE, WE WILL SKIP RECORDS IF                
*                                  RECEIVER'S ITEM DOES NOT HAVE                
*                                  SAME DATE AS SENDER'S                        
                                                                                
DLI100   CLC   KEY(RINVKSPR-RINVKEY),KEYSAVE   MATCH THROUGH EFF DATE?          
         BNE   DLI200              NO, DELETE POINTERS FOR THIS ITEM            
DLI110   OI    KEY+27,X'80'        ELSE, DELETE KEY FOR THIS REC                
         BRAS  RE,MYDIRWRT                                                      
         GOTO1 SEQ                 GET NEXT RECORD                              
         BCT   R3,DLI100                                                        
         DC    H'0'                OVER 200 RECORDS CHANGED,TOO MANY            
                                                                                
*      END SUB LOOP                                                             
                                                                                
*  REMOVE THE PASSIVE POINTERS (R4 POINTS TO RECORD)                            
DLI200   GOTO1 INVPTR,DMCB,0(R4),WORK2                                          
         GOTO1 DELPT,DMCB,WORK2                                                 
*                                                                               
DELNXT   MVC   KEY,KEYHOLD1        REPLACE HEADER KEY                           
         ZICM  R5,RINVKSTD,3       TAKE KEY EFFECTIVE DATE                      
         AHI   R5,1                BUMP IT UP A NOTCH                           
         STCM  R5,7,RINVKSTD       PUT BACK IN KEY                              
                                                                                
*                                  CLEAR REST OF KEY                            
         XC    RINVKRTP(RINVLEN-RINVKRTP),RINVKRTP                              
                                                                                
         B     DLI050              REPEAT MAIN LOOP                             
                                                                                
*  END MAIN LOOP                                                                
                                                                                
         DROP  R2                                                               
*                                                                               
DELX     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                                                                               
* PUTINVS ROUTINE  -- ADD THE INVENTORY RECORDS                                 
*                                                                               
***********************************************************************         
PUTINVS  NTR1                                                                   
         MVC   AIO,AIO1                                                         
         L     R5,AIO1                                                          
         USING REINVREC,R5                                                      
         LA    R3,DISKADDR                                                      
*  CHECK IF MARKET OR STATION FACT RECORD                                       
         CLI   HOLDKEY+RINVKRTP-RINVKEY,C'M'                                    
         BE    PTIN050                                                          
         CLI   HOLDKEY+RINVKRTP-RINVKEY,C'S'                                    
         BE    PTIN050                                                          
*  FIRST PUT THE HEADER                                                         
         MVC   KEY+27(5),DISKADDR                                               
         ICM   RE,15,AUTL                                                       
         MVC   4(1,RE),OLDSE       SET UP TO OLD SE NUMBER (ORG AGENCY)         
         MVC   KEY+27(5),DISKADDR                                               
         GOTO1 GETREC                                                           
         ICM   RE,15,AUTL                                                       
         MVC   4(1,RE),NEWSE       SET UP TO NEW SE NUMBER (REC AGENCY)         
         MVC   RINVKREP,NEWREP                                                  
         BAS   RE,CVTDAYDP                                                      
         BAS   RE,FLADD                                                         
         OC    BSVDA,BSVDA                                                      
         BNZ   *+6                                                              
         DC    H'0'                IF RECORD DOESNT EXIST ERROR                 
*  ADD THE PASSIVE POINTERS                                                     
         GOTO1 INVPTR,DMCB,0(R5),WORK2                                          
         GOTO1 RSTPT,DMCB,WORK2                                                 
         LA    R3,5(R3)            GET NEXT DISK ADDRESS                        
*                                                                               
*  ADD THE COMMENT AND TEXT RECORDS                                             
*                                                                               
PTIN050  LA    R4,199                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
PTIN100  CLI   0(R3),X'FF'                                                      
         BE    PTINEX                                                           
*        GOTO1 =V(PRNTBL),DMCB,=C'PT1',(R3),C'DUMP',5,=C'1D'                    
         ICM   RE,15,AUTL                                                       
         MVC   4(1,RE),OLDSE       SET UP TO OLD SE NUMBER (ORG AGENCY)         
         MVC   KEY+27(5),0(R3)                                                  
*        GOTO1 =V(PRNTBL),DMCB,=C'PT2',(R3),C'DUMP',5,=C'1D'                    
         GOTO1 GETREC                                                           
*        GOTO1 =V(PRNTBL),DMCB,=C'PT3',(R3),C'DUMP',5,=C'1D'                    
         ICM   RE,15,AUTL                                                       
         MVC   4(1,RE),NEWSE       SET UP TO NEW SE NUMBER (REC AGENCY)         
         MVC   RINVKREP,NEWREP                                                  
         BAS   RE,FLADD                                                         
*        GOTO1 =V(PRNTBL),DMCB,=C'PT4',(R3),C'DUMP',5,=C'1D'                    
         LA    R3,5(R3)                                                         
         BCT   R4,PTIN100                                                       
         DC    H'0'                TABLE IS BLOWN DUMP                          
*                                                                               
PTINEX   B     EXXMOD                                                           
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE TO ADD PASSIVE POINTERS                                  
*                                                                               
         SPACE 1                                                                
*              PARAM 1   BYTES 1-3 A(LIST OF POINTERS)                          
         SPACE 1                                                                
NWPT     NTR1                                                                   
         L     R2,0(R1)                                                         
NWPT1    CLI   0(R2),0                                                          
         BE    EXXMOD              END OF LIST                                  
         MVC   KEY(27),0(R2)                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(27),KEY                                                  
         BE    NWPT3                                                            
         MVC   KEY(28),0(R2)                                                    
         MVC   KEY+28(4),BSVDA                                                  
         BRAS  RE,MYDIRADD         COMMENT                                      
         B     NWPT4                                                            
         SPACE 1                                                                
NWPT3    MVC   KEY(28),0(R2)                                                    
         MVC   KEY+28(4),BSVDA                                                  
         BRAS  RE,MYDIRWRT         COMMENT                                      
         SPACE 1                                                                
NWPT4    LA    R2,32(R2)                                                        
         B     NWPT1                                                            
         SPACE 2                                                                
*              ROUTINE TO DELETE POINTERS                                       
         SPACE 1                                                                
DELPT    NTR1                                                                   
         L     R2,0(R1)                                                         
DELPT1   CLI   0(R2),0                                                          
         BE    EXXMOD                                                           
         MVC   KEY(27),0(R2)                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DELPT4                                                           
         OI    KEY+27,X'80'                                                     
         BRAS  RE,MYDIRWRT         COMMENT                                      
         SPACE 1                                                                
DELPT4   LA    R2,32(R2)                                                        
         B     DELPT1                                                           
         EJECT                                                                  
         SPACE 2                                                                
*              ROUTINE TO RESTORE POINTERS                                      
         SPACE 1                                                                
RSTPT    NTR1                                                                   
         L     R2,0(R1)                                                         
RSTPT1   CLI   0(R2),0                                                          
         BE    EXXMOD                                                           
         MVC   KEY(27),0(R2)                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(27),KEY                                                  
         BNE   RSTPT4                                                           
         NI    KEY+27,X'7F'                                                     
         BRAS  RE,MYDIRWRT         COMMENT                                      
         SPACE 1                                                                
RSTPT4   LA    R2,32(R2)                                                        
         B     RSTPT1                                                           
         EJECT                                                                  
*              CREATE NEW PASSIVE POINTER                                       
         SPACE 1                                                                
*              PARAM 1   BYTES 1-3 A(INVENTORY RECORD)                          
*              PARAM 2   BYTES 1-3 A(200 BYTE OUTPUT AREA)                      
         SPACE 1                                                                
         USING RINVREC,R2                                                       
         USING RIDPKEY,R4                                                       
INVPTR   NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R4,4(R1)                                                         
         XC    0(200,R4),0(R4)                                                  
         LA    R6,6                                                             
         LA    R3,RINVDP                                                        
         SPACE 1                                                                
INVPTR1  MVI   RIDPKTYP,RIDPKTYQ                                                
         MVC   RIDPKREP,RINVKREP                                                
         MVC   RIDPKSTA,RINVKSTA                                                
         MVC   RIDPKDPT,0(R3)                                                   
         MVC   RIDPKINV,RINVKINV                                                
         MVC   RIDPKSTD,RINVKSTD                                                
         SPACE 1                                                                
*                                                                               
*  IF SELF ASSIGNED GET NEXT DAYPART                                            
*  ONLY COMPUTER GENERATED NUMBERS GET THE DAY,QTR HOUR                         
*  AND THE LENGTH FILLED IN.                                                    
*                                                                               
         TM    RINVSTAT,X'80'                                                   
         BO    INVPTR20            BIT ON SELF ASSIGNED                         
*                                                                               
         MVC   RIDPKDAY,RINVOINV+1   MOVE DAY CODE,                             
         MVC   RIDPKQTR,RINVOINV     QUARTER HOUR,                              
         MVC   RIDPKLEN,RINVOINV+2   AND PROGRAM LENGTH TO KEY                  
         SPACE                                                                  
INVPTR20 LA    R3,1(R3)            NEXT DAYPART CODE                            
         CLI   0(R3),X'40'                                                      
         BNH   INVPTX                                                           
         LA    R4,32(R4)                                                        
         BCT   R6,INVPTR1          DO NEXT POINTER                              
         SPACE 1                                                                
INVPTX   B     EXXMOD                                                           
         SPACE 1                                                                
         DROP  R2,R4                                                            
         SPACE 1                                                                
*  THESE DAYPARTS GET A DAY CODE, QUARTER HOUR, AND PROGRAM LENGTH              
DAYCOD   DC    C'MDKNPOUXYWZ',X'FF'                                             
         SPACE 1                                                                
*  THESE DAYPARTS GET EFFECTIVE DATE, QUARTER HOUR, AND PROGRAM LENGTH          
EFFDAT   DC    C'VSJ',X'FF'                                                     
         EJECT                                                                  
*  CHECK TO DYPCONV TABLE TO SEE IF DAYPART CONVERSION NEEDED                   
         SPACE 1                                                                
CVTDAYDP NTR1                                                                   
         L     R4,AIO1                                                          
         USING REINVREC,R4                                                      
*        GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',0(R4),C'DUMP',200,=C'1D'              
*                                                                               
         LA    R1,6                NUMBER OF DAYPARTS IN HEADER                 
         L     RE,=A(DYPCONV)                                                   
         A     RE,RELO                                                          
         LA    RF,RINVDP                                                        
*                                                                               
CVTDP100 CLI   0(RE),X'FF'         END OF TABLE                                 
         BE    CVTDP200                                                         
         CLC   AGENCY,0(RE)        OLD AGENCY MATCH                             
         BNE   CVTDP150                                                         
         CLC   NEWREP,2(RE)        NEW AGENCY MATCH                             
         BNE   CVTDP150                                                         
         CLC   0(1,RF),4(RE)       DAYPART MATCH                                
         BNE   CVTDP150                                                         
         MVC   0(1,RF),5(RE)       MOVE IN NEW DAYPART                          
         B     CVTDP200                                                         
*                                                                               
CVTDP150 LA    RE,6(RE)            NEXT TABLE ENTRY                             
         B     CVTDP100                                                         
*                                                                               
CVTDP200 L     RE,=A(DYPCONV)      RESET THE TABLE                              
         A     RE,RELO                                                          
         LA    RF,1(RF)            GO TO NEXT DAYPART                           
         BCT   R1,CVTDP100                                                      
*                                                                               
*        GOTO1 =V(PRNTBL),DMCB,=C'AFTER',0(R4),C'DUMP',200,=C'1D'               
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
*  THESE DAYPARTS ONLY GET INVENTORY NUMBER AND START DATE                      
*       ERATLF - THEY ARE THE FRINGE "SUB-DAYPARTS"                             
* (W-WEEKEND IS NOT TREATED AS FRINGE FOR PASSIVE POINTERS, BUT                 
*    IS GROUPED WITH FRINGE EVERYWHERE ELSE)                                    
         EJECT                                                                  
*                                                                               
*              ADD THE RECORD TO FILE                                           
*                                                                               
         SPACE 1                                                                
FLADD    NTR1                                                                   
         L     R6,AIO                                                           
         USING RINVAEL,R5                                                       
         USING REINVREC,R6                                                      
         MVC   KEY,RINVREC                                                      
         SPACE 1                                                                
         LA    R5,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   RINVACOD(2),=X'EF0C'                                             
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVAFST)                                 
         MVC   RINVALST,RINVAFST                                                
         MVI   RINVAWHY,C'A'                                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    FLPUT                                                            
         SPACE 1                                                                
         GOTO1 HELLO,DMCB,(C'D',REPFILE),(X'EF',AIO3),0                         
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO3),WORK,=C'ADD=CODE'             
*        GOTO1 =V(PRNTBL),DMCB,=C'ADD',AIO,C'DUMP',200,=C'1D'                   
         BAS   RE,MYFILADD         ADD THE RECORD   (COMMENT)                   
         MVC   BSVDA,KEY+28        SAVE DISK ADDRESS                            
         NI    DMINBTS,X'F7'       TURN OFF PASS DELETES                        
         B     EXXMOD                                                           
         SPACE 1                                                                
FLPUT    TM    KEY+27,X'80'                                                     
         BNO   FLP020                                                           
         MVI   KEY+27,0                                                         
         BRAS  RE,MYDIRWRT         UNDELETE THE POINTER  (COMMENT)              
FLP020   MVC   AIO,AIO3                                                         
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
*  GET X'EF' ELEMENT AND UPDATE THE CHANGE DATE                                 
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'EF',AIO),0                          
         CLI   12(R1),0                                                         
         BNE   FLP100              NO X'EF' ELEM BUILD ONE                      
         L     R5,12(R1)                                                        
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
         MVI   RINVAWHY,C'C'                                                    
         B     FLP200                                                           
         SPACE 1                                                                
FLP100   GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),WORK,=C'ADD=CODE'              
         SPACE 1                                                                
*        GOTO1 =V(PRNTBL),DMCB,=C'PUT',AIO,C'DUMP',200,=C'1D'                   
FLP200   BAS   RE,MYFILWRT         WRITE BACK THE NEW   (COMMENT)               
         NI    DMINBTS,X'F7'                                                    
         MVC   BSVDA,KEY+28                                                     
         B     EXXMOD                                                           
         DROP  R5,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* DATAMGR INTERFACE                                                             
***********************************************************************         
MYFILADD NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'REPFILE ',KEY+28,AIO,DMWORK           
         BAS   RE,DMCHECK                                                       
         MVC   BSVDA,KEY+28     SAVE DISK ADDRESS                               
         B     YES                                                              
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
MYFILWRT NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'REPFILE ',KEY+28,AIO,DMWORK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
MYDIRWRT NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'REPDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
MYDIRADD NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REPDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
*  NO LITERALS MAY BE USED BELOW, AS BASE REGISTER WILL DIFFER                  
*  DEPENDING ON CALLING ROUTINE.  ONLY RELATIVE BRANCHING ALLOWED               
* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!              
                                                                                
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         JM    NO                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         J     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1  REGS=(R0,R1)                                                     
                                                                                
* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!              
*                                                                               
         EJECT                                                                  
******************************************************************              
         SPACE 1                                                                
REPTAB   DC    CL4'BLPV'           BL --> PV                                    
         DC    CL4'PVBL'           PV --> BL                                    
         DC    CL4'MRAM'           MR --> AM                                    
         DC    CL4'MRCQ'           MR --> CQ                                    
         DC    CL4'MRNK'           MR --> NK                                    
         DC    CL4'AMMR'           AM --> MR                                    
         DC    CL4'AMCQ'           AM --> CQ                                    
         DC    CL4'AMNK'           AM --> NK                                    
         DC    CL4'CQMR'           CQ --> MR                                    
         DC    CL4'CQAM'           CQ --> AM                                    
         DC    CL4'CQNK'           CQ --> NK                                    
         DC    CL4'NKMR'           NK --> MR                                    
         DC    CL4'NKAM'           NK --> AM                                    
         DC    CL4'NKCQ'           NK --> CQ                                    
         DC    CL4'PVFN'           PV --> FN                                    
         DC    CL4'AMSZ'           AM --> SZ                                    
         DC    CL4'SZAM'           SZ --> AM                                    
         DC    CL4'CQSZ'           CQ --> SZ                                    
         DC    CL4'SZCQ'           SZ --> CQ                                    
         DC    CL4'B3NS'           B3 --> NS  (TEST ENTRY)                      
         DC    CL4'KHSJ'           KH --> SJ  (TEST ENTRY)                      
         DC    CL4'SJKH'           SJ --> KH  (TEST ENTRY)                      
         DC    CL4'KHTV'           KH --> TV  (TEST ENTRY)                      
         DC    CL4'KHB3'           KH --> TV  (TEST ENTRY)                      
         DC    CL4'B3KH'           B3 --> KH  (TEST ENTRY)                      
         DC    CL4'B3LS'           B3 --> LS  (TEST ENTRY)                      
         DC    CL4'B3SJ'           B3 --> SJ  (TEST ENTRY)                      
         DC    CL4'SJB3'           SJ --> B3  (TEST ENTRY)                      
         DC    CL4'9UB5'           9U --> B5  (TEST ENTRY)                      
         DC    CL4'PVP9'           PV --> P9                                    
         DC    CL4'BLAM'           BL --> AM                                    
         DC    CL4'PVAM'           PV --> AM                                    
         DC    CL4'PQNB'           PQ --> NB                                    
         DC    CL4'AMSJ'           AM --> SJ                                    
         DC    CL4'PQPV'           PQ --> PV  (PAXSON - PETRY)                  
         DC    CL4'PQAM'           PQ --> AM                                    
         DC    CL4'PQSZ'           PQ --> SZ                                    
         DC    CL4'PQCQ'           PQ --> CQ                                    
         DC    CL4'FNBL'           FN --> BL                                    
         DC    CL4'BLP9'           BL --> P9                                    
         DC    CL4'FNP9'           FN --> P9                                    
         DC    CL4'PQBL'           PQ --> BL  (PAXSON - BLAIR)                  
         DC    CL4'PQUT'           PQ --> UT  (PAXSON - UTS)                    
         DC    CL4'UTSZ'           UT --> SZ  (UTS - SELTEL)                    
         DC    CL4'J1SZ'           J1 --> SZ  (KTVX- SELTEL)                    
         DC    CL4'SZFB'           SZ --> FB  (SELTEL-FSS  )                    
         DC    CL4'UTFB'           UT --> FB  (UTS   -FSS  )                    
         DC    CL4'PV3M'           PV --> 3M  (PETRY -BLTRN)                    
         DC    CL4'3MPV'           3M --> PV  (BLTRN -PETRY)                    
         DC    CL4'FBSJ'           FB --> SJ  (FSSNY -SJR  )                    
         DC    CL4'FBKH'           FB --> KH  (FSSNY -REPDEMO)                  
         DC    CL4'B1KH'           B1 --> KH  (TELNY -REPDEMO)                  
         DC    CL4'B1NB'           B1 --> NB  (TELNY -NBC    )                  
         DC    CL4'NBMW'           NB --> MW  (NBC   -MGBG   )                  
         DC    CL4'NBB1'           NB --> B1  (NBC   -TELNY  )                  
         DC    CL4'CQSJ'           CQ --> SJ  (KCONY -SJR    )                  
         DC    CL4'AMSJ'           AM --> SJ  (ETVNY -SJR    )                  
         DC    CL4'UVSJ'           UV --> SJ  (UNNY  -SJR    )                  
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
         SPACE 1                                                                
DYPCONV  DC    CL6'AMSZRG'         AM --> SZ DP R > DP G                        
         DC    CL6'AMSZSJ'         AM --> SZ DP S > DP J                        
         DC    CL6'AMSZYR'         AM --> SZ DP Y > DP R                        
         DC    CL6'AMSZCS'         AM --> SZ DP C > DP S                        
         DC    CL6'AMSZXZ'         AM --> SZ DP X > DP Z                        
         DC    CL6'CQSZRG'         CQ --> SZ DP R > DP G                        
         DC    CL6'CQSZSJ'         CQ --> SZ DP S > DP J                        
         DC    CL6'CQSZYR'         CQ --> SZ DP Y > DP R                        
         DC    CL6'CQSZCS'         CQ --> SZ DP C > DP S                        
         DC    CL6'CQSZXZ'         CQ --> SZ DP X > DP Z                        
         DC    X'FF'                                                            
         EJECT                                                                  
******************************************************************              
*                                                                               
*     OVERLAY WORKING STORAGE                                                   
*                                                                               
******************************************************************              
OVERWRKD DSECT                                                                  
*                                                                               
RELO     DS    A                                                                
REPFILE  DS    CL8                 'REPFILE '                                   
*                                                                               
INCBKTAB DS    XL25                TABLE FOR INCLUDE BOOKS (MAX 4)              
INCBKTLN EQU   6                                                                
*                                   4 BYTE RTG/DSRC/QLF/BKT                     
*                                   2 BYTES BOOK                                
*                                                                               
EXCBKTAB DS    XL25                TABLE FOR EXCLUDE BOOKS (MAX 4)              
*                                   4 BYTE RTG/DSRC/QLF/BKT                     
*                                   2 BYTES BOOK                                
*                                                                               
*                                                                               
VREBKLST DS    A                   ADDRESS OF TRANSFER MODULE                   
LASTINV# DS    CL4                 LAST INVENTORY ITEM'S #                      
BOOKFLG  DS    XL1                 BOOK FLAGS                                   
INCBK    EQU   X'01'               THERE ARE INCLUDE BOOKS                      
EXCBK    EQU   X'02'               THERE ARE EXCLUDE BOOKS                      
TRBKLIST DS    XL64                BOOK ENTRIES BUILT BY REBKLST                
IBLK     DS    CL5                 INPUT BLOCK FOR GETKSRC                      
OBLK     DS    CL5                 OUTPUT BLOCK FOR GETKSRC                     
WORK2    DS    CL200               SECOND WORK AREA                             
KEYHOLD1 DS    CL27                KEY HOLDER FOR DELETION ROUTINE              
OVERWRKQ EQU   *-OVERWRKD          LENGTH OFWORKING STORAGE                     
*                                                                               
         EJECT                                                                  
         SPACE 2                                                                
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RERMPFFD                                                                      
* DDGENTWA                                                                      
* RERMPWTWA                                                                     
* RERMPD7D                                                                      
* REGENMKT                                                                      
* REGENREP(A)                                                                   
* RERMPWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPEED                                                       
         EJECT                                                                  
       ++INCLUDE RERMPWTWA                                                      
         EJECT                                                                  
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
       ++INCLUDE REGENRDP                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
       ++INCLUDE RERMPWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*              WORK AREA                                                        
*                                                                               
TAPECNT  DS    F                   NUMBER OF RECORDS                            
AUTL     DS    F                   ADDRESS OF UTL                               
*                                                                               
STRTOPT  DS    CL3                 START DATE                                   
STRTOPTC DS    CL2                 START DATE COMPRESSED                        
ENDOPT   DS    CL3                 END DATE                                     
NEWREP   DS    CL2                 NEW REP CODE                                 
INVSTART DS    CL4                 INVENTORY START RANGE                        
INVEND   DS    CL4                 INVENTORY END RANGE                          
TEXTOPT  DS    CL1                 INCLUDE TEXT (Y OR N)                        
ESTBOPT  DS    CL1                 INCLUDE ESTIMATED BOOKS (Y OR N)             
*                                                                               
HOLDKEY  DS    CL27                                                             
NEXTKEY  DS    CL27                                                             
COMPKEY  DS    CL27                                                             
*                                                                               
DISKADDR DS    CL1001              SAVE UP TO 200 DISK ADDRESSES                
RECLEN   DS    H                   RECORD LENGTH                                
*                                                                               
BSVDA    DS    CL4                 SAVED DISK ADDRESS                           
*                                                                               
KEYLEN   DS    CL1                 LENGTH OF KEY COMPARE                        
NEWSE    DS    CL1                 RECEIVING AGENCY SE NUMBER                   
OLDSE    DS    CL1                 GIVING AGENCY SE NUMBER                      
MYSSPREL EQU   *-SYSSPARE                                                       
SYSPREMN EQU   L'SYSSPARE-MYSSPREL  AMOUNT LEFT IN SYSSPARE                     
         DS    0XL(SYSPREMN+1)      CHECK AGAINST SYSSPARE LIMIT                
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         SPACE 5                                                                
* SAVED STORAGE IN TWA0 FOR NESFM00 STARTS HERE                                 
T810FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T810FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'185RERMP1E   01/26/10'                                      
         END                                                                    
