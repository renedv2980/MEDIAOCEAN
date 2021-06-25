*          DATA SET RERMP1EX   AT LEVEL 151 AS OF 05/01/02                      
*PHASE T8101EB,*                                                                
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
*                                                                     *         
***********************************************************************         
*                                                                               
         SPACE 2                                                                
T8101E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8101E**,RR=RE                                                 
*                                                                               
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING T8101E,RB,R7                                                     
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
         LA    RE,REPTAB                                                        
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
         GOTO1 VREBKLST,DMCB,(R2),(C'B',TRBKLIST),BOOKVAL,SCANNER               
         CLI   DMCB,0                                                           
         BE    INVINPT                                                          
*                                                                               
         LA    R5,TRBKLIST                                                      
*                                                                               
BLDBK20  DS    0H                                                               
         CLI   0(R5),0             ANY MORE BOOKS TO PROCESS                    
         BE    BLDBK40                                                          
*                                                                               
         XC    IBLK,IBLK                                                        
         XC    OBLK,OBLK                                                        
         MVC   IBLK+3(1),0(R5)     BOOKVAL BITS                                 
         MVC   IBLK+4(1),3(R5)     BOOKTYPE                                     
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',IBLK),OBLK                                   
         CLI   DMCB+4,0            ANY ERRORS?                                  
         BNE   INVINPT             INVALID BOOK                                 
*                                                                               
         MVC   0(1,R3),OBLK+2      MOVE SOURCE INTO TABLE                       
         MVC   1(2,R3),1(R5)       MOVE BOOK INTO TABLE                         
*                                                                               
         LA    R3,3(R3)            NEXT ENTRY IN BK TABLE                       
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
*  CHECK IF WE ARE ADDING NEW RECS OR CHANGEING EXISTING RECS                   
*                                                                               
         ICM   RE,15,AUTL                                                       
         MVC   4(1,RE),NEWSE       SET UP TO NEW SE NUMBER (REC AGENCY)         
         MVC   KEY,HOLDKEY                                                      
         MVC   KEY+10(2),NEWREP                                                 
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
RPM200   BAS   RE,DELINVS          DELETE THE OLD INVENTORY                     
         BAS   RE,PUTINVS          WRITE NEW INVENTORY                          
         B     RPM100                                                           
*                                                                               
RPMEX    ICM   RE,15,AUTL                                                       
         MVC   4(1,RE),OLDSE       SET UP TO OLD SE NUMBER                      
         B     EXXMOD                                                           
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*--READ AND FILTER INVENTORY RECORDS                                            
*                                                                               
GTINV    NTR1                                                                   
         MVI   KEYLEN,16           DEFAULT LENGTH OF KEY CHECK                  
*                                                                               
         LA    R4,KEY                                                           
         USING REINVREC,R4                                                      
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   NEXTKEY,X'12'       FIRST TIME                                   
         BNE   *+14                NO GET NEXT RECORD                           
         MVC   KEY(27),NEXTKEY                                                  
         B     GTINV50                                                          
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,CSTAT                                                   
         MVC   RINVKINV,INVSTART                                                
         MVC   KEY+21(3),STRTOPT   MOVE DATE IN                                 
         MVC   COMPKEY,KEY                                                      
GTINV50  GOTO1 HIGH                                                             
         CLC   KEY(17),COMPKEY                                                  
         BNE   GTINV70                                                          
         OC    INVEND,INVEND     WAS INV # SPECIFIED                            
         BZ    GTINV100                                                         
         CLC   KEY+17(4),INVEND  CHECK IF INV # WITHIN RANGE                    
         BNH   GTINV100                                                         
GTINV70  MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
*                                                                               
GTINV100 CLI   RINVKSRC,C'M'       IS RECORD A MARKET FACT                      
         BE    GTINV500                                                         
         CLI   RINVKSRC,C'S'       IS RECORD A STATION FACT                     
         BE    GTINV500                                                         
         CLI   RINVKSRC,0          IS RECORD A HEADER                           
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
         CLC   KEY(17),COMPKEY                                                  
         BNE   GTINV380                                                         
         OC    INVEND,INVEND     WAS INV # SPECIFIED                            
         BZ    GTINV400                                                         
         CLC   KEY+17(4),INVEND  CHECK IF INV # WITHIN RANGE                    
         BNH   GTINV400                                                         
GTINV380 MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
GTINV400 CLI   RINVKSRC,0                                                       
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
         CLC   KEY(24),KEYSAVE                                                  
         BNE   GTINV800                                                         
*        GOTO1 =V(PRNTBL),DMCB,=C'BEFED',KEY,C'DUMP',30,=C'1D'                  
*                                                                               
         CLI   KEY+24,X'FF'        IS THIS A TEXT RECORD?                       
         BNE   GTINV580                                                         
*                                                                               
         CLI   TEXTOPT,C'N'        ADD TEXT RECORD?                             
         BNE   GTINV600            YES                                          
         B     GTINV550                                                         
*                                                                               
GTINV580 DS    0H                                                               
         CLI   KEY+24,C'M'         MARKET FACT?                                 
         BE    GTINV600                                                         
         CLI   KEY+24,C'S'         STATION FACT?                                
         BE    GTINV600                                                         
*                                                                               
         CLI   KEY+24,C'Z'         RATE RECORD?                                 
         BE    GTINV600                                                         
         CLI   KEY+24,C'E'                                                      
         BE    GTINV581                                                         
         CLI   KEY+24,C'R'                                                      
         BE    GTINV581                                                         
         CLI   KEY+24,C'X'                                                      
         BE    GTINV581                                                         
*                                                                               
         XC    OBLK,OBLK           OUTPUT BLOCK                                 
         XC    IBLK,IBLK           INPUT BLOCK:                                 
         MVC   IBLK+2(1),KEY+24      RINVKSRC                                   
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'K',IBLK),OBLK                                   
         CLI   DMCB+4,0            ANY ERRORS?                                  
         BE    *+6                 INVALID BOOK                                 
         DC    H'00'                                                            
*                                                                               
         CLI   OBLK+1,C'E'         ESTIMATED BOOK?                              
         BE    GTINV581                                                         
         CLI   OBLK+1,C'R'                                                      
         BE    GTINV581                                                         
         CLI   OBLK+1,C'X'                                                      
         BE    GTINV581                                                         
*                                                                               
         B     GTINV582                                                         
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
         CLC   KEY+24(3),0(RF)     SAME SRC AND BOOK?                           
         BE    GTINV600            YES - INCLUDE THIS BOOK                      
         LA    RF,3(RF)            CHECK NEXT ENTRY IN TABLE                    
         B     GTINV585                                                         
*                                                                               
GTINV590 DS    0H                                                               
         CLI   0(RF),X'FF'         ANY MATCH FOUND?                             
         BE    GTINV600            NO - INCLUDE THIS BOOK                       
*                                                                               
         CLC   KEY+24(3),0(RF)     SAME SRC AND BOOK?                           
         BE    GTINV550            YES - EXCLUDE THIS BOOK                      
         LA    RF,3(RF)            CHECK NEXT ENTRY IN TABLE                    
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
GTINV800 CLI   KEY,X'12'           READ ALL THE INVENTORY                       
         BNE   GTINV450                                                         
         MVC   NEXTKEY,KEY                                                      
         XC    NEXTKEY+24(3),NEXTKEY+24                                         
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
         CLI   HOLDKEY+24,C'M'                                                  
         BE    ADIN050                                                          
         CLI   HOLDKEY+24,C'S'                                                  
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
*                                                                               
*--DELETE INVENTORY RECORDS                                                     
*                                                                               
DELINVS  NTR1                                                                   
         L     R4,AIO                                                           
         GOTO1 GETREC              READ THE OLD INV HEADER                      
*                                                                               
         LA    R3,200                                                           
*                                                                               
DLI100   CLC   KEY(24),KEYSAVE     CHECK UP TO RECORD TYPE                      
         BNE   DLI200                                                           
         OI    KEY+27,X'80'        DELETE KEY                                   
         BAS   RE,MYDIRWRT                                                      
         GOTO1 SEQ                 GET NEXT RECORD                              
         BCT   R3,DLI100                                                        
         DC    H'0'                OVER 100 RECORDS CHANGED TOO MANY            
*  CHECK IF MARKET OR STATION FACT RECORD                                       
         CLI   HOLDKEY+24,C'M'                                                  
         BE    DLINEX                                                           
         CLI   HOLDKEY+24,C'S'                                                  
         BE    DLINEX                                                           
*                                                                               
*  REMOVE THE PASSIVE POINTERS (R4 POINTS TO RECORD)                            
DLI200   GOTO1 INVPTR,DMCB,0(R4),WORK2                                          
         GOTO1 DELPT,DMCB,WORK2                                                 
*                                                                               
DLINEX   B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*--ADD THE INVENTORY RECORDS                                                    
*                                                                               
PUTINVS  NTR1                                                                   
         MVC   AIO,AIO1                                                         
         L     R5,AIO1                                                          
         USING REINVREC,R5                                                      
         LA    R3,DISKADDR                                                      
*  CHECK IF MARKET OR STATION FACT RECORD                                       
         CLI   HOLDKEY+24,C'M'                                                  
         BE    PTIN050                                                          
         CLI   HOLDKEY+24,C'S'                                                  
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
         BAS   RE,MYDIRADD         COMMENT                                      
         B     NWPT4                                                            
         SPACE 1                                                                
NWPT3    MVC   KEY(28),0(R2)                                                    
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,MYDIRWRT         COMMENT                                      
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
         BAS   RE,MYDIRWRT         COMMENT                                      
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
         BAS   RE,MYDIRWRT         COMMENT                                      
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
INVPTR1  MVI   RIDPKTYP,X'92'                                                   
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
         LA    RE,DYPCONV                                                       
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
CVTDP200 LA    RE,DYPCONV          RESET THE TABLE                              
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
         BAS   RE,MYDIRWRT         UNDELETE THE POINTER  (COMMENT)              
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
***********************************************************************         
* DATAMGR INTERFACE                                                             
***********************************************************************         
MYFILADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'REPFILE ',KEY+28,AIO,DMWORK           
         BAS   RE,DMCHECK                                                       
         MVC   BSVDA,KEY+28     SAVE DISK ADDRESS                               
         B     YES                                                              
*                                                                               
MYFILWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'REPFILE ',KEY+28,AIO,DMWORK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'REPDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REPDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
RELO     DS    A                                                                
REPFILE  DC    CL8'REPFILE'                                                     
*                                                                               
BOOKFLG  DS    XL1                 BOOK FLAGS                                   
INCBK    EQU   X'01'               THERE ARE INCLUDE BOOKS                      
EXCBK    EQU   X'02'               THERE ARE EXCLUDE BOOKS                      
*                                                                               
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
         DC    CL4'KHTV'           KH --> TV  (TEST ENTRY)                      
         DC    CL4'KHB3'           KH --> TV  (TEST ENTRY)                      
         DC    CL4'B3KH'           B3 --> KH  (TEST ENTRY)                      
         DC    CL4'PVP9'           PV --> P9                                    
         DC    CL4'BLAM'           BL --> AM                                    
         DC    CL4'PVAM'           PV --> AM                                    
         DC    CL4'PQNB'           PQ --> NB                                    
         DC    CL4'AMSJ'           AM --> SJ                                    
         DC    CL4'PQPV'           PQ --> PV  (PAXSON - PETRY)                  
         DC    X'FF'                                                            
*                                                                               
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
*                                                                               
FLIST    DC    CL8'UREPFILE'                                                    
         DC    CL8'UREPDIR '                                                    
         DC    CL8'UREPRCV '                                                    
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
*                                                                               
INCBKTAB DS    XL13                TABLE FOR INCLUDE BOOKS (MAX 4)              
*                                   1 BYTE KSRC                                 
*                                   2 BYTES BOOK                                
*                                                                               
EXCBKTAB DS    XL13                TABLE FOR EXCLUDE BOOKS (MAX 4)              
*                                   1 BYTE KSRC                                 
*                                   2 BYTES BOOK                                
*                                                                               
TRBKLIST DS    XL64                BOOK ENTRIES BUILT BY REBKLST                
VREBKLST DS    A                   ADDRESS OF TRANSFER MODULE                   
IBLK     DS    CL5                 INPUT BLOCK FOR GETKSRC                      
OBLK     DS    CL5                 OUTPUT BLOCK FOR GETKSRC                     
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
*  ALL FIELDS DEFINED ABOVE THE DOUBLE LINE OF ASTERIKS                         
*  MUST ALSO BE DEFINED IN THE RERMP30 PHASE.                                   
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
WORK2    DS    CL200               SECOND WORK AREA                             
BSVDA    DS    CL4                 SAVED DISK ADDRESS                           
*                                                                               
KEYLEN   DS    CL1                 LENGTH OF KEY COMPARE                        
NEWSE    DS    CL1                 RECEIVING AGENCY SE NUMBER                   
OLDSE    DS    CL1                 GIVING AGENCY SE NUMBER                      
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
**PAN#1  DC    CL21'151RERMP1EX  05/01/02'                                      
         END                                                                    
