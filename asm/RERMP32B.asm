*          DATA SET RERMP32B   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET RERMP32    AT LEVEL 052 AS OF 12/22/97                      
*PHASE T81032A,*                                                                
         TITLE 'T81032 - RERMP32 - INV REP TRANSFER'                            
*                                                                               
***********************************************************************         
*                                                                     *         
*- RERMP32 -- INV REP TRANSFER                                        *         
*                                                                     *         
*  MOD LOG:                                                           *         
*  --------                                                           *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
         SPACE 2                                                                
T81032   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81032**,RR=RE                                                 
*                                                                               
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING T81032,RB,R7                                                     
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
VREX     B     EXXMOD                                                           
*                                                                               
REPERR   MVC   RERROR,=AL2(INVALID) ERROR - INVALID REP                         
         GOTO1 MYERROR                                                          
*                                                                               
         EJECT                                                                  
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
         ICM   RE,15,AUTL                                                       
         MVC   4(1,RE),NEWSE       SET UP TO NEW SE NUMBER (REC AGENCY)         
*  READ STATION RECORD SEE IF AGENCY CAN ERCEIVED TRANSFERRED DATA              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RESTAREC,R4                                                      
         MVI   RSTAKEY,X'02'                                                    
         MVC   RSTAKREP,NEWREP                                                  
         MVC   RSTAKREP,NEWREP                                                  
         MVC   RSTAKSTA,CSTAT                                                   
         CLI   RSTAKSTA+4,C'T'     IF MEDIA 'T' BLANK FILL                      
         BNE   *+8                                                              
         MVI   RSTAKSTA+4,X'40'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     CHECK STATION RECORD                         
         BNE   RPMEX               STATION DOES NOT EXIST EXIT                  
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'08',AIO),0                          
         CLI   12(R1),6                                                         
         BE    RPM080              ELEMENT NOT FOUND CONTINUE                   
         CLI   12(R1),0                                                         
         BNE   RPMEX               OTHER ERROR EXIT                             
         L     R4,12(R1)                                                        
         TM    64(R4),X'10'        IS THIS A NON COMPETITIVE STATION            
         BNZ   RPMEX                                                            
*                                                                               
RPM080   BAS   RE,DELINVS          DELETE THE INV FROM THE REC AGENCY           
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
RPM200   BAS   RE,PUTINVS          WRITE NEW INVENTORY                          
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
         BE    GTINV100                                                         
*                                                                               
GTINV70  MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
*                                                                               
GTINV100 CLI   RINVKSRC,C'M'       IS RECORD A MARKET FACT                      
         BE    GTINV500                                                         
         CLI   RINVKSRC,C'S'       IS RECORD A STATION FACT                     
         BE    GTINV500                                                         
         CLI   RINVKSRC,0          IS RECORD A HEADER                           
         BE    GTINV500                                                         
         B     GTINV350            SKIP TO NEXT RECORDS                         
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
         BE    GTINV400                                                         
GTINV380 MVI   KEY,X'FF'           END OF LOOKUP                                
         B     EXXMOD                                                           
GTINV400 CLI   RINVKSRC,0                                                       
         BE    GTINV500                                                         
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
*        GOTO1 =V(PRNTBL),DMCB,=C'KEY',KEY,C'DUMP',30,=C'1D'                    
*                                                                               
         LA    R5,DISKADDR+5       END OF LOOKUP                                
         LA    R6,199                                                           
GTINV550 GOTO1 SEQ                                                              
         CLC   KEY(24),KEYSAVE                                                  
         BNE   GTINV800                                                         
*        GOTO1 =V(PRNTBL),DMCB,=C'BEFED',KEY,C'DUMP',30,=C'1D'                  
         MVC   0(5,R5),KEY+27                                                   
         MVI   5(R5),X'FF'                                                      
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
         DROP  R4                                                               
         EJECT                                                                  
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
         BAS   RE,CKBOOK                                                        
         BZ    ADIN150                                                          
         MVC   RINVKREP,NEWREP                                                  
         BAS   RE,FLADD                                                         
ADIN150  LA    R3,5(R3)                                                         
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
*  CHECK IF WE ARE ADDING NEW RECS OR CHANGEING EXISTING RECS                   
*                                                                               
         LA    R4,KEY                                                           
         USING REINVREC,R4                                                      
         MVC   AIO,AIO1                                                         
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,NEWREP                                                  
         MVC   RINVKSTA,CSTAT                                                   
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE     INV HEADER RECORD                            
         BNE   DLINEX                                                           
*                                                                               
DLI200   CLC   KEY(17),KEYSAVE     CHECK UP TO RECORD TYPE                      
         BNE   DLI300                                                           
*        GOTO1 =V(PRNTBL),DMCB,=C'KEY',KEY,C'DUMP',30,=C'1D'                    
         OI    KEY+27,X'80'        DELETE KEY                                   
         BAS   RE,MYDIRWRT                                                      
DLI250   GOTO1 SEQ                 GET NEXT RECORD                              
         B     DLI200                                                           
*                                                                               
*  DELETE THE PASSIVE POINTERS                                                  
DLI300   XC    KEY,KEY                                                          
*                                                                               
         MVI   KEY,X'92'                                                        
         MVC   KEY+3(2),NEWREP                                                  
         MVC   KEY+5(5),CSTAT                                                   
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     INV HEADER RECORD                            
         BE    DLI400                                                           
         DC    H'0'                IF ACTIVE POINTER EXISTS                     
*                                  THERE MUST BE A PASSIVE POINTER              
*                                                                               
DLI400   CLC   KEY(10),KEYSAVE     CHECK UP TO RECORD TYPE                      
         BNE   DLINEX                                                           
*        GOTO1 =V(PRNTBL),DMCB,=C'PAS',KEY,C'DUMP',30,=C'1D'                    
         OI    KEY+27,X'80'        DELETE KEY                                   
         BAS   RE,MYDIRWRT                                                      
         GOTO1 SEQ                 GET NEXT RECORD                              
         B     DLI400                                                           
*                                                                               
DLINEX   B     EXXMOD                                                           
         DROP  R4                                                               
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
         BAS   RE,CKBOOK                                                        
         BZ    PTIN150                                                          
         MVC   RINVKREP,NEWREP                                                  
         BAS   RE,FLADD                                                         
*        GOTO1 =V(PRNTBL),DMCB,=C'PT4',(R3),C'DUMP',5,=C'1D'                    
PTIN150  LA    R3,5(R3)                                                         
         BCT   R4,PTIN100                                                       
         DC    H'0'                TABLE IS BLOWN DUMP                          
*                                                                               
PTINEX   B     EXXMOD                                                           
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE TO CHECK BOOK EXISTS ON TRACK REC                        
*                                                                               
         SPACE 1                                                                
CKBOOK   NTR1                                                                   
         L     R5,AIO1                                                          
         USING REINVREC,R5                                                      
         LA    R6,BUFF                                                          
         A     R6,=F'2000'         DBLOCK=BUFF+2000                             
         USING DEMBLOCK,R6                                                      
         L     R7,ACOMFACS                                                      
         USING COMFACSD,R7                                                      
*                                                                               
         CLI   RINVKSRC,0          BYPASS HEADERS                               
         BE    CKBYES                                                           
         CLI   RINVKSRC,C'M'       BYPASS MARKET LEVEL                          
         BE    CKBYES                                                           
         CLI   RINVKSRC,C'S'       BYPASS STATION LEVEL                         
         BE    CKBYES                                                           
         CLI   RINVKSRC,X'FF'      BYPASS TEXT RECORDS                          
         BE    CKBYES                                                           
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVI   DBFUNCT,DBGETTLB                                                 
         MVC   DBFILE,=C'TP '                                                   
         ST    R7,DBCOMFCS                                                      
         MVC   DBSELMED,RINVKSTA+4                                              
         MVI   DBSELDAY,X'40'                 MONDAY                            
         MVC   DBSELTIM,=XL4'076C07D0'        7-8P                              
         MVC   DBAREC,AIO2                                                      
         MVC   DBSELSTA,RINVKSTA                                                
         MVC   DBSELAGY,NEWREP                                                  
*                                                                               
         XC    WORK(20),WORK                                                    
         MVC   WORK+2(1),RINVKSRC                                               
         GOTO1 VGETKSRC,DMCB,(C'K',WORK),WORK+10                                
         CLI   WORK+11,C'E'        IS BOOK ESTIMATE                             
         BE    CKBYES              DON'T CHECK                                  
         MVC   DBSELSRC,WORK+10    SERVICE                                      
         MVC   DBBTYPE,WORK+14     BOOK TYPE                                    
         GOTO1 CDEMAND,DMCB,DBLOCK,0,0                                          
*        GOTO1 =V(PRNTBL),DMCB,=C'DEM',DBERROR,C'DUMP',1,=C'1D'                 
         CLI   DBERROR,0                                                        
         BE    CKBYES                                                           
*                                                                               
CKBNO    SR    R5,R5                                                            
CKBYES   LTR   R5,R5                                                            
         B     EXXMOD                                                           
         DROP  R5,R6,R7                                                         
*                                                                               
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
*******  GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',0(R4),C'DUMP',200,=C'1D'              
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
*******  GOTO1 =V(PRNTBL),DMCB,=C'AFTER',0(R4),C'DUMP',200,=C'1D'               
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
*        L     RF,AIO                                                           
*        GOTO1 =V(PRNTBL),DMCB,=C'FAD',0(RF),C'DUMP',100,=C'1D'                 
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'REPFILE ',KEY+28,AIO,DMWORK           
         BAS   RE,DMCHECK                                                       
         MVC   BSVDA,KEY+28     SAVE DISK ADDRESS                               
         B     YES                                                              
*                                                                               
MYFILWRT NTR1                                                                   
*        L     RF,AIO                                                           
*        GOTO1 =V(PRNTBL),DMCB,=C'FWR',0(RF),C'DUMP',100,=C'1D'                 
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'REPFILE ',KEY+28,AIO,DMWORK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRWRT NTR1                                                                   
*        GOTO1 =V(PRNTBL),DMCB,=C'DWR',KEY,C'DUMP',30,=C'1D'                    
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'REPDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRADD NTR1                                                                   
*        GOTO1 =V(PRNTBL),DMCB,=C'DAD',KEY,C'DUMP',30,=C'1D'                    
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
REPTAB   DC    CL4'AMSZ'           AM --> SZ                                    
         DC    CL4'CQSZ'           CQ --> SZ                                    
         DC    CL4'B3NS'           B3 --> NS  (TEST ENTRY)                      
         DC    CL4'KHSJ'           KH --> SJ  (TEST ENTRY)                      
         DC    CL4'KHTV'           KH --> TV  (TEST ENTRY)                      
         DC    CL4'KHB3'           KH --> TV  (TEST ENTRY)                      
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
         DC    CL8' DEMDIRN'                                                    
         DC    CL8' DEMFILN'                                                    
         DC    CL8' DEMDIRA'                                                    
         DC    CL8' DEMFILA'                                                    
         DC    CL8'X       '                                                    
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
       ++INCLUDE RERMPC8D                                                       
         EJECT                                                                  
       ++INCLUDE RERMPWTWA                                                      
         EJECT                                                                  
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
         EJECT                                                                  
RESTAREC DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
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
*                                                                               
DEMIN    DS    CL4                 DEMOS FOR DEMOUT CALL                        
DEMFLD   DS    CL4                 DEMO VALUE FROM DEMOUT CALL                  
         EJECT                                                                  
DEMBLOCK DSECT                                                                  
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
**PAN#1  DC    CL21'002RERMP32B  05/01/02'                                      
         END                                                                    
