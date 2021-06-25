*          DATA SET NEWRI70    AT LEVEL 140 AS OF 12/02/20                      
*PHASE T32070B,+0                                                               
*INCLUDE CLUNPK                                                                 
*INCLUDE CASHVAL                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE NETBLRDR                                                               
*INCLUDE CALCASHP                                                               
         TITLE 'T32070 - UTILITY PROGRAM'                                       
                                                                                
*****************************************************                           
*  OPTIONS:                                                                     
*                                                                               
*  ACT = Y     - OPTTYPE=X'01' SET ACTUAL DOLLARS TO ASSIGNED                   
*  UNM = Y     - OPTTYPE=X'02' UNMATCHED SPOTS DELETED                          
*  RATE = N    - OPTTYPE=X'04' CHANGE RATE TYPE TO N                            
*  LOCK        - OPTTYPE=X'08' LOCK UNITS                                       
*  UNLOCK      - OPTTYPE=X'10' UNLOCK UNITS                                     
*  SPLIT       - OPTTYPE=X'20' SPLIT UNITS                                      
*  COS2        - OPTTYPE=X'40' APPLY COST2 TO UNITS                             
*  ERNC        - OPTTYPE=X'80' CALCULATE EARNED COST OF PKG CPM                 
*  PF=         - OPTTYP2=X'01' PACKAGE FILTERS                                  
*  AUDIT       - OPTTYP2=X'02' SET AUDIT ON FROM PACKAGE RECORD                 
*  PGEST       - OPTTYP2=X'04' PGEST LIST                                       
*  I2BUG       - OPTTYP2=X'08' X'24' ELEM WITH NO AFFID DATE                    
*  POSTDT      - OPTTYP2=X'10' SET NUPOSTDT -> 0                                
*  TRADE       - OPTTYP2=X'20' TRADE                                            
*  ATRADE      - OPTTYP2=X'40' TRADE                                            
*  VTYPE       - OPTTYP2=X'80' VTYPE                                            
*  CASH%       - OPTTYP3=X'01' CASH PERCENTAGE                                  
*  SREP        - OPTTYP3=X'02' SREP CODE                                        
*  APKG        - OPTTYP3=X'04' APKG CALCULATE                                   
*  DPT         - OPTTYP3=X'08' DAYPART CHANGE                                   
*  DEMOLOCK    - OPTTYP3=X'10' SAVE OVERRIDE DEMOS PERMANENTLY                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*******************************************************                         
T32070   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEUT**,RA,RR=R2                                              
         LA    R6,2048(RA)         R6=THIRD BASE REGISTER                       
         LA    R6,2048(R6)                                                      
         USING T32070+8192,R6                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            ANETWS1=CLIENT RECORD                        
         USING NETSYSD,R9                                                       
         LA    R1,HEADSPC                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         L     R7,ANETWS3          ANETWS3=WORKING STORAGE                      
         USING WORKD,R7                                                         
         LA    R1,NDDEMBLK                                                      
         ST    R1,NBADEM                                                        
         CLI   OFFLINE,C'Y'                                                     
         BNE   NOTOFLN                                                          
         L     R1,=A(NETLIST)      NETLIST                                      
         ST    R1,NBANBUFF                                                      
         OI    NBINDS7,NBI7NTIO                                                 
         L     R1,=A(STALIST)      STALIST                                      
         ST    R1,NBCNVNTI                                                      
NOTOFLN  EQU   *                                                                
* - NEED TO HAVE COPIES WRITTEN TO RECOVERY FILE                                
         ICM   R1,15,TWAMASTC                                                   
         BZ    ENDMST                                                           
         USING MCBLOCK,R1                                                       
         L     R1,MCSSB                                                         
         USING SSBD,R1                                                          
         OI    SSBSTAT2,SSBSROLC   RECOVER OFFLINE COPIES                       
         DROP  R1                                                               
ENDMST   DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,TODAY)                                      
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   MAIN15                                                           
*                                                                               
         TM    OPTTYP2,X'40'        ATRADE?                                     
         BO    MAIN05                                                           
         TM    OPTTYP2,X'20'        TRADE?                                      
         BZ    MAIN10                                                           
         MVI   TESTRUN,C'Y'         CHECK IF UNITS BILLED (DON'T WRITE          
         BAS   RE,REPMOD            BACK TO FILE)                               
*                                                                               
         TM    MYFLAG,UNITBILL      IF UNITS HAVE BEEN BILLED/PAID,             
         BO    XIT                  THEN SKIP                                   
*                                                                               
MAIN05   DS    0H                                                               
         CLI   SPLTEST,C'Y'         LIVE RUN?                                   
         BE    XIT                                                              
         MVI   TESTRUN,C'N'         YES - PROCESS AS USUAL                      
*                                                                               
MAIN10   BAS   RE,REPMOD                                                        
         B     XIT                                                              
*                                                                               
MAIN15   CLI   MODE,VALREC                                                      
         BNE   MAIN20                                                           
         BAS   RE,EDITMOD                                                       
         B     XIT                                                              
MAIN20   EQU   *                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE                                                                  
EDITMOD  NTR1                                                                   
*                                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
* DO ALL VALIDATIONS EACH TIME                                                  
         MVI   FTERMFLG,0          F                                            
         MVI   BYT1,0              SET LOCK/UNLOCK FLAG IN BYT1                 
* DO ALL VALID                                                                  
         CLC   =C'ERNC',SPLOPT                                                  
         BNE   *+8                                                              
         OI    BYT2,X'01'               SET ERNC FLAG                           
         CLC   =C'DPT',SPLOPT                                                   
         BNE   *+8                                                              
         OI    BYT2,X'01'               SET DPT FLAG                            
         CLC   =C'AUDIT',SPLOPT         AUDIT?                                  
         BNE   *+8                                                              
         OI    BYT2,X'03'               ERNC =X'01',AUDIT=X'02'                 
         CLC   =C'APKG',SPLOPT                                                  
         BNE   *+8                                                              
         MVI   BYT2,X'09'                                                       
         CLC   =C'VTYPE',SPLOPT         AUDIT?                                  
         BNE   *+8                                                              
         OI    BYT2,X'04'               SET VTYPE FLAG                          
         CLC   =C'LOCK',SPLOPT                                                  
         BNE   *+8                                                              
         MVI   BYT1,C'L'                                                        
         CLC   =C'UNLOCK',SPLOPT                                                
         BNE   *+8                                                              
         MVI   BYT1,C'U'                                                        
         CLC   =C'SREP',SPLOPT                                                  
         BNE   *+8                                                              
         MVI   BYT1,C'S'                                                        
*                                                                               
         LA    R2,SPLCLIH               CLIENT                                  
         NETGO NVCLI,DMCB                                                       
         L     R2,NBAIO                                                         
         USING CLTHDR,R2                                                        
         MVC   CLICPRD,CPRPRD      CORPORATE PRODUCT                            
         DROP  R2                                                               
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
                                                                                
         LA    R2,SPLPROH               PRODUCT                                 
         NETGO NVPRDALL,DMCB                                                    
         TM    BYT2,X'01'              ,,IF EARNED COST                         
         BO    *+12                    ,,IF AUDIT                               
         CLI   BYT1,0                  ,,IF LOCK/UNLOCK/SREP                    
         BE    EDT03                                                            
         CLC   =C'POL',SPLPRO          ,,PROD MUST BE POL                       
         BNE   EDINV                                                            
*                                                                               
EDT03    DS    0H                       ESTIMATE                                
         TM    BYT2,X'01'          ,,IF EARNED COST REQUEST                     
         BO    EDT3B               ,, IF AUDIT                                  
         CLI   BYT1,0              ,,IF LOCK/UNLOCK/SREP                        
         BE    *+8                                                              
EDT3B    MVI   FTERMFLG,0          ,,ESTIMATE IS REQUIRED                       
         LA    R2,SPLESTH                                                       
         NETGO NVESTALL,DMCB,0,NDDEMBLK                                         
         TM    BYT2,X'01'          IF ERNC/AUDIT                                
         BO    *+12                                                             
         CLI   BYT1,0                                                           
         BE    EDT03C                                                           
         CLC   =C'ALL',SPLEST      ,,EST CAN NOT = ALL                          
         BE    EDINV                                                            
         L     R2,NBAIO                                                         
         USING EKEY,R2                                                          
         MVC   NBSELSTR,ESTART      ,,SET ESTIMATE START                        
         MVC   NBSELEND,EEND        ,,SET ESTIMATE END                          
         DROP  R2                                                               
*                                                                               
EDT03C   LA    R2,SPLNETH               NETWORK                                 
         TM    BYT2,X'04'          IS IT VTYPE                                  
         BNO   EDT03D                                                           
         MVI   FTERMFLG,0          ONLY ONE NET FOR VTYPE                       
         NETGO NVNET,DMCB                                                       
         L     RE,NBAIO                                                         
         USING STAREC,RE                                                        
         CLI   SPTYPE,C'S'                                                      
         BE    VTYPERR                                                          
         B     EDT03E                                                           
         DROP  RE                                                               
EDT03D   LA    R2,SPLNETH               NETWORK                                 
         TM    BYT2,X'01'          IS IT ERNC-AUDIT?                            
         BNO   EDT03DD                                                          
         MVI   FTERMFLG,0          ONLY ONE NET FOR ERNC                        
         NETGO NVNET,DMCB                                                       
         TM    BYT2,X'02'          AUDIT?                                       
         BO    EDT03E                                                           
         TM    BYT2,X'08'          TMEX?                                        
         BO    EDT03E                                                           
         CLI   NBSTATYP,C'S'       ONLY SYNDICATION FOR ERNC                    
         BNE   SYNERR                                                           
         B     EDT03E                                                           
EDT03DD  MVI   FTERMFLG,1                                                       
         NETGO NVNETALL,DMCB                                                    
         CLI   BYT1,0                                                           
         BE    EDT03E                                                           
         CLC   =C'ALL',SPLNET      IF LOCK/UNLOCK/SREP                          
         BE    EDINV               ALL IS INVALID                               
*                                                                               
EDT03E   CLI   BYT1,0              IF LOCK/UNLOCK/SREP                          
         BE    EDT04                                                            
         XC    SPLDPT,SPLDPT       CLEAR DPT FIELD                              
         OI    SPLDPTH+6,X'80'                                                  
         B     EDT04A                                                           
EDT04    DS    0H                       DAYPART                                 
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
         LA    R2,SPLDPTH               DAYPART                                 
         NETGO NVDPTALL,DMCB                                                    
*                                                                               
EDT04A   LA    R2,SPLPAKH               PACKAGE                                 
         TM    BYT2,X'01'               IF ERNC/AUDIT                           
         BNO   EDT04AA                                                          
         MVI   FTERMFLG,0                REQUIRED                               
         NETGO NVPAK,DMCB                                                       
         MVI   FTERMFLG,1                SET BACK TO OPTIONAL                   
         L     R2,NBAIO                                                         
         USING NPRECD,R2                                                        
*                                                                               
         TM    BYT2,X'08'          IF TMEX                                      
         BO    EDT04B                                                           
         TM    BYT2,X'02'          IF AUDIT                                     
         BNO   CHK09END                                                         
         GOTO1 HELLO,DMCB,(C'G',=C'UNTFILE '),(X'09',(R2)),0                    
         CLI   DMCB+12,0           X'09' ELEMENT MUST BE THERE                  
         BNE   AUDERR                                                           
         L     R1,DMCB+12                                                       
         MVC   PKG09SV,0(R1)       SAVE GROUP/COMMENT ELEM                      
         B     EDT04B                                                           
CHK09END EQU   *                                                                
*                                                                               
         OC    NPAKGCPM,NPAKGCPM                                                
         BZ    CPMERR                                                           
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   CPMERR                                                           
         USING NPK2D,R2                                                         
         OC    NPK2PDEM,NPK2PDEM                                                
         BZ    CPMERR                                                           
         B     EDT04B                                                           
CPMERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'*** ERROR - NO PACKAGE CPM/DEMO'                  
         B     ERX2                                                             
VTYPERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(33),=C'*CANNOT BE SYNDICATION FOR VTYPE*'                
         B     ERX2                                                             
SYNERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'*MUST BE SYNDICATION FOR ERNC*'                   
         B     ERX2                                                             
AUDERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'*PACKAGE MUST HAVE AUDIT GROUP*'                  
ERX2     GOTO1 ERREX2                                                           
         DROP  R2                                                               
*                                                                               
EDT04AA  NETGO NVPAKLOK,DMCB                                                    
         CLI   BYT1,0              ,,IF LOCK/UNLOCK/SREP                        
         BE    EDT04B                                                           
         CLC   =C'ALL',SPLPAK      ,,PKG CANNOT = ALL                           
         BE    EDINV                                                            
         L     R2,NBAIO                                                         
         USING NPRECD,R2                                                        
         CLI   0(R2),X'02'                                                      
         BNE   EDINV                                                            
         LA    R2,NPAKEL-NPKEY(R2)                                              
         USING NPAKEL,R2                                                        
         MVC   NBSELDP,NPAKDP           SET DAYPART                             
         DROP  R2                                                               
*                                                                               
EDT04B   TM    BYT2,X'08'          IF TMEX                                      
         BO    *+12                                                             
         CLI   BYT1,0              IF LOCK/UNLOCK/SREP                          
         BE    EDT04C                                                           
         XC    SPLSDT,SPLSDT       CLEAR START/END DATE FIELDS                  
         OI    SPLSDTH+6,X'80'                                                  
         XC    SPLEDT,SPLEDT                                                    
         OI    SPLEDTH+6,X'80'                                                  
         B     EDT04E                                                           
EDT04C   LA    R2,SPLSDTH               START DATE                              
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLEDTH               END DATE                                
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
EDT04E   MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         MVI   TESTRUN,C'Y'                                                     
         MVI   LOCKSW,0                                                         
         LA    R2,SPLTESTH        TEST RUN                                      
         CLI   5(R2),0                                                          
         BE    EDINV                                                            
         CLI   SPLTEST,C'Y'                                                     
         BE    EDT05                                                            
         CLI   SPLTEST,C'N'        LIVE RUN                                     
         BNE   EDINV                                                            
         MVI   TESTRUN,C'N'                                                     
         CLC   =C'SOON',CONWHEN                                                 
         BNE   EDT05                                                            
         MVI   LOCKSW,C'Y'           LOCK FOR UPDATIVE SOON                     
*->      BRAS  RE,LOCKEM             STUPID TO DO THIS HERE -                   
*                                    WHAT IF ERROR IN OPTIONS                   
         EJECT                                                                  
*        EDIT (CONTINUED)                                                       
                                                                                
EDT05    DS    0H                                                               
         LA    R2,SPLOPTH                                                       
         GOTO1 SCANNER,DMCB,(0,(R2)),SCNBLOCK,0                                 
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    EDINV                                                            
         LA    R3,SCNBLOCK                                                      
                                                                                
EDT10    CLC   12(3,R3),=C'ACT'      ACTUAL TO ASSIGNED $                       
         BNE   EDT11                                                            
         OI    OPTTYPE,X'01'                                                    
         B     EDT100                                                           
                                                                                
EDT11    DS    0H                                                               
         CLC   12(3,R3),=C'UNM'    DELETE UNMATCHED SPOTS                       
         BNE   EDT12                                                            
         OI    OPTTYPE,X'02'                                                    
         B     EDT100                                                           
*                                                                               
EDT12    DS    0H                                                               
         CLC   12(4,R3),=C'RATE'   RATE TYPE CHANGE                             
         BNE   EDT14                                                            
         CLC   =C'NULL',22(R3)     DELETE RATE TYPES                            
         BNE   EDT12A              NO                                           
         MVI   NEWRCOV,0           YES                                          
         XC    NEWRATE,NEWRATE     YES                                          
         OI    OPTTYPE,X'04'                                                    
         B     EDT100                                                           
*                                                                               
EDT12A   CLI   1(R3),2    2ND HALF LEN = 2 (RATE AND COVERAGE)                  
         BNE   EDINV                                                            
         OI    OPTTYPE,X'04'                                                    
         LA    R1,RTYPE                                                         
EDT12B   CLC   22(1,R3),0(R1)                                                   
         BE    EDT12D                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),C'0'          END OF LIST?                                 
         BE    EDINV               YES                                          
         B     EDT12B              NO                                           
EDT12D   MVC   NEWRATE,22(R3)                                                   
         MVC   NEWRCOV,23(R3)                                                   
         CLI   NEWRCOV,C'T'                                                     
         BE    EDT100                                                           
         CLI   NEWRCOV,C'I'                                                     
         BE    EDT100                                                           
         CLI   NEWRCOV,C'A'        IF ALL COST                                  
         BNE   EDINV                                                            
         MVI   NEWRCOV,0           SET FIELD TO BLANK                           
         B     EDT100                                                           
                                                                                
RTYPE    DC    C'JFCWYHTR0'                                                     
*                                                                               
EDT14    DS    0H                                                               
         CLC   =C'LOCK',12(R3)     LOCK UNITS                                   
         BNE   EDT15                                                            
         OI    OPTTYPE,X'08'                                                    
         B     EDT100                                                           
*                                                                               
EDT15    DS    0H                                                               
         CLC   =C'UNLOCK',12(R3)     UNLOCK UNITS                               
         BNE   EDT17                                                            
         OI    OPTTYPE,X'10'                                                    
         B     EDT100                                                           
*                                                                               
EDT17    DS    0H                                                               
         CLC   =C'SPLIT',12(R3)     SPLIT UNITS                                 
         BNE   EDT18                                                            
         OI    OPTTYPE,X'20'                                                    
         B     EDT100                                                           
*                                                                               
EDT18    DS    0H                                                               
         CLC   =C'COS2',12(R3)     APPLY COST2 TO UNITS                         
         BNE   EDT20                                                            
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   EDT19                                                            
*                                                                               
         OI    NBINDS2,NBBILLRD    USE BILLREADER TO SEE IF BILLED              
         OI    NBINDS4,NBI4BLRD    TELL NETIO NOT TO WRITE BACK TO FILE         
         L     R2,=A(NETBLRD)                                                   
         XC    0(200,R2),0(R2)                                                  
         USING NBLBILLD,R2                                                      
         STCM  R2,15,NBABILRD      A(NETBILLRD) DSECT AREA IN NETBLOCK          
*                                                                               
         MVC   NBLUNAIO,NBAIO      A(UNIT)                                      
         OI    NBLFUNC2,NBLDOLS    GET ME BILLING DOLLARS                       
*                                                                               
         LA    RF,BILLDOL                                                       
         ZAP   BILLDOL(8),=P'0'                                                 
         ZAP   BILLDOL+8(8),=P'0'                                               
         STCM  RF,15,NBLADLRS      A(RETURNED BILL DOLLARS)                     
         DROP  R2                                                               
*                                                                               
EDT19    OI    OPTTYPE,X'40'                                                    
         B     EDT100                                                           
*                                                                               
EDT20    DS    0H                                                               
         CLC   =C'ERNC',12(R3)     EARNED COST                                  
         BNE   EDT22                                                            
         OI    OPTTYPE,X'80'                                                    
         B     EDT100                                                           
*                                                                               
EDT22    DS    0H                                                               
         CLC   =C'PF',12(R3)       PACCKAGE FILTER                              
         BNE   EDT24                                                            
         CLI   1(R3),6             PKGFILTER>6?                                 
         BH    EDINV                                                            
         MVC   PKGFLTRS,22(R3)                                                  
         OI    OPTTYP2,X'01'                                                    
         B     EDT100                                                           
*                                                                               
EDT24    DS    0H                                                               
         CLC   =C'AUDIT',12(R3)       SET AUDIT BIT ON                          
         BNE   EDT26                                                            
         OI    OPTTYP2,X'02'                                                    
         B     EDT100                                                           
*                                                                               
EDT26    DS    0H                                                               
         CLC   =C'PGEST',12(R3)       LIST OF MISSING PGEST                     
         BNE   EDT28                                                            
         OI    OPTTYP2,X'04'                                                    
         B     EDT100                                                           
*                                                                               
EDT28    DS    0H                                                               
         CLC   =C'I2BUG',12(R3)       FIX I2BUG                                 
         BNE   EDT29                                                            
         OI    OPTTYP2,X'08'                                                    
         B     EDT100                                                           
*                                                                               
EDT29    DS    0H                                                               
         CLC   =C'POSTDT',12(R3)       FIX POSTDT                               
         BNE   EDT30                                                            
         OI    OPTTYP2,X'10'                                                    
         B     EDT100                                                           
*                                                                               
EDT30    DS    0H                                                               
         XC    TRDPCT,TRDPCT                                                    
*                                                                               
         CLC   =C'ATRADE',12(R3)        ATRADE                                  
         BNE   *+12                                                             
         OI    OPTTYP2,X'40'                                                    
         B     EDT30A                                                           
         CLC   =C'TRADE',12(R3)        TRADE                                    
         BNE   EDT31                                                            
         OI    OPTTYP2,X'20'                                                    
*                                                                               
EDT30A   TM    3(R3),X'80'             NUMERIC?                                 
         BZ    EDINV                                                            
*!!!     OC    8(4,R3),8(R3)           MUST BE >0                               
*!!!     BZ    EDINV                                                            
         MVC   TRDPCT,10(R3)           TRADE%                                   
*&&DO                                                                           
         CLI   OFFLINE,C'Y'                                                     
         BNE   EDT100                                                           
*                                                                               
         OI    NBINDS2,NBBILLRD    USE BILLREADER TO SEE IF BILLED              
         OI    NBINDS4,NBI4BLRD    TELL NETIO NOT TO WRITE BACK TO FILE         
         L     R2,=A(NETBLRD)                                                   
         XC    0(200,R2),0(R2)                                                  
         USING NBLBILLD,R2                                                      
         STCM  R2,15,NBABILRD      A(NETBILLRD) DSECT AREA IN NETBLOCK          
*                                                                               
         MVC   NBLUNAIO,NBAIO      A(UNIT)                                      
         OI    NBLFUNC,NBLBLD      IS THE UNIT BILLED?                          
*                                                                               
         LA    RF,BILLDOL                                                       
         ZAP   BILLDOL(8),=P'0'                                                 
         ZAP   BILLDOL+8(8),=P'0'                                               
         STCM  RF,15,NBLADLRS      A(RETURNED BILL DOLLARS)                     
*&&                                                                             
         B     EDT100                                                           
*****    DROP  R2                                                               
         SPACE 2                                                                
EDT31    DS    0H                                                               
         CLC   12(4,R3),=C'VTYPE'  VTYPE CHANGE                                 
         BNE   EDT40                                                            
*                                                                               
         LA    RE,VTYPTAB                                                       
*                                                                               
EDT32    CLI   0(RE),X'FF'                                                      
         BE    EDT35                                                            
         CLC   0(2,RE),22(R3)                                                   
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     EDT32                                                            
*                                                                               
         OI    OPTTYP2,X'80'                                                    
         MVC   NEWVTYP(2),22(R3)                                                
         CLI   1(R3),2                                                          
         BNE   EDINV                                                            
         B     EDT100                                                           
*                                                                               
EDT35    LA    RE,MINVTYP           MINUTE X MINUTE TABLE                       
*                                                                               
EDT37    CLI   0(RE),X'FF'                                                      
         BE    EDINV                                                            
         CLC   0(2,RE),22(R3)                                                   
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     EDT37                                                            
*                                                                               
         OI    OPTTYP2,X'80'                                                    
         MVC   NEWVTYP,22(R3)                                                   
         CLI   1(R3),3                                                          
         BNE   EDT100                                                           
         CLI   24(R3),C'P'          CHECK POD LOOKUP                            
         BNE   EDINV                                                            
         B     EDT100                                                           
*                                                                               
VTYPTAB  DC    CL32'CLCSC1C2C3C7PLPSP1P2P3P7ALASA7N3'                           
         DC    XL1'FF'                                                          
*                                                                               
MINVTYP  DC    CL12'MLMSM1M2M3M7'                                               
         DC    XL1'FF'                                                          
         SPACE 2                                                                
*                                                                               
*                                                                               
EDT40    DS    0H                                                               
         XC    CASHPCT,CASHPCT         CASHPCT CHANGE                           
*                                                                               
         CLC   =C'CASH%',12(R3)                                                 
         BNE   EDT42                                                            
*                                                                               
         TM    3(R3),X'80'             NUMERIC?                                 
         BZ    EDINV                                                            
         CLC   10(2,R3),=H'0'          MUST BE >= 0                             
         BL    EDINV                                                            
         CLC   10(2,R3),=H'100'        MUST BE <100                             
         BH    EDINV                                                            
* FOR THIS OPTION THERE CAN BE NO DATE OR PRODUCT FILTER                        
         LA    R2,SPLSDTH                                                       
         CLI   5(R2),0                                                          
         BNE   EDINV                                                            
         LA    R2,SPLEDTH                                                       
         CLI   5(R2),0                                                          
         BNE   EDINV                                                            
         LA    R2,SPLPROH               PRODUCT                                 
         CLI   5(R2),0                                                          
         BNE   EDINV                                                            
         LA    R2,SPLOPTH                                                       
*                                                                               
         OI    OPTTYP3,X'01'                                                    
         MVC   CASHPCT,=X'FFFF'         DEFAULT TO 0%                           
         OC    10(L'CASHPCT,R3),10(R3)                                          
         BZ    *+10                                                             
         MVC   CASHPCT,10(R3)                                                   
         B     EDT100                                                           
*                                                                               
EDT42    DS    0H                                                               
         XC    SREPCODE,SREPCODE       SPECIAL REP CHANGE                       
*                                                                               
         CLC   =C'SREP',12(R3)                                                  
         BNE   EDT45                                                            
*                                                                               
         CLI   22(R3),C'^'             TEST TO REMOVE REP                       
         BE    EDT43                                                            
*                                                                               
         TM    3(R3),X'80'             NUMERIC?                                 
         BZ    EDINV                                                            
*                                                                               
* VALIDATE THE REP                                                              
         XC    KEY,KEY             NOW VALIDATE THE REP                         
         LA    R2,KEY                                                           
         USING REPREC,R2                                                        
         MVI   REPKEY,C'0'                                                      
         MVC   REPKEY+1(L'REPKEY-1),REPKEY                                      
         MVI   REPKTYPE,C'R'                                                    
         MVI   REPKMED,C'N'                                                     
         MVC   REPKREP,22(R3)                                                   
         MVC   REPKAGY,NBSELAGY                                                 
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION ',KEY,AIO1                    
         L     RE,AIO1                                                          
         LA    R2,SPLOPTH                                                       
         CLC   KEY(L'REPKEY),0(RE)    TEST IF REP FOUND                         
         BNE   EDINV                  YES                                       
         MVC   SREPCODE,10(R3)                                                  
EDT43    OI    OPTTYP3,X'02'                                                    
         B     EDT100                                                           
         DROP  R2                                                               
         SPACE                                                                  
*                                                                               
*  TMEX OPTION                                                                  
EDT45    DS    0H                                                               
         XC    TMEXCST,TMEXCST         CLEAR TMEX COST                          
*                                                                               
         CLC   =C'APKG',12(R3)                                                  
         BNE   EDT48                                                            
*                                                                               
         TM    3(R3),X'80'          MUST BE NUMERIC                             
         BZ    EDINV                                                            
*                                                                               
         MVC   TMEXCST,8(R3)                                                    
         OI    OPTTYP3,X'04'                                                    
*                                                                               
         B     EDT100                                                           
         SPACE                                                                  
*                                                                               
*  DAYPART OPTION                                                               
EDT48    DS    0H                                                               
         XC    TMEXCST,TMEXCST         CLEAR TMEX COST                          
*                                                                               
         CLC   =C'DPT',12(R3)                                                   
         BNE   EDT95                                                            
*                                                                               
*                                                                               
*  VALIDATE A DAYPART                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
         MVC   NDPTKTYP,=XL2'0D07'                                              
         MVC   NDPTAGM,NBACTAM                                                  
*                                                                               
EDT50    MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR  ',KEY,KEY                     
         B     EDT54                                                            
EDT52    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'UNTDIR  ',KEY,KEY                     
                                                                                
EDT54    CLC   KEY(5),KEYSAVE      CHECK UP TO CLIENT                           
         BNE   EDT56               YES                                          
         CLC   NDPTDPTA,22(R3)     CHECK FOR MATCH ON CODE                      
         BNE   EDT52               GET NEXT RECORD                              
         B     EDT58                                                            
*                                                                               
* CHECK KEY FOR AGENCY OR CLIENT LEVEL CHECK                                    
* IF AGENCY LEVEL RESET KEY MOVE CLIENT CODE IN RESTART SEARCH                  
* IF CLIENT LEVEL EXIT ROUTINE DEMO WAS INVALID                                 
*                                                                               
EDT56    OC    KEYSAVE+3(2),KEYSAVE+3                                           
         BNZ   EDINV                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(5),KEYSAVE                                                   
         MVC   KEY+3(2),NBACTCLI                                                
         B     EDT50                                                            
*                                                                               
*  MOVE DATA OUT AND EXIT                                                       
EDT58    MVC   DPTEQU,NDPTDPTE                                                  
         OI    OPTTYP3,X'08'                                                    
         B     EDT100                                                           
         SPACE                                                                  
*                                                                               
*                                                                               
*                                                                               
EDT95    DS    0H                                                               
         CLC   =C'DEMOLOCK',12(R3)                                              
         BNE   EDINV                                                            
         OI    OPTTYP3,OP3DEMOQ                                                 
         MVI   REQSML,0                                                         
*                                                                               
EDT100   LA    R3,32(R3)                                                        
***->>>  LA    R5,1(R5)            WHAT WAS THIS? ERROR INDICATOR?              
         BCT   R4,EDT10                                                         
*                                                                               
EDTX     TM    OPTTYP3,OP3DEMOQ    DEMOLOCK?                                    
         JZ    EDTX10                                                           
         CLI   OPTTYPE,0           CAN'T HAVE ANY OTHER OPTION                  
         JNE   EDINV                                                            
         CLI   OPTTYP2,0                                                        
         JNE   EDINV                                                            
         J     EDTXX                                                            
*                                                                               
EDTX10   CLI   LOCKSW,C'Y'         LOCK FOR UPDATIVE SOON?                      
         BNE   EDTXX                                                            
         CLI   OFFLINE,C'Y'                                                     
         BE    LETIT                                                            
         TM    OPTTYPE,X'20'       FOR SPLIT?                                   
         BO    EDINV               SOON UPDATE IS INVALID                       
         CLI   TWAOFFC,C'*'        DDS?                                         
         BE    LETIT                                                            
         TM    OPTTYP2,X'01'       FOR PKG FILTER?                              
         BO    EDINV               SOON UPDATE IS INVALID                       
         TM    OPTTYPE,X'80'       FOR ERNC?                                    
         BO    EDINV               SOON UPDATE IS INVALID                       
LETIT    MVI   LOCKSW,C'L'         SO LOCK IT                                   
         BRAS  RE,LOCKEM                                                        
EDTXX    LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
* - REPORT MODE                                                                 
* - NETIO READS UNIT AND HOOKS TO MAINLINE FOR PROCESSING                       
                                                                                
REPMOD   NTR1                                                                   
                                                                                
         XC    COUNTER,COUNTER                                                  
         ZAP   ASSTOT,=P'0'                                                     
         ZAP   ACTTOT,=P'0'                                                     
         ZAP   INTTOT,=P'0'                                                     
         ZAP   IC2TOT,=P'0'                                                     
                                                                                
         TM    OPTTYPE,X'20'       IF SPLITTING UNITS                           
         BO    SPLITEM             DO MY OWN I/O                                
                                                                                
* - SET UP NETIO PARAMETERS                                                     
         MVI   NBSELPST,C'B'       LOCKED/UNLOCKED                              
         MVI   NBDATA,C'B'         PACKAGES/UNITS                               
         MVI   NBSELUOP,0          EST+ACT SCHEDULE                             
         MVI   NBUSER+13,C'N'      OVERRIDE NO PREEMPTS PROFILE                 
         MVI   NBRESUME,NBPROCPK   START AGAIN AT PACKAGES                      
*                                                                               
         TM    OPTTYPE,X'80'       ERNC ?                                       
         BNO   ERNCXQ                                                           
         MVI   NBDEMRAW,C'Y'           DON'T ADJUST DEMOS                       
         MVI   NBSELUOP,C'A'       ACT SCHEDULE                                 
         MVI   NBACTOPT,C'Y'       ACTUAL DEMOS                                 
         MVI   NBSELPST,0          UNLOCKED ONLY                                
         OI    NBSBKEND,NBEXPDM6   EXPAND DEMO                                  
**->     MVI   NBHUNOPT,C'Y'                                                    
ERNCXQ   EQU   *                                                                
                                                                                
         TM    OPTTYP2,X'20'       TRADE?                                       
         BO    *+12                                                             
         TM    OPTTYPE,X'08'       LOCK UNITS                                   
         BNO   RP9                                                              
*                                                                               
         BRAS  RE,CHKACC           SEE IF UNITS HAVE BILL/PAY                   
*                                                                               
         CLI   TESTRUN,C'N'        IF TEST RUN                                  
         BE    RP9                                                              
         TM    OPTTYP2,X'40'       ATRADE?                                      
         BO    RPLX                                                             
         TM    OPTTYP2,X'20'       TRADE?                                       
         BO    RPLX                                                             
                                                                                
RP9      LA    R1,MAINLINE                                                      
         ST    R1,NBHOOK                                                        
                                                                                
RP10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    RPLAST                                                           
         B     RP10                                                             
                                                                                
                                                                                
* - REQUEST LAST PUTS OUT CHANGED REC COUNT                                     
RPLAST   DS    0H                                                               
         TM    OPTTYP3,X'04'       TMEX PERCENT                                 
         BZ    RPL05                                                            
         BRAS  RE,TMEXFIN                                                       
*                                                                               
RPL05    TM    OPTTYPE,X'80'     ERNC?                                          
         BO    RPL12             ALREADY PRINTED                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(13),=C'UNITS UPDATED'                                          
         L     R2,COUNTER                                                       
         EDIT  (R2),(10,P+15),ALIGN=LEFT                                        
         LTR   R2,R2                                                            
         BZ    RPL10                                                            
         TM    OPTTYP3,OP3DEMOQ    DEMOLOCK?                                    
         JO    RPL11                                                            
         EDIT  (P8,ASSTOT),(12,P+34),2                                          
         EDIT  (P8,ACTTOT),(12,P+48),2                                          
         EDIT  (P8,IC2TOT),(12,P+62),2                                          
         EDIT  (P8,INTTOT),(12,P+76),2                                          
         B     *+10                                                             
RPL10    MVC   P+15(5),=C'=NONE'                                                
RPL11    GOTO1 SPOOL,DMCB,(R8)                                                  
RPL12    CLI   TESTRUN,C'Y'        IF TEST RUN                                  
         BE    RPLX                THAT'S ALL                                   
*        MVI   HALF,C'U'           ELSE/UNLOCK FOR UPDATIVE SOON                
         MVI   LOCKSW,C'U'           ELSE/UNLOCK FOR UPDATIVE SOON              
         BRAS  RE,LOCKEM                                                        
RPLX     B     XIT                                                              
                                                                                
         EJECT                                                                  
* - UNIT PROCESSING                                                             
MAINLINE NTR1                                                                   
         MVI   NBUPUNIT,C'N'       SET DON'T WRITE SWITCHES                     
         MVI   NBNOWRIT,C'N'                                                    
         CLI   TESTRUN,C'N'                                                     
         BNE   *+12                                                             
         MVI   NBUPUNIT,C'Y'       SET WRITE SWITCHES                           
         MVI   NBNOWRIT,C'Y'                                                    
                                                                                
* - DEMOLOCK                                                                    
         TM    OPTTYP3,OP3DEMOQ                                                 
         JZ    MN02                                                             
         CLI   NBMODE,NBPROCUN     UNITS ONLY                                   
         BNE   MNX                                                              
         BRAS  RE,DEMOLOCK                                                      
         BNE   MNX                                                              
         B     MN100                                                            
                                                                                
* - ACTUAL$ -> ASSIGNED$                                                        
MN02     TM    OPTTYPE,X'01'                                                    
         BNO   MN05                                                             
         CLI   NBMODE,NBPROCUN     UNITS ONLY                                   
         BNE   MNX                                                              
         BAS   RE,ASSACT                                                        
         BNE   MNX                                                              
         B     MN100                                                            
                                                                                
* - POSTDT$ -> 0                                                                
MN05     TM    OPTTYP2,X'10'                                                    
         BNO   MN10                                                             
         CLI   NBMODE,NBPROCUN     UNITS ONLY                                   
         BNE   MNX                                                              
         BAS   RE,POSTDT                                                        
         BNE   MNX                                                              
         B     MN100                                                            
                                                                                
                                                                                
* - DELETE UNMATCHED SPOTS                                                      
MN10     TM    OPTTYPE,X'02'                                                    
         BNO   MN20                                                             
         CLI   NBMODE,NBPROCUN     UNITS ONLY                                   
         BNE   MNX                                                              
         BAS   RE,UNMAT                                                         
         BNE   MNX                                                              
         B     MN100                                                            
                                                                                
* - CHANGE RATE TYPE                                                            
MN20     TM    OPTTYPE,X'04'       CHANGE RATE TYPE                             
         BNO   MN30                                                             
         CLI   NBMODE,NBPROCUN     UNITS ONLY                                   
         BNE   MNX                                                              
         BAS   RE,RATYPE                                                        
         B     MN100                                                            
                                                                                
* LOCK UNITS                                                                    
MN30     TM    OPTTYPE,X'08'                                                    
         BNO   MN40                                                             
         BAS   RE,UNITLOCK                                                      
         BE    MN100               PRINT OUT RECORD                             
         B     MNX                                                              
                                                                                
* - UNLOCK UNITS                                                                
MN40     DS    0H                                                               
         TM    OPTTYPE,X'10'                                                    
         BNO   MN50                                                             
         BAS   RE,UNLOCKU                                                       
         B     MN100                                                            
                                                                                
* - APPLY COST2 TO UNITS                                                        
MN50     DS    0H                                                               
         TM    OPTTYPE,X'40'                                                    
         BNO   MN60                                                             
         BAS   RE,APPCOS2                                                       
         BE    MN100               IF COS2 APPLIED - PRINT IT                   
         B     MNX                 ELSE, SKIP UNIT                              
                                                                                
         EJECT                                                                  
                                                                                
* - CALCULATE EARNED COST (PKGCPM X DEMO IMP)                                   
MN60     DS    0H                                                               
         CLI   BYT1,X'FF'          ERROR?                                       
         BE    MNX                 YES/EXIT                                     
         TM    OPTTYPE,X'80'                                                    
         BNO   MN70                                                             
         CLI   NBMODE,NBPROCPK                                                  
         BNE   MN65                                                             
         MVC   PKGCPM,NBPAKCPM     PROCESS PACKAGE                              
         OC    PKGCPM,PKGCPM       DO WE HAVE PKG CPM?                          
         BNZ   MN62                                                             
MN67     MVC   P+1(18),=C'*** NO PACKAGE CPM'                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   BYT1,X'FF'          SET ERROR                                    
         B     MNX                                                              
*                                                                               
MN62     L     R2,NBAIO                                                         
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPK2D,R2                                                         
         OC    NPK2PDEM,NPK2PDEM   PKG GUA DEMO?                                
         BNZ   *+6                                                              
         DC    H'0'                MUST BE HERE                                 
         MVC   PKGDEM3,NDDEMOS     SAVE FIRST DEMO                              
         MVC   NDDEMOS(3),NPK2PDEM SET IN PKGDEM FOR DEMOCON                    
         LA    R3,NDDEMOS          SET UP FOR DEMOCON                           
         LA    R4,DBLOCK                                                        
         MVC   DBCOMFCS,NBACOM                                                  
         MVC   DBFILE,=C'NTI'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVC   DBSELAGY,NBSELAGY                                                
*                                                                               
         ICM   RF,15,ADEMCON       DO WE HAVE DEMOCON?                          
         BNZ   MN63                          YES                                
         GOTO1 CALLOV,DMCB,0,X'D9000AE0'     A(DEMOCON)                         
         L     RF,DMCB                                                          
         ST    RF,ADEMCON                                                       
MN63     GOTO1 (RF),DMCB,(0,0(R3)),(7,WORK),(C'S',(R4))                         
         MVC   PKGDEM,WORK             PRINTABLE DEMO                           
         MVC   NDDEMOS(3),PKGDEM3      RESTORE ORIGINAL DEMO                    
         MVC   PKGDEM3,NPK2PDEM        SAVE IT                                  
         DROP  R2                                                               
                                                                                
MN65     CLI   NBMODE,NBPROCUN     UNITS ONLY                                   
         BNE   MNX                                                              
         BAS   RE,ERNC                                                          
         B     MNX                                                              
*                                                                               
MN70     DS    0H                                                               
         TM    OPTTYP2,X'01'       PKG FILTER?                                  
         BNO   MN80                                                             
         BAS   RE,PKGFLTR                                                       
         B     MN100                                                            
*                                                                               
MN80     DS    0H                                                               
         TM    OPTTYP2,X'02'       AUDIT                                        
         BNO   MN85                                                             
         CLI   NBMODE,NBPROCUN     UNITS                                        
         BE    *+12                                                             
         CLI   NBMODE,NBPROCPK     PKG                                          
         BNE   MNX                                                              
         BAS   RE,AUDITRTN                                                      
         B     MN100                                                            
*                                                                               
MN85     DS    0H                                                               
         TM    OPTTYP2,X'40'                                                    
         BO    *+12                                                             
         TM    OPTTYP2,X'20'       TRADE?                                       
         BNO   MN87                                                             
         BAS   RE,TRADEPCT                                                      
         B     MN100                                                            
*                                                                               
MN87     DS    0H                                                               
         TM    OPTTYP2,X'80'       VTYPE                                        
         BNO   MN88                                                             
         BAS   RE,VTYPERTN                                                      
         B     MN100                                                            
*                                                                               
MN88     DS    0H                                                               
         TM    OPTTYP3,X'01'       CASH%                                        
         BNO   MN89                                                             
         BAS   RE,CASHPCTG                                                      
         B     MN100                                                            
*                                                                               
MN89     DS    0H                                                               
         TM    OPTTYP3,X'02'       SPECIAL REP                                  
         BNO   MN90                                                             
         BAS   RE,SPECREP                                                       
         B     MN100                                                            
*                                                                               
MN90     DS    0H                                                               
         TM    OPTTYP2,X'04'       PGEST RECS                                   
         BNO   MN92                                                             
         BAS   RE,PGESTRTN                                                      
         B     MNX                                                              
*                                                                               
MN92     DS    0H                                                               
         TM    OPTTYP3,X'04'       TMEX PERCENT                                 
         BNO   MN95                                                             
         BAS   RE,TMEXRTN                                                       
         BNZ   MNX                                                              
         B     MN100                                                            
*                                                                               
MN95     DS    0H                                                               
         TM    OPTTYP2,X'08'           I2BUG                                    
         BNO   MN96                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BNE   MNX                                                              
         BAS   RE,I2BUG                                                         
         BNE   MNX                                                              
         B     MN100                                                            
*                                                                               
MN96     DS    0H                                                               
         DC    H'0'                                                             
         EJECT                                                                  
                                                                                
* - PRINT OUT CHANGED RECORDS                                                   
                                                                                
MN100    MVC   P+1(3),NBCLICOD     PRINTABLE CLIENT CODE                        
         MVC   P+5(6),NBACTPRG     PROGRAM                                      
         MVC   P+13(4),NBACTNET    NETWORK                                      
         ZIC   R3,NBACTEST         ESTIMATE                                     
         EDIT  (R3),(3,P+19)                                                    
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(8,P+24)                                
         EDIT  (B1,NBACTSUB),(3,P+33)                                           
         EDIT  (B1,NBPACK),(3,P+37)                                             
                                                                                
         L     R2,NBAIO                                                         
         CLI   0(R2),X'04'         IS IT UNIT                                   
         BNE   MN110               NO                                           
         TM    OPTTYP3,OP3DEMOQ    DEMOLOCK?                                    
         JZ    *+12                                                             
         BRAS  RE,PRTDLOCK                                                      
         J     MN110                                                            
                                                                                
         MVI   ELCODE,1            YES                                          
         USING NUMAINEL,R2                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
**       EDIT  (B4,NUASSIGN),(12,P+34),2                                        
**       EDIT  (B4,NUACTUAL),(12,P+48),2                                        
**       EDIT  (B4,NUINTEG),(12,P+76),2                                         
         EDIT  (B4,NUASSIGN),(12,P+44),2                                        
         EDIT  (B4,NUACTUAL),(12,P+58),2                                        
         EDIT  (B4,NUINTEG),(12,P+86),2                                         
         ICM   R1,15,NUASSIGN                                                   
         CVD   R1,DUB                                                           
         AP    ASSTOT,DUB                                                       
         ICM   R1,15,NUACTUAL                                                   
         CVD   R1,DUB                                                           
         AP    ACTTOT,DUB                                                       
         ICM   R1,15,NUINTEG                                                    
         CVD   R1,DUB                                                           
         AP    INTTOT,DUB                                                       
         DROP  R2                                                               
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'74'        COST 2 ELEMENT                               
         USING NUCOS2D,R2                                                       
         BAS   RE,GETEL                                                         
         BNE   MN110                                                            
         EDIT  (B4,NUC2AINT),(12,P+72),2                                        
         ICM   R1,15,NUC2AINT                                                   
         CVD   R1,DUB                                                           
         AP    IC2TOT,DUB                                                       
         B     MN110                                                            
*                                                                               
MN110    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         ST    R1,COUNTER                                                       
*                                                                               
MNX      B     XIT                                                              
         EJECT                                                                  
                                                                                
* - ACTUAL$ -> ASSIGNED$ OPTION                                                 
ASSACT   NTR1                                                                   
         L     R2,NBAIO                                                         
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   MNX                                                              
         USING NUMAINEL,R2                                                      
         CLC   NUASSIGN,=X'00000000' IF NO ASSIGNED                             
         BNE   ASSX                                                             
         TM    NBUNITST,X'08'        IF ASSIGNED COST INPUT                     
         BO    ASSX                  SKIP                                       
         MVC   NUASSIGN,NUACTUAL      MOVE IN ACTUAL                            
ASSX     B     XIT                                                              
*                                                                               
* - POSTDT  -> SET IT TO 0                                                      
POSTDT   NTR1                                                                   
         L     R2,NBAIO                                                         
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   POSTDTX                                                          
         USING NUMAINEL,R2                                                      
         CLI   NUPOSTDT,C'C'       IF CONFORMED                                 
         BNE   POSTDTX                                                          
         MVI   NUPOSTDT,0          CLEAR IT                                     
POSTDTX  B     XIT                                                              
*                                                                               
         EJECT                                                                  
* - PREEMPT UNMATCHED SPOTS                                                     
UNMAT    NTR1                                                                   
*                                                                               
         CLC   NBAFFTIM,=X'0000'   ...IF NO AFFID TIME                          
         BNE   UNMX                                                             
         CLC   NBSDAFDT,=X'0000'   ...AND NO AFFID DATE                         
         BNE   UNMX                                                             
         CLI   TESTRUN,C'Y'           (IF TEST RUN/ EXIT NOW)                   
         BE    UNMX                                                             
         L     R2,NBAIO                                                         
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R2                                                      
         OI    NUUNITST,X'40'      ...PREEMPT RECORD                            
         SR    R1,R1               RESET CC TO =                                
         LTR   R1,R1                                                            
         B     UNMX                                                             
         EJECT                                                                  
* - SPLIT UNITS -                                                               
* _ CALL NETIO TO GET FIRST VALID UNIT AND AFTER THAT                           
* - DO MY OWN I/O LEST NETIO INTERFERE WITH CREATING/WRITING                    
* - BACK OF UNITS                                                               
                                                                                
SPLITEM  DS    0H                                                               
* - CLEARED NEEDED STORAGE AREAS                                                
         XC    KEYSV,KEYSV                                                      
         XC    MYKEY,MYKEY                                                      
         XC    HALF,HALF                                                        
         MVI   BYTE,0                                                           
* - SET UP TO READ 1ST UNIT                                                     
         MVI   NBDATA,C'U'         UNITS                                        
         MVI   NBSELUOP,0          EST+ACT SCHEDULE                             
         MVI   NBUSER+13,C'N'      OVERRIDE NO PREEMPTS PROFILE                 
         MVI   NBSELPST,C'U'       UNLOCKED                                     
SPLT00   NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBPROCUN                                                  
         BE    SPLT02                                                           
         CLI   NBMODE,NBREQLST                                                  
         BE    RPLAST                                                           
         B     SPLT00                                                           
SPLT02   CLI   NBPRD2,0            REJECT PIGGY-BACKS                           
         BNE   SPLT00                                                           
* - GOT 1ST VALID UNIT - IF PRD NOT = ALL/POL, SAVE ITS 1BYTE PRD CODE          
         MVI   NBSPLPRN,0          SET TO 0 IF ALL PRODS                        
         CLC   NBSELPRD,=C'ALL'                                                 
         BE    SPLT05                                                           
         CLC   NBSELPRD,=C'POL'                                                 
         BE    SPLT05                                                           
         MVC   NBSPLPRN,NBPRD      ELSE SAVE PROD NUMBER                        
                                                                                
                                                                                
* NETBLOCK FILLED IN - NBSPLPRN HAS VALID PROD NUMBER OR 0                      
* NOW DO MY OWN I/O TO SIMPLIFY SPLITTING/WRITING BACK OF UNITS                 
*                                                                               
SPLT05   L     R2,NBAIO                                                         
         MVC   KEY(20),0(R2)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         B     SPLT10                                                           
*                                                                               
SPLTSEQ  GOTO1 SEQ                                                              
                                                                                
SPLT10   CLC   KEY(4),KEYSAVE      AGY/MED - CLIENT                             
         BNE   SPLTX                                                            
                                                                                
* KEY FILTERS                                                                   
         LA    R2,KEY                                                           
         USING NUKEY,R2                                                         
         TM    NUPACKST,X'A1'      LOCKED/FROZEN/NOSHOW DELETE                  
         BNZ   SPLTSEQ                                                          
                                                                                
         CLC   NBCMPSTR,NUKDATE    DATE                                         
         BH    SPLTSEQ                                                          
         CLC   NBCMPEND,NUKDATE                                                 
         BL    SPLTSEQ                                                          
                                                                                
         CLC   =C'ALL',NBSELNET                                                 
         BE    CHKEST                                                           
         CLI   NBSELNET,0                                                       
         BE    CHKEST                                                           
         CLC   NBSELNET,NUKNET     NETWORK                                      
         BNE   SPLTSEQ                                                          
                                                                                
CHKEST   CLC   NBSELEST,NUKEST     ESTIMATE                                     
         BNE   SPLTSEQ                                                          
                                                                                
         CLI   NBSELDP,X'40'       DAYPART                                      
         BNH   SPLT12                                                           
         CLC   NBSELDP,NUKDP                                                    
         BNE   SPLTSEQ                                                          
                                                                                
                                                                                
         DROP  R2                                                               
SPLT12   GOTO1 GETREC                                                           
                                                                                
* - RECORD FILTERS                                                              
         L     R2,NBAIO                                                         
         USING NURECD,R2                                                        
                                                                                
         CLI   NBSELPAK,0                                                       
         BE    *+14                                                             
         CLC   NBSELPAK,NUPACK     PACKAGE                                      
         BNE   SPLTSEQ                                                          
                                                                                
         CLI   NUPRD2,0            ONLY UNITS WITH 1 PROD                       
         BNE   SPLTSEQ                                                          
                                                                                
         CLI   NBSPLPRN,0          ALL PRODS?                                   
         BE    *+14                YES                                          
         CLC   NBSPLPRN,NUPRD      NO/IS IT A MATCH?                            
         BNE   SPLTSEQ                                                          
                                                                                
         CLI   NULEN,30            IF NOT 30 SECONDS                            
         BNE   SPLTSEQ             SKIP                                         
                                                                                
         L     R2,NBAIO               SKIP MULTI PRODS                          
         MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         BE    SPLTSEQ                                                          
                                                                                
         L     R2,NBAIO               SKIP PAID UNITS                           
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BE    SPLTSEQ                                                          
                                                                                
         L     R2,NBAIO               SKIP BILLED UNITS                         
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    SPLTSEQ                                                          
                                                                                
         L     R2,NBAIO               SKIP TRAFFICKED UNITS                     
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BE    CHKTRAF                                                          
* - NO TRAFFIC ELEMENT ??? SO LET'S ADD ONE !!!!                                
         XC    SCNBLOCK(100),SCNBLOCK                                           
         MVC   SCNBLOCK(2),=X'2150'                                             
         GOTO1 =V(HELLO),DMCB,(C'P',=C'UNTFILE '),NBAIO,SCNBLOCK,0              
         CLI   DMCB+12,0                                                        
         BE    CHKTRAFX                                                         
         DC    H'0'                                                             
         USING NUCMLEL,R2                                                       
CHKTRAF  OC    NUCML1,NUCML1                                                    
         BNZ   SPLTSEQ                                                          
CHKTRAFX EQU   *                                                                
         DROP  R2                                                               
                                                                                
*                                                                               
* - SPLIT UNIT LENGTH AND WRITE UNIT IN NBAIO BACK WITH NEW LENGTH              
         L     R2,NBAIO                                                         
         USING NURECD,R2                                                        
         SR    R0,R0               SPLIT UNIT LENGTH                            
         ZIC   R1,NULEN                                                         
         LR    R3,R1                                                            
         D     R0,=F'2'                                                         
         STC   R1,HALF             SAVE NEW LENGTH OF CURRENT UNIT              
         SR    R3,R1               GET LENGTH OF FUTURE CREATED UNIT            
         STC   R3,BYTE             AND SAVE LENGTH IN BYTE                      
                                                                                
* - GET NEXT VALID LINE USING X'84'KEY                                          
         L     R2,NBAIO                                                         
         MVC   KEY,0(R2)                                                        
         MVC   MYKEY,KEY           SAVE ACTIVE KEY IN MYKEY                     
         LA    R2,KEY                                                           
         USING NUKPKEY,R2                                                       
         XC    KEY+4(16),KEY+4                                                  
         MVI   KEY,X'84'                                                        
         MVC   NUKPNET,MYKEY+7     NETWORK                                      
         MVC   NUKPPROG,MYKEY+11   PROGRAM                                      
         MVC   NUKPDATE,MYKEY+4    DATE                                         
         MVC   NUKPEST,MYKEY+17    ESTIMATE                                     
         MVC   NUKPSUB,MYKEY+18    SUBLINE                                      
         MVC   NUKPDP,MYKEY+19     DAYPART                                      
                                                                                
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
SPLT20   CLC   KEY(17),KEYSAVE                                                  
         BNE   SPLT22                                                           
         MVC   FULL(1),NUKPSUB     SAVE SUB-LINE                                
         GOTO1 SEQ                                                              
         B     SPLT20                                                           
SPLT22   NI    DMINBTS,X'FF'-X'08' TURN IT OFF                                  
         CLI   FULL,X'C1'         HAVE WE REACHED THE LINE NUMB LIMIT?          
         BNL   SPLTSEQ             YES/DON'T SPLIT THIS UNIT                    
                                                                                
                                                                                
*  ROOM FOR NEW UNIT *         WRITE BACK CURRENT UNIT WITH NEW LENGTH          
                                                                                
                                                                                
         CLI   TESTRUN,C'N'        TEST RUN?                                    
         BNE   SPLT25                                                           
                                                                                
                                                                                
*  GETREC BEFORE PUTREC                                                         
         L     R2,NBAIO                                                         
         MVC   KEY(20),0(R2)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
                                                                                
* SET NEW VALUES IN  CURRENT UNIT                                               
         L     R2,NBAIO                                                         
         MVI   ELCODE,2            GET SECONDARY DATA ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   SPLT23                                                           
         USING NUSDREL,R2                                                       
         OI    NUSDST4,X'40'       SET UNIT SPLIT BY UTILS PROGRAM              
                                                                                
SPLT23   L     R2,NBAIO                                                         
         USING NURECD,R2                                                        
         MVC   NULEN,HALF          SET NEW LENGTH                               
* HALVE COST                                                                    
         SR    R0,R0                                                            
         ICM   R1,15,NUASSIGN                                                   
         D     R0,=F'2'                                                         
         ICM   R0,15,NUASSIGN      SAVE IT                                      
         STCM  R1,15,NUASSIGN      REPLACE IT                                   
         SR    R0,R1                                                            
         STCM  R1,15,NEWASSGN                                                   
*                                                                               
         SR    R0,R0                                                            
         ICM   R1,15,NUACTUAL                                                   
         D     R0,=F'2'                                                         
         ICM   R0,15,NUACTUAL      SAVE IT                                      
         STCM  R1,15,NUACTUAL      REPLACE WITH HALF                            
         SR    R0,R1                                                            
         STCM  R1,15,NEWACTUL                                                   
*                                                                               
         GOTO1 =V(CALCASHP),DMCB,NBAIO,ACOMFACS,CLICPRD                         
*                                                                               
         GOTO1 PUTREC              WRITE TO FILE                                
         MVC   KEYSV,0(R2)         SAVE KEY FOR SEQ READ                        
                                                                                
SPLT25   BAS   RE,WRTSPLIT         PRINT RECORD                                 
                                                                                
         DROP  R2                                                               
*                                                                               
* - CREATE PARTNER UNIT WITH NEW LENGTH AND LINE NUMBER                         
*                                                                               
         L     R2,NBAIO                                                         
         USING NURECD,R2                                                        
         MVC   NULEN,BYTE          *** NEW LENGTH                               
         ZIC   R1,FULL                                                          
         LA    R1,1(R1)                                                         
         STC   R1,NUKSUB           *** NEW LINE NUMBER                          
         MVC   BYTE,NUKSUB         SAVE NEW LINE NUMBER                         
         MVC   NUASSIGN,NEWASSGN   *** NEW ASSIGNED COST                        
         MVC   NUACTUAL,NEWACTUL   *** NEW ACTUAL COST                          
*                                                                               
         GOTO1 =V(CALCASHP),DMCB,NBAIO,ACOMFACS,CLICPRD                         
*                                                                               
         L     R2,NBAIO                                                         
         MVI   ELCODE,2            SECODARY DATA ELEMENT                        
         USING NUSDREL,R2                                                       
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    NUSDST4,X'40'       UNIT SPLIT BY UTILS PROGRAM                  
* SET CREATION DATE                                                             
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'99'        CREATION ELEMENT                             
         USING NUACTD,R2                                                        
         BAS   RE,GETEL                                                         
         BNE   SPLT28                                                           
         MVC   NUACTADT,TODAY      TODAY'S DATE                                 
         XC    NUACTCCD,NUACTCCD                                                
         XC    NUACTCDT,NUACTCDT                                                
         XC    NUACTACD,NUACTACD                                                
         DROP  R2                                                               
*                                                                               
SPLT28   ICM   R1,15,NBINTEG       ADD UP INTEGRATION                           
         CVD   R1,DUB                                                           
         AP    INTTOT,DUB                                                       
                                                                                
         CLI   TESTRUN,C'N'        TEST RUN?                                    
         BE    SPLT30                                                           
         BAS   RE,WRTSPLIT         YES/PRINT                                    
         MVC   KEY,MYKEY           RESTORE SEQ READ                             
         GOTO1 HIGH                                                             
***      GOTO1 =V(PRNTBL),DMCB,=C'KEY0',KEY,C'DUMP',25,=C'1D'                   
         B     SPLTSEQ                 AND GET NEXT RECORD                      
                                                                                
SPLT30   GOTO1 ADDREC              NO/WRITE TO FILE                             
         BAS   RE,WRTSPLIT         PRINT RECORD                                 
                                                                                
                                                                                
                                                                                
* CREATE PASSIVE POINTERS WITH NEW SUB-LINE NUMBER                              
         L     R2,NBAIO                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(20),0(R2)       NEW KEY                                      
         GOTO1 HIGH                HIGH TO GET DISK ADDRESS                     
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   MYKEY,KEY           SAVE KEY IN MYKEY                            
                                                                                
         LA    R2,KEY             CREATE X'84' PASSIVE                          
         USING NUKPKEY,R2                                                       
         XC    KEY+4(16),KEY+4                                                  
         MVI   KEY,X'84'                                                        
         MVC   NUKPNET,MYKEY+7                                                  
         MVC   NUKPPROG,MYKEY+11                                                
         MVC   NUKPDATE,MYKEY+4                                                 
         MVC   NUKPEST,MYKEY+17                                                 
         MVC   NUKPSUB,BYTE         SAVED NEW LINE NUMBER                       
         MVC   NUKPDP,MYKEY+19                                                  
         MVC   FILENAME,=C'UNTDIR  '                                            
         BAS   RE,ADDDIR                                                        
         XC    FILENAME,FILENAME                                                
                                                                                
         USING NUKDKEY,R2            CREATE X'94' PASSIVE                       
         XC    KEY+4(16),KEY+4                                                  
         MVI   KEY,X'94'                                                        
         MVC   NUKDEST,MYKEY+17                                                 
         MVC   NUKDNET,MYKEY+7                                                  
         BAS   RE,DAYLOOK           PUTS ONE BYTE DAY CODE IN NUKDDAY           
         MVC   NUKDTIME,MYKEY+6                                                 
         MVC   NUKDPROG,MYKEY+11                                                
         MVC   NUKDDATE,MYKEY+4                                                 
         MVC   NUKDSUB,BYTE           NEW SAVED LINE NUMBER                     
         MVC   FILENAME,=C'UNTDIR  '                                            
         BAS   RE,ADDDIR                                                        
         XC    FILENAME,FILENAME                                                
                                                                                
*                                                                               
* EXISTING UNIT WITH NEW LENGTH AND ITS NEW PARTNER UNIT ADDED                  
*                                                                               
* RESET SEQUENTIAL READ                                                         
         MVC   KEY(20),KEYSV        SAVED KEY OF NEW PARTNER UNIT               
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         B     SPLTSEQ                                                          
                                                                                
SPLTX    DS    0H                                                               
         MVC   P+1(19),=C'*** INTEG TOTAL ***'                                  
         EDIT  (P8,INTTOT),(10,P+22),FLOAT=$                                    
         B     UNMX                                                             
                                                                                
* SPLITEM ROUTINE WRITES ITS OWN REPORT                                         
WRTSPLIT NTR1                                                                   
         L     R2,NBAIO                                                         
         USING NURECD,R2                                                        
         MVC   P+5(6),NUKPROG      PROGRAM                                      
         MVC   P+13(4),NUKNET      NETWORK                                      
         ZIC   R3,NUKEST           ESTIMATE                                     
         EDIT  (R3),(3,P+19)                                                    
         GOTO1 DATCON,DMCB,(2,NUKDATE),(8,P+24)                                 
         MVI   P+32,C'-'                                                        
         EDIT  (B1,NUKSUB),(3,P+33)  ..PRINT OUT LENGTH                         
         EDIT  (B1,NULEN),(3,P+38)  ..PRINT OUT LENGTH                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         ST    R1,COUNTER                                                       
         B     UNMX                                                             
         EJECT                                                                  
*                                                                               
UNMX     B     XIT                                                              
*                                                                               
*                                                                               
* DATAMGR WRITE TO DIRECTORY                                                    
ADDDIR   NTR1                                                                   
         CLI   TESTRUN,C'N'                                                     
         BNE   ADDDX                                                            
         GOTO1 DATAMGR,DMCB,=C'DMADD',FILENAME,KEY,KEY                          
ADDDX    B     XIT                                                              
                                                                                
                                                                                
         EJECT                                                                  
* - R2 MUST POINT TO SECOND PASSIVE KEY                                         
*                                                                               
DAYLOOK  DS    0H                                                               
         LA    R4,DAYLKUP                                                       
DLK2     CLC   0(3,R4),NBDAYNAM                                                 
         BE    DLK5                                                             
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   DLK2                                                             
         DC    H'0'                                                             
DLK5     MVC   NUKDDAY,3(R4)                                                    
         BR    RE                                                               
*                                                                               
DAYLKUP  EQU   *                                                                
         DC    CL3'ALL',XL1'FF'                                                 
         DC    CL3'M-F',XL1'08'                                                 
         DC    CL3'MON',XL1'01'                                                 
         DC    CL3'TUE',XL1'02'                                                 
         DC    CL3'WED',XL1'03'                                                 
         DC    CL3'THU',XL1'04'                                                 
         DC    CL3'FRI',XL1'05'                                                 
         DC    CL3'SAT',XL1'06'                                                 
         DC    CL3'SUN',XL1'07'                                                 
         DC    CL3'M-S',XL1'09'                                                 
         DC    CL3'VAR',XL1'0A'                                                 
         DC    XL3'FFFFFF',CL1' '      END OF TABLE                             
*                                                                               
         EJECT                                                                  
                                                                                
* - CHANGE RATE TYPE                                                            
                                                                                
RATYPE   NTR1                                                                   
         L     R2,NBAIO                                                         
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NUSDRD,R2                                                        
         MVC   NUSDSRT,NEWRATE                                                  
         MVC   NUSDRTCV,NEWRCOV                                                 
RATX     B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
APPCOS2  NTR1                                                                   
         L     R2,NBAIO                                                         
         CLI   0(R2),X'04'         ONLY APPLY TO UNITS                          
         BNE   APPCOS2N                                                         
*                                                                               
         MVC   SVPRD,NBSELPRD      SAVE AWAY PRODUCT                            
         MVC   SVEST,NBSELEST      AND ESTIMATE                                 
         ZAP   BILLTOT,=P'0'       ACCUMULATE BILL TOTALS                       
*                                                                               
         MVI   ELCODE,X'10'        IF BILLED, THEN SKIP COS2                    
         BAS   RE,GETEL                                                         
         BE    APPC20              TO CHECK IF UNBILLED                         
         B     APPC40              CHECK NEW BILLING RECORDS                    
*                                                                               
APPC10   BAS   RE,NEXTEL                                                        
         BNE   APPC30                                                           
*                                                                               
APPC20   ICM   RF,15,12(R2)        ACCUMULATE GROSS TOTALS                      
         CVD   RF,DUB                                                           
         AP    BILLTOT,DUB                                                      
         B     APPC10                                                           
*                                                                               
APPC30   CP    BILLTOT,=P'0'       $0 BILLING DOLLARS?                          
         BNE   APPCOS2N            NO - SKIP IT                                 
         B     APPC80                                                           
*                                                                               
APPC40   DS    0H                                                               
         CP    BILLDOL(8),=P'0'    THIS SHOULD BE FILLED IN BY                  
         BNE   APPCOS2N            NETBILLRDR (FROM NETIO)                      
*                                                                               
APPC80   DS    0H                                                               
         L     R2,NBAIO                                                         
*                                                                               
*****************************************************                           
*                                                   *                           
* GET CCOST2 AND ECOST2 FROM CLIENT/ESTIMATE RECORD *                           
*                                                   *                           
*****************************************************                           
         XC    COST2C,COST2C                                                    
         XC    COST2E,COST2E                                                    
         MVI   ICOST2C,C'N'        INIT INTEGRATION COST2 TO NO                 
         MVI   ICOST2E,C'N'                                                     
         XC    KEYSV,KEYSV                                                      
         MVC   KEYSV,0(R2)                                                      
*                                                                               
         MVC   SVEST,17(R2)        USE UNIT'S ESTIMATE VALUE                    
*                                                                               
         XC    KEY,KEY             GET CLIENT RECORD                            
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   KEY+1(3),1(R2)      AGY/MEDIA & CLIENT                           
         MVC   SAVEKEY(13),KEY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
         CLC   KEY(13),SAVEKEY                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL  ',KEY+14,AIO                  
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     RF,AIO                                                           
         USING CLTHDR,RF                                                        
*                                                                               
         MVC   COST2C,CCOST2       GET CLIENT COST2 FACTOR                      
         TM    COPT3,X'10'         DOES COST2 APPLY TO INTEGRATION              
         BZ    *+8                                                              
         MVI   ICOST2C,C'Y'                                                     
         DROP  RF                                                               
*                                                                               
         XC    KEY,KEY             GET ESTIMATE RECORD                          
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   KEY+1(3),1(RF)      AGY/MEDIA & CLIENT                           
         MVC   KEY+4(3),SVPRD      PRODUCT                                      
         CLC   =C'ALL',SVPRD                                                    
         BNE   *+10                                                             
         MVC   KEY+4(3),=C'POL'                                                 
         MVC   KEY+7(1),SVEST      ESTIMATE                                     
*                                                                               
         MVC   SAVEKEY(13),KEY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
         CLC   KEY(13),SAVEKEY                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL  ',KEY+14,AIO                  
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     RF,AIO                                                           
         USING ESTHDR,RF                                                        
*                                                                               
         MVC   COST2E,ECOST2       GET ESTIMATE COST2 FACTOR                    
         TM    EFLAG1,X'01'        DOES COST2 APPLY TO INTEGRATION              
         BZ    *+8                                                              
         MVI   ICOST2E,C'Y'                                                     
         DROP  RF                                                               
*                                                                               
APPC100  DS    0H                  APPLY COST2 TO UNIT                          
         XC    KEY,KEY                                                          
         MVC   KEY(20),KEYSV                                                    
         MVC   FILENAME,=C'UNTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   FILENAME,=C'UNTFIL  '                                            
         GOTO1 GETREC                                                           
*                                                                               
         L     R2,NBAIO                                                         
         MVC   UNITDATE,4(R2)       SAVE UNIT DATE                              
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING NUMAINEL,R2                                                      
*                                                                               
***         MVC   FULL,COST2C         MOVE CLIENT COST2                         
***         OC    COST2E,COST2E       IS THERE AN ESTIMATE COST2                
***         BZ    *+10                                                          
***         MVC   FULL,COST2E                                                   
***         OC    FULL,FULL           ANY COST2 FACTOR?                         
***         BZ    APPCOS2X            NO, EXIT                                  
*                                                                               
***         NI    NUUNITST,X'F7'                                                
***         XC    NUASSIGN,NUASSIGN                                             
***         TM    FULL,X'80'           WAS COST PCT SET TO ZERO                 
***         BZ    APPC120              NO CALCULATE                             
***         OI    NUUNITST,X'08'                                                
***         B     APPCOS2X                                                      
*                                                                               
***APPC120  DS    0H                                                            
***         ZAP   WORK(16),=PL1'0'                                              
***         ICM   R1,15,NUACTUAL       COST                                     
***         CVD   R1,WORK+8                                                     
***         ICM   R1,15,FULL           PERCENTAGE X.XXXXXX                      
***         CVD   R1,DUB                                                        
***         MP    WORK(16),DUB+4(4)    MULT COST BY PERCENTAGE                  
***         AP    WORK(16),=PL4'500000'  ROUND                                  
***         DP    WORK(16),=PL4'1000000'                                        
***         CVB   R1,WORK+4                                                     
***         STCM  R1,15,NUASSIGN                                                
***         OC    NUASSIGN,NUASSIGN                                             
***         BNZ   APPCOS2X                                                      
***         OI    NUUNITST,X'08'                                                
***         DROP  R2                                                            
*                                                                               
*  NEW COST2 CALCULATION INCLUDING THE INTEGRATION                              
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'UNTFILE '),(X'02',NBAIO),0                   
         CLI   12(R1),0            SET CC ON EXIT                               
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         TM    NUSDST3,X'80'        CHECK FOR OVERRIDE ASSIGNED COST            
         BO    XIT                                                              
         DROP  RE                                                               
*                                                                               
         MVI   BYTE,C'N'           INIT INTEG COS2 SWITCH                       
         MVC   FULL,COST2C         MOVE CLIENT PCT.                             
         MVC   BYTE,ICOST2C        INTEGRATION SWITCH                           
         OC    COST2E,COST2E       WAS ESTIMATE LEVEL COST INPUTTED             
         BZ    APPC120                                                          
         MVC   FULL,COST2E                                                      
         MVC   BYTE,ICOST2E                                                     
*                                                                               
APPC120  GOTO1 HELLO,DMCB,(C'D',=C'UNTFILE '),(X'74',NBAIO),0                   
         OC    FULL,FULL           WAS ANY COST PCT INPUTTED                    
         BZ    APPCOS2N            NO, EXIT                                     
*                                                                               
         NI    NUUNITST,X'F7'                                                   
         XC    NUASSIGN,NUASSIGN                                                
         TM    FULL,X'80'           WAS COST PCT SET TO ZERO                    
         BZ    APPC140              NO CALCULATE                                
         OI    NUUNITST,X'08'                                                   
         B     APPC260                                                          
*                                                                               
APPC140  ZAP   WORK(16),=PL1'0'                                                 
         ICM   R1,15,NUACTUAL       COST                                        
         CVD   R1,WORK+8                                                        
         ICM   R1,15,FULL           PERCENTAGE X.XXXXXX                         
         CVD   R1,DUB                                                           
         MP    WORK(16),DUB+4(4)    MULT COST BY PERCENTAGE                     
*  CHECK AGENCY'S FOR ALTERNATE ROUNDING                                        
         CLC   UNITDATE,=XL2'D47A'   APPLIES TO UNITS PRIOR MAR26/06            
         BH    APPC200                                                          
         LA    RE,COS2AGY                                                       
APPC160  CLI   0(RE),X'FF'                                                      
         BE    APPC200                                                          
         CLC   0(2,RE),NBSELAGY                                                 
         BE    APPC180                                                          
         LA    RE,2(RE)                                                         
         B     APPC160                                                          
APPC180  CLC   NUACTUAL,=F'2000'    DONT APPLY IF LESS THEN $20                 
         BL    XIT                                                              
         AP    WORK(16),=PL6'250000000'    ROUND                                
         DP    WORK(16),=PL6'500000000'                                         
         MP    WORK(10),=PL4'500'                                               
         CVB   R1,WORK+2                                                        
         B     APPC220                                                          
*   REGUALR ROUNDING                                                            
APPC200  AP    WORK(16),=PL4'500000'  ROUND                                     
         DP    WORK(16),=PL4'1000000'                                           
         CVB   R1,WORK+4                                                        
APPC220  STCM  R1,15,NUASSIGN                                                   
         OC    NUASSIGN,NUASSIGN                                                
         BNZ   APPC260                                                          
         OI    NUUNITST,X'08'                                                   
*                                                                               
* CALCULATE INTEGRATION ADJUSTMENT IF REQUESTED                                 
*                                                                               
APPC260  SR    R1,R1                CLEAR ACCUMULATOR                           
         CLI   BYTE,C'Y'            IS INTEGRATION COS2 SET                     
         BNE   APPC340              NO, BYPASS CALCULATION                      
         TM    FULL,X'80'           TEST FOR ZERO COS2 FACTOR                   
         BNZ   APPC340                                                          
*                                                                               
         ZAP   WORK(16),=PL1'0'                                                 
         ICM   R1,15,NUINTEG        INTEGRATION                                 
         CVD   R1,WORK+8                                                        
         ICM   R1,15,FULL           PERCENTAGE X.XXXXXX                         
         CVD   R1,DUB                                                           
         MP    WORK(16),DUB+4(4)    MULT COST BY PERCENTAGE                     
*                                                                               
*  CHECK AGENCY'S FOR ALTERNATE ROUNDING                                        
         CLC   UNITDATE,=XL2'D47A'   APPLIES TO UNITS PRIOR MAR26/06            
         BH    APPC320                                                          
         LA    RE,COS2AGY                                                       
APPC280  CLI   0(RE),X'FF'                                                      
         BE    APPC320                                                          
         CLC   0(2,RE),NBSELAGY                                                 
         BE    APPC300                                                          
         LA    RE,2(RE)                                                         
         B     APPC280                                                          
APPC300  AP    WORK(16),=PL6'250000000'    ROUND                                
         DP    WORK(16),=PL6'500000000'                                         
         MP    WORK(10),=PL4'500'                                               
         CVB   R1,WORK+2                                                        
         B     APPC340                                                          
*                                                                               
APPC320  AP    WORK(16),=PL4'500000'  ROUND                                     
         DP    WORK(16),=PL4'1000000'                                           
         CVB   R1,WORK+4                                                        
*                                                                               
*  BUILD COS2 ELEMENT                                                           
*                                                                               
APPC340  XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING NUCOS2D,R3                                                       
*                                                                               
         MVI   NUC2EL,X'74'                                                     
         MVI   NUC2LEN,11                                                       
         OI    NUC2STAT,NUC2ACT                                                 
         CLI   BYTE,C'Y'                                                        
         BNE   *+12                                                             
         OI    NUC2STAT,NUC2INT                                                 
         STCM  R1,15,NUC2AINT                                                   
         TM    FULL,X'80'           TEST FOR ZERO COS2 FACTOR                   
         BNZ   *+10                                                             
         MVC   NUC2FACT,FULL                                                    
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFILE '),(X'74',NBAIO),WORK,0              
         B     APPCOS2X                                                         
*                                                                               
APPCOS2X DS    0H                                                               
         ZAP   BILLDOL(8),=P'0'                                                 
         ZAP   BILLDOL+8(8),=P'0'                                               
         L     R2,NBAIO                                                         
         SR    RE,RE                                                            
         LTR   RE,RE                                                            
         B     XIT                                                              
* AGENCY'S USING ALTERNATE COS2 ROUNDING                                        
COS2AGY  DC    CL4'SJS$'                                                        
         DC    X'FF'                                                            
*                                                                               
APPCOS2N DS    0H                                                               
         ZAP   BILLDOL(8),=P'0'                                                 
         ZAP   BILLDOL+8(8),=P'0'                                               
         L     R2,NBAIO                                                         
         LTR   RE,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
                                                                                
* ERNCOST SITS ON X'15' ELEM WITH MISLEADING LABEL OF NUMCEIC                   
* THE CALCULATED ADJUSTED COST (EICADJ) GOES TO X'03' ELEM                      
*                                                                               
* CALCULATE EARNED COST = (PKGCPM X IMPS) IMPS=NDACTDEM+4                       
* IF EARNED COST ELEM EXISTS AND IS OVERRIDE/USE THIS VALUE                     
* IF EARNED COST ELEM EXISTS AND NO OVERRIDE/REPLACE VALUE                      
* IF NO EARNED COST ELEM / CREATE X'15' ELEM/CALCULATE EARNED COST              
*                                                                               
* THEN DEAL WITH X'03' ELELM, TYPE 'Q'                                          
* (EARNED COST-ACTUAL COST) = CALC EICADJ COST -> ON X'03' ELEM                 
* IF NO X'03' ELEM CREATE IT                                                    
* IF X'03' ELEM BUT NOT PAID/ REPLACE EICADJ VALUE (NUSPRAMT FIELD)             
* IF X'03' ELEM AND PAID/ THEN (CALC EICADJ-PAID EICADJ)= NEW EICADJ            
*         SET THE NEW EICADJ ON ITS OWN X'03' ELEM                              
ERNC     NTR1                                                                   
         XC    EICPAID,EICPAID                                                  
         XC    ELEM,ELEM                                                        
         L     R2,NBAIO                                                         
         USING NUMCSTD,R2                                                       
         MVI   ELCODE,X'15'                                                     
         BAS   RE,GETEL            EARNED COST ELEM?                            
         BNE   ERNC10              NO                                           
         TM    NUMCSTAT,X'80'      YES/EARNED COST OVERRIDDEN?                  
         BNO   ERNC15              NO/CALCULATE NEW ERNC                        
* USE EARNED COST OVERRIDE - SET UP FIELDS FOR PRINTING                         
         MVC   PAKIMPSV(3),=C'***' WE WON'T GET IMPS                            
         ICM   R1,15,NUMCEIC       OR EARNED COST                               
         CVD   R1,DUB                                                           
         ZAP   PAKEARN,DUB                                                      
         B     ERNC17                                                           
*                                                                               
ERNC10   LA    R2,ELEM             NO EARNED COST ELEM                          
         MVI   NUMCSEL,X'15'                                                    
         MVI   NUMCSLEN,NUMCSLNQ                                                
ERNC15   DS    0H                                                               
*                                                                               
* NETVALUE RETURNS RAW IMPS X10 TOO HIGH AND I CAN'T FIND WHERE                 
* SO FOR NOW LET ME HANDLE IT HERE GRRRRRRR&*%!                                 
                                                                                
* BUT LO! FIRST LET US SEE IF WE HAVE AN OVERRIDE                               
         B     ERNC15A             NETVALUE HANDLES OVERRIDES!!!!!!!!           
***      L     R3,NBAIO                                                         
***      MVI   WORK,0              X'00'+3CHAR DEMO IN ELEM                     
***      MVC   WORK+1(3),PKGDEM3                                                
***      GOTO1 HELLO,DMCB,(C'G',=C'UNTFILE '),(X'DE',(R3)),(4,WORK)             
***      CLI   DMCB+12,0                                                        
***      BNE   ERNC15A                                                          
***      MVI   NBRESULT,C'*'       FOR PRINTING LATER                           
***      L     RE,DMCB+12                                                       
***      USING NUOVD,RE                                                         
***      ICM   R1,15,NUOVVAL                                                    
***      SR    R0,R0                                                            
***      M     R0,=F'1000'         TO KEEP EXPDEM PRECISION                     
***      B     ERNC16                                                           
***      DROP  RE                                                               
ERNC15A  LA    RE,NDDEMOS          FIND REQUIRED DEMO IN LIST                   
         LA    RF,NDACTDEM+4                                                    
******** ZIC   R3,15,NDNDEMOS                                                   
         ZIC   R3,NDNDEMOS                                                      
         LTR   R3,R3                                                            
         BNZ   ERNC15B                                                          
         BNZ   *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
* FIND DEMO FROM PKG REC IN NDDEMOS LIST                                        
ERNC15B  CLC   PKGDEM3(1),0(RE)    MATCH ON 1ST AND 3D                          
         BNE   ERNC15C                                                          
         CLC   PKGDEM3+2(1),2(RE)                                               
         BE    ERNC15D                                                          
ERNC15C  LA    RE,3(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R3,ERNC15B                                                       
         MVC   P+1(43),=C'** ERROR PKG CPM DEMO       NOT ON ESTIMATE'          
         MVC   P+22(6),PKGDEM                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   BYT1,X'FF'          SET ERROR                                    
         B     ERNCX                                                            
         DC    H'0'                                                             
ERNC15D  ICM   R1,15,0(RF)         IMP OF TARGET DEMO                           
         BZ    ERNC16                                                           
****     BAS   RE,CHKRESLT         CHECK WE HAVE ACTUAL DEMOS                   
****     BNE   ERNC16              NO/ROUTINE RETURNS VALUE IN R1               
         SR    R0,R0               YES                                          
         D     R0,=F'10'           SEE GRRRRR NOTE ABOVE                        
*                                                                               
ERNC16   CVD   R1,DUB                                                           
         MVC   PAKIMPSV,DUB        SAVE IMP FOR PRINTING                        
         ZAP   PWORK16,DUB                                                      
         ICM   R3,15,PKGCPM                                                     
         CVD   R3,DUB                                                           
         ZAP   PWORK8,DUB                                                       
         MP    PWORK16,PWORK8      X PACKAGE CPM                                
         AP    PWORK16,=P'500'          ROUND UP                                
         DP    PWORK16,=P'1000'                                                 
         MVC   PAKEARN,PWORK16+5   SAVE FOR PRINTING                            
         ZAP   DUB,PWORK16+5(8)                                                 
         CVB   R1,DUB                                                           
         STCM  R1,15,NUMCEIC                                                    
         CLI   TESTRUN,C'Y'                                                     
         BE    ERNC17                                                           
         CLI   ELEM,0              DID WE CREATE NEW X'15' ELEM?                
         BE    ERNC17              NO                                           
         GOTO1 =V(HELLO),DMCB,(C'P',=C'UNTFILE '),NBAIO,ELEM,0                  
         EJECT                                                                  
ERNC17   DS    0H                                                               
         MVC   DUB(4),NUMCEIC      SAVE EARNED COST                             
         DROP  R2                                                               
                                                                                
* NEW EARNED COST OR OVERRIDEN EARNED COST SITS IN DUB(4)                       
         L     R2,NBAIO            DO WE HAVE 'Q' 03 ELEM?                      
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERNC17C             NO                                           
ERNC17B  DS    0H                                                               
         USING NUSPRD,R2                                                        
         CLI   NUSPRTYP,C'Q'       IS IT A 'Q' 03?                              
         BE    ERNC17D             YES                                          
         BAS   RE,NEXTEL                                                        
         BE    ERNC17B                                                          
         B     ERNC17C             NO 'Q' 03 FOUND-CREATE ONE                   
*                                                                               
* CALCULATE EICADJ (EARNED-ACTUAL=EICADJ) / CREATE X'03'                        
ERNC17C  ICM   R3,15,NBACTUAL      ACTUAL COST TO R3                            
         ICM   R1,15,DUB           EARNED COST TO R1                            
         SR    R1,R3               R1 GETS EICADJ COST                          
         B     ERNC19                                                           
                                                                                
* 'Q' 03 FOUND                                                                  
* GO THROUGH AND ADD UP ANY PAID 'Q'                                            
* THEN FIND IF ANY UNPAID 'Q'                                                   
ERNC17D  DS    0H                                                               
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
ERNC17F  CLI   NUSPRTYP,C'Q'                                                    
         BNE   ERNC17H                                                          
         TM    NUSPRSTA,X'40'      PAID?                                        
         BNO   ERNC17H                                                          
         ICM   R1,15,NUSPRAMT      YES / ADD TO EIC TOTAL                       
         L     R3,EICPAID                                                       
         AR    R3,R1                                                            
         ST    R3,EICPAID                                                       
ERNC17H  BAS   RE,NEXTEL                                                        
         BE    ERNC17F                                                          
* NOW SEE IF ANY 'Q' UNPAID                                                     
         L     R2,NBAIO                                                         
         BAS   RE,GETEL                                                         
ERNC17I  CLI   NUSPRTYP,C'Q'                                                    
         BNE   ERNC17K                                                          
         TM    NUSPRSTA,X'40'      PAID?                                        
         BNO   ERNC18              NO                                           
ERNC17K  BAS   RE,NEXTEL           YES/CONTINUE SEARCH                          
         BE    ERNC17I                                                          
         ICM   R3,15,NBACTUAL      ACTUAL COST TO R3                            
         ICM   R1,15,DUB           EARNED COST TO R1                            
         SR    R1,R3               R1 GETS EICADJ COST                          
         B     ERNC19             NO NON-PAID 'Q' 03 FOUND/CREATE ONE           
                                                                                
         EJECT                                                                  
*                                                                               
* R2 -> UNPAID 'Q' ELEM - CALCULATE AND REPLACE NUSPRAMT                        
* CALCULATE NEW EICADJ (EARNED-ACTUAL)                                          
* THEN NEW EICADJ-PAID EICADJ = NEW NUSPRAMT FOR X'03' ELEM                     
ERNC18   DS    0H                                                               
         ICM   R1,15,DUB         EARNED COST IN DUB                             
         ICM   R3,15,NBACTUAL                                                   
         SR    R1,R3             PUTS EICADJ IN R1                              
         L     R3,EICPAID        PAID EIC AMOUNT                                
         SR    R1,R3             NEW AMOUNT - PAID AMOUNT                       
         STCM  R1,15,NUSPRAMT    SET IN ELEM                                    
         B     ERNC20            THAT'S ALL                                     
                                                                                
* EXPECTS EICADJ IN R1 AND CREATES/ADDS 'Q' 03 ELEM                             
ERNC19   DS    0H                                                               
         LA    R2,ELEM                                                          
         USING NUSPRD,R2                                                        
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'03'                                                       
         MVI   ELEM+1,20           FIXED LENGTH                                 
         MVI   ELEM+3,C'Q'         'Q' EICADJ                                   
         MVI   NUSPRCOM,C'Y'       COMMISSION                                   
         L     R3,EICPAID          PAID EIC AMOUNT                              
         SR    R1,R3               NEW AMOUNT - PAID AMOUNT                     
         STCM  R1,15,NUSPRAMT                                                   
         CLI   TESTRUN,C'Y'                                                     
         BE    ERNC20                                                           
         GOTO1 =V(HELLO),DMCB,(C'P',=C'UNTFILE '),NBAIO,ELEM,0                  
         B     ERNC20                                                           
                                                                                
* - PRINT OUT DETAILS                                                           
ERNC20   MVC   P+1(3),NBCLICOD                                                  
         EDIT  (B1,NBACTEST),(3,P+5)                                            
         EDIT  (B1,NBLEN),(3,P+9)                                               
***      MVC   P+9(4),NBACTNET                                                  
         EDIT  (B1,NBPACK),(3,P+14)                                             
         MVC   P+18(6),NBACTPRG                                                 
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(10,P+25)                               
         CLC   =C'***',PAKIMPSV    EARNED COST OVERRIDEN?                       
         BE     ERNC25                                                          
         EDIT  (P8,PAKIMPSV),(8,P+36)    NO                                     
         B     *+10                                                             
ERNC25   MVC   P+36(3),=C'***'            YES/SO NO IMPS                        
         MVC   P+44(1),NBRESULT                                                 
         EDIT  (B4,PKGCPM),(7,P+45),2,MINUS=YES                                 
         EDIT  (P8,PAKEARN),(11,P+54),2,MINUS=YES                               
         EDIT  (B4,NBACTUAL),(11,P+67),2,MINUS=YES                              
         EDIT  (B4,NBMAXCST),(11,P+80),2,MINUS=YES                              
         ZAP   DUB,PAKEARN    CALCULATE EICADJ FOR PRINTING                     
         CVB   R4,DUB               EARNED-ACTUAL=EICADJ                        
         ICM   R3,15,NBACTUAL                                                   
         SR    R4,R3                                                            
         EDIT  (R4),(11,P+92),2,MINUS=YES                                       
         EDIT  (B4,EICPAID),(11,P+104),2,MINUS=YES                              
         EDIT  (B4,NUSPRAMT),(11,P+116),2,MINUS=YES                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
ERNCX    B     XIT                                                              
         DROP  R2                                                               
                                                                                
*                                                                               
* CHECK NBRESULT TO SEE IF NETVALUE RETURNED ACTUAL DEMOS                       
* R1 CONTAINS DEMO VALUE                                                        
*                                                                               
**CHKRESLT NTR1                                                                 
**         LA    RE,RESULTAB       RESULT TABLE OF NON-ACTUAL VALUES            
**CHKR10   CLI   0(RE),X'FF'                                                    
**         BE    XIT                 EOF-DON'T SAVE R1                          
**         CLC   0(1,RE),NBRESULT                                               
**         BE    *+12                                                           
**         LA    RE,1(RE)                                                       
**         B     CHKR10                                                         
**         SR    R0,R0               ASSUME NOT REAL ACTUAL                     
**         M     R0,=F'100'          GET TO EXPDEM PRECISION                    
**         LTR   RE,RE                                                          
*8         XIT1  REGS=(R1)                                                      
**RESULTAB DC    C'ELZYBW'                                                      
**         DC    X'FF'                                                          
*                                                                               
PWORK16  DS    PL16                                                             
PWORK8   DS    PL8                                                              
PAKIMPSV DS    CL8                                                              
PAKEARN  DS    CL8                                                              
         EJECT                                                                  
*                                                                               
         GETEL (R2),NBDTADSP,ELCODE                                             
*                                                                               
         EJECT                                                                  
HEADSPC  SSPEC H1,1,REQUESTOR                                                   
         SSPEC H1,46,C'NETWORK UTILITY PROGRAM'                                 
         SSPEC H2,46,C'________________________'                                
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,1,C'CLIENT'                                                   
         SSPEC H4,1,C'PRODUCT'                                                  
         SSPEC H5,1,C'ESTIMATE'                                                 
         SSPEC H6,1,C'NETWORK'                                                  
         SSPEC H4,46,PERIOD                                                     
         SSPEC H4,99,REPORT                                                     
         SSPEC H5,99,RUN                                                        
         SSPEC H6,120,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
HOOK     NTR1                                                                   
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
*                                                                               
         LA    R1,H3                                                            
         MVC   H3+10(5),SPLCLI                                                  
         LA    R1,H4                                                            
         MVC   H4+10(5),SPLPRO                                                  
         LA    R1,H5                                                            
         MVC   H5+10(8),SPLEST                                                  
         LA    R1,H6                                                            
         MVC   H6+10(8),SPLNET                                                  
*                                                                               
         TM    OPTTYP3,OP3DEMOQ    DEMOLOCK?                                    
         BZ    *+12                                                             
         BRAS  RE,DLHOOK                                                        
         B     HOOKX                                                            
*                                                                               
         TM    OPTTYPE,X'80'       ERNC?                                        
         BNO   HOOKX                                                            
         MVI   H7,0                                                             
         MVC   H8+1(3),=C'CLI'                                                  
         MVC   H9+1(3),=C'------------'                                         
         MVC   H8+5(3),=C'EST'                                                  
         MVC   H9+5(3),=C'------------'                                         
         MVC   H8+9(3),=C'LEN'                                                  
         MVC   H9+9(3),=C'------------'                                         
         MVC   H8+14(3),=C'PKG'                                                 
         MVC   H9+14(3),=C'------------'                                        
         MVC   H8+18(4),=C'PROG'                                                
         MVC   H9+18(4),=C'------------'                                        
         MVC   H8+25(4),=C'DATE'                                                
         MVC   H9+25(4),=C'------------'                                        
         MVC   H8+37(4),=C'IMPS'                                                
         MVC   H9+37(4),=C'------------'                                        
         MVC   H8+46(6),=C'PKGCPM'                                              
         MVC   H9+46(6),=C'------------'                                        
         MVC   H8+55(11),=C'EARNED COST'                                        
         MVC   H9+55(11),=C'------------'                                       
         MVC   H8+67(11),=C'ACTUAL COST'                                        
         MVC   H9+67(11),=C'------------'                                       
         MVC   H8+80(11),=C'  MAX  COST'                                        
         MVC   H9+80(11),=C'------------'                                       
         MVC   H8+92(11),=C'  ERCADJ   '                                        
         MVC   H9+92(11),=C'------------'                                       
         MVC   H8+104(11),=C'PAID ERCADJ'                                       
         MVC   H9+104(11),=C'------------'                                      
         MVC   H8+116(11),=C'BLBL ERCADJ'                                       
         MVC   H9+116(11),=C'------------'                                      
         MVC   H10+1(11),=C'TARGET DEMO'                                        
         MVC   H10+13(7),PKGDEM                                                 
HOOKX    B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
TMEXRTN  NTR1                                                                   
*                                                                               
         CLI   NBMODE,NBPROCUN                                                  
         BE    TMEX100                                                          
         CLI   NBMODE,NBPROCPK                                                  
         BNE   TMEXBD                                                           
         L     R3,NBAIO                                                         
         USING NPRECD,R3                                                        
*                                                                               
*  GET TMEX PCT                                                                 
         ZAP   TOTTMEX,=PL1'0'                                                  
         ICM   R1,15,NPAKCOST       PACKAGE COST                                
         CVD   R1,DUB                                                           
         ZAP   WORK(14),=PL1'0'                                                 
         ICM   R1,15,TMEXCST        TMEX CST                                    
         CVD   R1,WORK+6                                                        
         MP    WORK(14),=PL4'1000000'                                           
         DP    WORK(14),DUB                                                     
         AP    WORK(6),=PL2'50'                                                 
         DP    WORK(6),=PL2'100'                                                
         MVC   TMEXPCT,WORK                                                     
*                                                                               
*  MOVE TMEX COST TO PACKAGE RECORD                                             
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPK2D,R2                                                         
         MVC   NPAKTMEX,TMEXCST                                                 
         B     TMEXGD                                                           
         DROP  R2,R3                                                            
*                                                                               
* ADJUST ACTUAL COST USING TMEXPCT MOVE INTO ASSIGNED COST                      
*                                                                               
TMEX100  L     R3,NBAIO                                                         
         USING NURECD,R3                                                        
*                                                                               
*  INITIALIZE THE SETTINGS                                                      
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUSDRD,R2                                                        
*                                                                               
         XC    NUASSIGN,NUASSIGN                                                
         NI    NUUNITST,X'FF'-NUUNIGCI   TURN OFF ASS COST OVERRIDE             
         NI    NUSDST3,X'FF'-X'80'       TURN OFF ASSIGNED COST                 
*                                                                               
* BYPASS PREEMPTS, MISSED UNITS, AND UNITS WITH NO ACTUAL COST                  
         TM    NUUNITST,NUUNIPRE                                                
         BO    TMEXBD                                                           
*                                                                               
         OC    NUACTUAL,NUACTUAL                                                
         BZ    TMEXBD                                                           
*                                                                               
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'07'         IS UNIT MISSED                              
         BAS   RE,GETEL                                                         
         BE    TMEXBD                                                           
*                                                                               
* ADJUST UNIT                                                                   
         MVC   LASTREC,NUKEY        SAVE LAST RECORD KEY                        
*                                                                               
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUSDRD,R2                                                        
*                                                                               
         ZAP   WORK(16),=PL1'0'                                                 
         ICM   R1,15,NUACTUAL       ACTUAL COST                                 
         CVD   R1,WORK+8                                                        
         MP    WORK(16),TMEXPCT                                                 
         AP    WORK(16),=PL4'500000' ROUND TO NEAREST DOLLAR                    
         DP    WORK(16),=PL4'1000000'                                           
         MP    WORK(12),=PL2'100'    GET TO WHOLE DOLLARS                       
*                                                                               
*  UPDATE THE UNIT RECORD                                                       
         AP    TOTTMEX(8),WORK+4(8)   ACCUM TMEX DOLLARS                        
         CVB   R1,WORK+4                                                        
         STCM  R1,15,NUASSIGN                                                   
         OI    NUUNITST,NUUNIGCI                                                
         OI    NUSDST3,X'80'          SET OVERRIDE ASSIGNED COST                
TMEXGD   SR    R3,R3                SET GOOD RETURN CODE                        
TMEXBD   LTR   R3,R3                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*                                                                               
* - UNLOCK UNITS/PKGS                                                           
UNLOCKU  NTR1                                                                   
         CLI   NBMODE,NBPROCUN                                                  
         BNE   UNLK10                                                           
         L     R2,NBAIO                                                         
         USING NUMAINEL,R2                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    NUPACKST,X'FF'-X'20'      UNLOCK IT                              
         B     UNLX                                                             
UNLK10   CLI   NBMODE,NBPROCPK                                                  
         BNE   UNLX                                                             
         L     R2,NBAIO                                                         
         USING NPAKEL,R2                                                        
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    NPAKSTAT,X'FF'-X'20'      UNLOCK IT                              
UNLX     B     XIT                                                              
         DROP R2                                                                
         EJECT                                                                  
* LOCK UNIT/PACKAGE RECS                                                        
UNITLOCK NTR1                                                                   
*                                                                               
UNIT00   CLI   NBMODE,NBPROCUN                                                  
         BNE   UNIT10                                                           
         L     R2,NBAIO                                                         
         USING NUMAINEL,R2                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    NUPACKST,X'20'      LOCK IT                                      
         B     UNIYES                                                           
UNIT10   CLI   NBMODE,NBPROCPK                                                  
         BNE   UNINO                                                            
         L     R2,NBAIO                                                         
         USING NPAKEL,R2                                                        
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    NPAKSTAT,X'20'      LOCK IT                                      
         B     UNIYES                                                           
UNIYES   SR    RE,RE                                                            
UNINO    LTR   RE,RE                                                            
UNIX     B     XIT                                                              
         DROP  R2                                                               
                                                                                
*                                                                               
* ADD PKGFILTERS                                                                
* DELETE CURRENT PKG FILTER ELEM AND ADD NEW ONE                                
PKGFLTR  NTR1                                                                   
         GOTO1 HELLO,DMCB,(C'D',=C'UNTFILE '),(X'08',NBAIO),=C'K',0             
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'08'                                                       
         MVI   ELEM+1,11                                                        
         MVI   ELEM+2,C'K'                                                      
         MVC   ELEM+3(6),PKGFLTRS                                               
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFILE '),NBAIO,ELEM,0                      
PKGX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
* APPLY TRADE %                                                                 
*                                                                               
TRADEPCT NTR1                                                                   
         CLI   NBMODE,NBPROCUN     PROCESSING UNIT?                             
         BE    TRADEP20                                                         
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'UNTFILE '),(X'02',NBAIO),0                   
         CLI   DMCB+12,0                                                        
         BNE   XIT                                                              
         L     R1,DMCB+12                                                       
         USING NPK2D,R1                                                         
         MVC   NPK2TRAD,TRDPCT     TRADE %                                      
         B     XIT                                                              
*                                                                               
TRADEP20 GOTO1 HELLO,DMCB,(C'G',=C'UNTFILE '),(X'18',NBAIO),0                   
         CLI   DMCB+12,0                                                        
         BE    TRADEP30                                                         
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING NUDTAD,R1                                                        
         MVC   WORK(2),=X'1832'                                                 
         MVC   NUDTTRAD,TRDPCT     TRADE %                                      
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFILE '),(X'18',NBAIO),WORK,0              
         B     XIT                                                              
         DROP  R1                                                               
*                                                                               
TRADEP30 L     R1,DMCB+12                                                       
         USING NUDTAD,R1                                                        
         MVC   NUDTTRAD,TRDPCT     TRADE %                                      
         B     XIT                                                              
         DROP  R1                                                               
*                                                                               
* APPLY CASH %                                                                  
*                                                                               
CASHPCTG NTR1                                                                   
         CLI   NBMODE,NBPROCUN     PROCESSING UNIT?                             
         BE    CASHP20                                                          
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'UNTFILE '),(X'02',NBAIO),0                   
         CLI   DMCB+12,0                                                        
         BNE   XIT                                                              
         L     R1,DMCB+12                                                       
         USING NPK2D,R1                                                         
         MVC   NPK2CASH,CASHPCT    CASH %                                       
         B     XIT                                                              
         DROP  R1                                                               
*                                                                               
CASHP20  GOTO1 HELLO,DMCB,(C'G',=C'UNTFILE '),(X'12',NBAIO),(1,=C'T')           
         CLI   DMCB+12,0                                                        
         BE    XIT                 IF TIME PAID EXIT                            
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'UNTFILE '),(X'18',NBAIO),0                   
         CLI   DMCB+12,0                                                        
         BE    CASHP30                                                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING NUDTAD,R1                                                        
         MVC   WORK(2),=X'1832'                                                 
         MVC   NUDTCASH,CASHPCT    CASH %                                       
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFILE '),(X'18',NBAIO),WORK,0              
         B     CASHP40                                                          
         DROP  R1                                                               
*                                                                               
CASHP30  L     R1,DMCB+12                                                       
         USING NUDTAD,R1                                                        
         MVC   NUDTCASH,CASHPCT    CASH %                                       
         B     CASHP40                                                          
         DROP  R1                                                               
*                                                                               
CASHP40  GOTO1 =V(CALCASHP),DMCB,NBAIO,ACOMFACS,CLICPRD                         
         B     XIT                                                              
         EJECT                                                                  
* SET SPECIAL REP CODES                                                         
SPECREP  NTR1                                                                   
*                                                                               
SPEC00   CLI   NBMODE,NBPROCUN                                                  
         BNE   SPEC10                                                           
*                                                                               
         L     R2,NBAIO               SKIP PAID UNITS                           
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BE    SPECEX                                                           
*                                                                               
         L     R2,NBAIO                                                         
         USING NUMAINEL,R2                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NUSREP,SREPCODE     SPECIAL REP                                  
         B     SPECEX                                                           
SPEC10   CLI   NBMODE,NBPROCPK                                                  
         BNE   SPECEX                                                           
         L     R2,NBAIO                                                         
         USING NPAKEL,R2                                                        
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NPAKSREP,SREPCODE   SPECIAL REP                                  
SPECEX   B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
* TURN ON AUDIT FLAG ONUNITS                                                    
AUDITRTN NTR1                                                                   
         CLI   PKG09SV,0          DO WE HAVE GROUP/COMMENTS?                    
         BNE  *+6                                                               
         DC    H'0'                SHOULD NEVER GET HERE                        
         CLI   NBMODE,NBPROCPK                                                  
         BNE   AUDIT20                                                          
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPAKEL,R2                                                        
         OI    NPAKSTAT,X'02'                                                   
         B     XIT                                                              
         DROP  R2                                                               
* DELETE IT IF ALREADY THERE                                                    
AUDIT20  GOTO1 HELLO,DMCB,(C'D',=C'UNTFILE '),(X'09',NBAIO),0                   
* ADD THIS ONE                                                                  
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFILE '),NBAIO,PKG09SV,0                   
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,NBAIO                                                         
         USING NUMAINEL,R2                                                      
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    NUPACKST,X'02'      AUDIT IS ON                                  
         B     XIT                                                              
         DROP  R2                                                               
*                                                                               
* PROCESS V TYPES                                                               
VTYPERTN NTR1                                                                   
         CLI   NBMODE,NBPROCUN     PROCESSING UNIT?                             
         BE    VTYPE20                                                          
         CLI   NBMODE,NBPROCPK                                                  
         BNE   XIT                                                              
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPK2D,R2                                                         
         MVC   NPK2VTYP,NEWVTYP                                                 
         MVC   NPK2MPOD,NEWVTYP+2                                               
         B     XIT                                                              
         DROP  R2                                                               
*                                                                               
VTYPE20  L     R2,NBAIO                                                         
         MVI   ELCODE,X'18'                                                     
         BAS   RE,GETEL                                                         
         BE    VTYPE30                                                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING NUDTAD,R1                                                        
         MVC   WORK(2),=X'1832'                                                 
         MVC   NUDVTYPE,NEWVTYP                                                 
         CLI   NEWVTYP+2,C'P'                                                   
         BNE   *+8                                                              
         OI    NUUNST5,X'20'                                                    
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFILE '),(X'18',NBAIO),WORK,0              
         B     XIT                                                              
         DROP  R1                                                               
*                                                                               
VTYPE30  DS    0H                                                               
         USING NUDTAD,R2                                                        
         MVC   NUDVTYPE,NEWVTYP                                                 
         NI    NUUNST5,X'DF'                                                    
         CLI   NEWVTYP+2,C'P'                                                   
         BNE   *+8                                                              
         OI    NUUNST5,X'20'                                                    
         B     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
*                                                                               
I2BUG    NTR1                                                                   
         L     R2,NBAIO                                                         
         USING NUMAINEL,R2                                                      
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                                                         
         BNE   I2BUGNO                                                          
         OC    NBSDAFDT,NBSDAFDT                                                
         BNZ   I2BUGNO                                                          
         GOTO1 HELLO,DMCB,(C'D',=C'UNTFILE '),(X'24',NBAIO),0                   
         SR    RE,RE                                                            
         B     I2BUGX                                                           
I2BUGNO  MVI   NBUPUNIT,C'N'           SET NO-WRITE SWITCH                      
         MVI   NBNOWRIT,C'N'                                                    
I2BUGX   LTR   RE,RE                   SET NOT EQUAL CONDITION CODE             
         B     XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
PGESTRTN NTR1                                                                   
* READ PRODUCT RECS AND TEST THAT EACH PROD REC HAS                             
* A PGEST 0 ESTIMATE REC SET UP.                                                
* AND THAT EACH ESTIMATE HAS A PGEST PROD=POL SET UP                            
*                                                                               
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),NBKEY+11                                                
         MVI   KEY+6,1                                                          
PGEST10  GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PGEST20                                                          
         CLI   KEY+7,0                 MAKE SURE PROD REC                       
         BNE   PGESTX                                                           
         MVC   KEYSV,KEY             SAVE PROD KEY                              
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D5D'                                                  
         MVC   KEY+2(3),KEYSV+1      SET AM/CLT                                 
         MVC   KEY+6(3),KEYSV+4        SET PRD                                  
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE          NEEDS 0 EST                              
         BE    *+8                                                              
         BAS   RE,PGPRINT                                                       
         MVC   KEY,KEYSV             RESET PRODUCT KEY                          
         MVC   KEY+7(2),=X'FFFF'       BUMP TO NEXT PROD                        
         B     PGEST10                                                          
*                                                                               
PGEST20  XC    KEY,KEY             *NOW READ THROUGH ESTIMATES                  
         LA    R2,KEY                                                           
         USING EKEY,R2                                                          
         MVC   KEY+1(3),NBKEY+11                                                
         MVI   EKEYPRD+2,1             GET 1ST PROD                             
PGEST24  GOTO1 HIGH                                                             
PGEST25  CLC   KEY(4),KEYSAVE                                                   
         BNE   PGESTX                                                           
         CLI   EKEYEST,0                                                        
         BNE   PGEST30                                                          
         GOTO1 SEQ                                                              
         B     PGEST25                                                          
PGEST30  MVC   KEYSV,KEY                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D5D'                                                  
         MVC   KEY+2(3),NBKEY+11                                                
         MVC   KEY+5(1),KEYSV+7        SET ESTIMATE                             
         MVC   KEY+6(3),=C'POL'        FOR POL                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+8                                                              
         BAS   RE,PG2PRINT                                                      
         MVC   KEY,KEYSV                                                        
         MVI   EKEYEST+1,X'FF'         BUMP TO NEXT ESTIMATE                    
         B     PGEST24                                                          
PGESTX   MVI   NBDATA,C'P'             CAN WE FOOL NETIO?                       
         MVI   NBKEY,X'FF'             NO MORE RECS                             
         XIT1                                                                   
*                                                                               
PGPRINT  NTR1                                                                   
         MVC   P+1(3),KEYSV+4             PRINT IT                              
         MVC   P+5(21),=C'** NO ZERO EST RECORD'                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
*                                                                               
ESTBL    DS    CL257                                                            
*                                                                               
PG2PRINT NTR1                                                                   
         LA    R1,ESTBL                                                         
PGP05    CLI   0(R1),0                                                          
         BE    PGP20                                                            
PGP10    CLC   KEYSV+7(1),0(R1)        DID WE DO THIS ONE?                      
         BE    PGPX                                                             
         LA    R1,1(R1)                                                         
         B     PGP05                                                            
PGP20    MVC   0(1,R1),KEYSV+7         NO/ADD IT TO LIST                        
         MVC   P+1(3),KEYSV+4          SET PRODUCT CODE                         
         EDIT  (B1,KEYSV+7),(3,P+5)   SET EST NUMBER                            
         MVC   P+10(20),=C'** NO POL EST RECORD'                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
PGPX     XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
PRTDLOCK NTR1  BASE=*,LABEL=*                                                   
         LA    R3,P                DEMOLOCK                                     
         USING PLINED,R3                                                        
         MVC   P(PLINEDQ),SPACES                                                
         MVC   PCLI(3),NBCLICOD    CLIENT CODE                                  
         MVC   PPROG(6),NBACTPRG   PROGRAM                                      
         MVC   PNET(4),NBACTNET    NETWORK                                      
         ZIC   R4,NBACTEST         ESTIMATE                                     
         EDIT  (R4),(3,PEST),ALIGN=LEFT                                         
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(8,PDATE)  UNIT DATE                    
         EDIT  (B1,NBACTSUB),(3,PSUB),ALIGN=LEFT                                
         EDIT  (B1,NBPACK),(3,PPACK),ALIGN=LEFT                                 
         J     XIT                                                              
         DROP  R3                                                               
         LTORG                                                                  
*                                                                               
DLHOOK   NTR1  BASE=*,LABEL=*                                                   
         MVI   H7,0                                                             
         LA    RF,H8                                                            
         USING PLINED,RF                                                        
         MVC   PCLI,=C'CLIENT'                                                  
         MVC   PPROG,=C'PROGRAM'                                                
         MVC   PNET,=C'NETWORK'                                                 
         MVC   PEST,=C'ESTIMATE'                                                
         MVC   PDATE,=C'UNIT DATE'                                              
         MVC   PSUB,=C'SUBLINE'                                                 
         MVC   PPACK,=C'PACKAGE'                                                
         LA    RF,H9                                                            
         MVC   PCLI,=C'------'                                                  
         MVC   PPROG,=C'-------'                                                
         MVC   PNET,=C'-------'                                                 
         MVC   PEST,=C'--------'                                                
         MVC   PDATE,=C'---------'                                              
         MVC   PSUB,=C'-------'                                                 
         MVC   PPACK,=C'-------'                                                
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
* DEMOLOCK                                                                      
* SAVE ESTIMAED DEMO OVERRIDES                                                  
*                                                                               
DEMOLOCK NTR1  BASE=*,LABEL=*                                                   
         NI    MYFLAG,X'FF'-ADDLKDEM                                            
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'UNTFILE '),(X'18',NBAIO),0                   
         CLI   DMCB+12,0                                                        
         JNE   DL05                                                             
         L     RF,DMCB+12                                                       
         USING NUDTAD,RF                                                        
         OI    NUUNST5,NST5DLCK    MARK UNIT DEMOLOCKED                         
         DROP  RF                                                               
*                                                                               
* REMOVE DEMO LOCKED ELEMENTS                                                   
*                                                                               
DL05     GOTO1 HELLO,DMCB,(C'D',=C'UNTFILE '),(X'D4',NBAIO),0                   
*                                                                               
         L     R3,NBAIO                                                         
         AHI   R3,27               POINT TO FIRST ELEMENT                       
         L     R4,AIO2                                                          
         MVI   0(R4),X'FF'                                                      
*                                                                               
DL10     CLI   0(R3),0             END OF RECORD?                               
         JE    DL40                                                             
         CLI   0(R3),NUOVDDQ       DD ELEMENT?                                  
         JNE   DL30                                                             
*                                                                               
DL20     ZIC   R5,1(R3)            SAVE IT                                      
         SHI   R5,1                                                             
         EX    R5,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         MVI   0(R4),NUOVD4Q       SET ELEMENT ID                               
*                                                                               
         ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         MVI   0(R4),X'FF'                                                      
*                                                                               
DL30     ZIC   RF,1(R3)            GET NEXT ELEMENT                             
         AR    R3,RF                                                            
         J     DL10                                                             
*                                                                               
DL40     L     R4,AIO2             PUT DEMO LOCKED ELEMENTS ON UNIT             
DL45     CLI   0(R4),X'FF'                                                      
         JE    DLYES                                                            
         XC    ELEM,ELEM                                                        
         ZIC   R5,1(R4)                                                         
         SHI   R5,1                                                             
         EX    R5,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),0(R4)                                                    
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'UNTFILE '),NBAIO,ELEM,0                  
         CLI   DMCB+12,0                                                        
         JNE   *+2                                                              
*                                                                               
         OI    MYFLAG,ADDLKDEM     ADDED LOCKED DEMO ELEMENT                    
         ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         J     DL45                                                             
*                                                                               
DLYES    SR    RE,RE                                                            
DLNO     LTR   RE,RE                                                            
DLX      J     XIT                                                              
                                                                                
         EJECT                                                                  
* LOCKER FOR UPDATIVE SOON                                                      
         SPACE                                                                  
LOCKEM   NTR1  BASE=*,LABEL=*                                                   
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
         CLC   =C'SOON',CONWHEN    IS IT SOON                                   
         BNE   LOCKX               NO/FORGET IT                                 
         TM    NBINDS6,NBI6SOX     READ ONLY?                                   
         BZ    LOCK00                                                           
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'*** SYSTEM NOT FOR UPDATE       '                 
         GOTO1 ERREX2                                                           
*                                                                               
LOCK00   DS    0H                                                               
*        CLC   =C'SJ',NBSELAGY     ONLY FOR SJR                                 
*        BNE   INVERROR                                                         
                                                                                
         CLI   NBSELCLI,X'40'                                                   
         BE    *+14                                                             
         CLC   =C'ALL',NBSELCLI                                                 
         BNE   LOCK0                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'*** UPDATIVE SOON FOR ONE CLIENT'                 
         GOTO1 ERREX2                                                           
* - GET SE NUMBER                                                               
LOCK0    GOTO1 GETFACT,DMCB,(2,0)                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           MUST BE NET                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,FASYS          GET SE NUMBER                                
         DROP  R1                                                               
* - LOCK / UNLOCK                                                               
         CLI   LOCKSW,C'L'         IF LOCKING                                   
         BNE   *+8                                                              
         MVI   TWAWHEN,5           SET FOR UPDATIVE SOON                        
         LA    R3,MYKEY                                                         
         USING LKKEYD,R3                                                        
         XC    MYKEY(L'LOCKEY),MYKEY                                            
         MVC   LOCKSE,BYTE         SET SE NUMBER                                
         MVC   LOCKAGY,NBSELAGY    AGENCY                                       
         MVC   LOCKRTY,=C'UN'      UNIT RECORDS                                 
         MVC   LOCKKEY(3),NBSELCLI    3 BYTE CLIENT CODE                        
         MVC   LOCKKEY+3(4),NBSELNET  4 BYTE NETWORK                            
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,X'7E'                                                     
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         L     R4,ACOMFACS                                                      
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),(LOCKSW,MYKEY),(R4)                                    
         CLI   DMCB+4,0            ANY ERRORS                                   
         BE    LOCKX                                                            
         XC    CONHEAD,CONHEAD    YES/ERRORS                                    
         MVC   CONHEAD(33),=C'*** CLIENT LOCKED - TRY LATER ***'                
         GOTO1 ERREX2                                                           
         DROP  R3                                                               
LOCKX    J     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*  MOVE ANY LEFTOVER DOLLARS INTO THE LAST UNIT                                 
*                                                                               
TMEXFIN  NTR1  BASE=*,LABEL=*                                                   
         CLI   TESTRUN,C'Y'         CHECK FOR TEST RUN                          
         BE    TMEXFEX                                                          
*                                                                               
         ICM   R1,15,TMEXCST        TMEX CST                                    
         CVD   R1,WORK                                                          
         MP    WORK(8),=PL2'100'    GET CORRECT PRECISSION                      
         SP    WORK(8),TOTTMEX(8)                                               
*                                                                               
         MVC   KEY,LASTREC          KEY OF LAST UPDATED UNIT                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR  ',KEY,KEY                     
         CLC   KEY(20),LASTREC                                                  
         JE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'UNTFIL  ',KEY+21,    X        
               NBAIO,DMWORK                                                     
         TM    DMCB+8,X'FD'                                                     
         JZ    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R3,NBAIO                                                         
         USING NURECD,R3                                                        
         ICM   R1,15,NUASSIGN                                                   
         CVD   R1,WORK+8                                                        
         AP    WORK(8),WORK+8(8)                                                
         CVB   R1,WORK                                                          
         STCM  R1,15,NUASSIGN                                                   
         GOTO1 DATAMGR,DMCB,(X'80',=C'PUTREC'),=C'UNTFIL  ',KEY+21,    X        
               NBAIO,DMWORK                                                     
TMEXFEX  J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* - CHECKS TO ENSURE UNITS TO BE LOCKED ARE NOT BILLED/PAID                     
* - SETS NBRESUME SO NETIO                                                      
* - STARTS AGAIN AT PACKAGE READ                                                
CHKACC   NTR1  BASE=*,LABEL=*                                                   
         NI    MYFLAG,X'FF'-UNITBILL                                            
*                                                                               
CHKACC2  GOTO1 NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST     TEST FOR EOF                                 
         BE    CHKACC8             YES                                          
         CLI   NBMODE,NBPROCUN     TEST FOR UNIT RECORD                         
         BNE   CHKACC2                                                          
*                                                                               
         XC    KEY2,KEY2                                                        
         LA    R1,KEY2                                                          
         USING NUBKEY,R1                                                        
         MVC   KEY2(2),=X'0E06'                                                 
         MVC   NUBKAM,NBACTAM                                                   
         MVC   NUBKCLI,NBACTCLI                                                 
         MVC   NUBKNET,NBACTNET                                                 
         MVC   NUBKPROG,NBACTPRG                                                
         MVC   NUBKDATE,NBACTDAT                                                
         MVC   NUBKEST,NBACTEST                                                 
         MVC   NUBKSUB,NBACTSUB                                                 
         MVC   NUBKDPT,NBACTDP                                                  
         MVC   KEY2SV,KEY2                                                      
         GOTO1 NBDM,DMCB,(0,=C'DMRDHI  '),=C'XSPDIR  ',KEY2,KEY2,0              
         CLC   KEY2SV(20),KEY2                                                  
         BE    CHKACC6             YES-BILLING                                  
*                                                                               
         L     R3,NBAIO            R3=ELEMENT POINTER                           
         LA    R3,NUMAINEL-NUKEY(R3)                                            
CHKACC4A CLI   0(R3),X'10'         TEST FOR BILLING ELEMENT                     
         BE    CHKACC6                                                          
         CLI   0(R3),0             TEST FOR EOF                                 
         BE    CHKACC2                                                          
*                                                                               
         TM    OPTTYP2,X'40'                                                    
         BO    CHKACC5                                                          
         TM    OPTTYP2,X'20'       TRADE?  DON'T CHECK PAYING                   
         BO    CHKACC5                                                          
*                                                                               
         CLI   0(R3),X'12'         TEST FOR PAYING ELEMENT                      
         BE    CHKACC6                                                          
*                                                                               
CHKACC5  ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CHKACC4A                                                         
*                                                                               
CHKACC6  DS    0H                  ERROR                                        
         TM    OPTTYP2,X'20'       TRADE?                                       
         BZ    CHKACC7                                                          
         OI    MYFLAG,UNITBILL                                                  
         B     CHKACC7A                                                         
*                                                                               
CHKACC7  L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
         CLI   TESTRUN,C'Y'                                                     
         BE    *+12                                                             
*        MVI   HALF,C'U'           UNLOCK IF UPDATIVE SOON                      
         MVI   LOCKSW,C'U'           UNLOCK IF UPDATIVE SOON                    
         BRAS  RE,LOCKEM                                                        
CHKACC7A XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(34),=C'*** ERROR - UNITS HAVE BILLED/PAID'               
         GOTO1 ERREX2                                                           
         DROP  R5                                                               
*                                                                               
CHKACC8  DS    0H                  SUCCESSFUL CHECK - EXIT                      
         MVI   NBRESUME,NBPROCPK      START AT PACKAGES                         
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
NETLIST  DS    CL6000                                                           
STALIST  DS    CL4000                                                           
NETBLRD  DS    XL200                                                            
*                                                                               
*                                                                               
WORKD    DSECT                                                                  
         DS    0D                                                               
MYDMWRK  DS    CL96                                                             
ADEMCON  DS    A                                                                
COUNTER  DS    F                                                                
ASSTOT   DS    PL8                                                              
ACTTOT   DS    PL8                                                              
INTTOT   DS    PL8                                                              
IC2TOT   DS    PL8                                                              
TMEXASS  DS    PL8                                                              
TOTTMEX  DS    PL8                                                              
TMEXPCT  DS    PL4                                                              
EICPAID  DS    F                                                                
TMEXCST  DS    F                                                                
DPTEQU   DS    CL1                                                              
TESTRUN  DS    CL1                                                              
OPTTYPE  DS    CL1                                                              
OPTTYP2  DS    CL1                                                              
*                                                                               
OPTTYP3  DS    CL1                                                              
OP3DEMOQ EQU   X'10'               DEMOLOCK                                     
*                                                                               
NEWRATE  DS    CL1                 NEW RATE TYPE                                
NEWRCOV  DS    CL1                 NEW RATE COVERAGE                            
NEWDTYP  DS    CL1                 NEW DEMO TYPE                                
NEWVTYP  DS    CL3                 NEW V TYPE                                   
FIRST    DS    CL1                                                              
SVPRD    DS    XL3                 PRODUCT                                      
SVEST    DS    XL1                 ESTIMATE                                     
LASTREC  DS    CL20                                                             
KEYSV    DS    CL20                                                             
MYKEY    DS    CL20                                                             
SAVEKEY  DS    CL20                                                             
KEY2     DS    CL40                                                             
KEY2SV   DS    CL40                                                             
HOOKSAVE DS    CL4                                                              
BYT1     DS    CL1                                                              
BYT2     DS    CL1                                                              
SCNBLOCK DS    CL400                                                            
NEWASSGN DS    CL4                                                              
NEWACTUL DS    CL4                                                              
TODAY    DS    CL3                 TODAY'S DATE (YMD)                           
COST2C   DS    XL4                 CLIENT COST2 FACTOR                          
COST2E   DS    XL4                 ESTIMATE COST2 FACT                          
UNITDATE DS    XL2                                                              
ICOST2C  DS    XL1                 CLIENT COST2 FACTOR (INTEGRATION)            
ICOST2E  DS    XL1                 ESTIMATE COST2 FACTOR (INTEGRATION)          
PKGCPM   DS    CL4                                                              
LOCKSW   DS    CL1                 Y=LOCK FOR UPDATIVE SOON                     
PKGFLTRS DS    CL6                                                              
PKGDEM   DS    CL7                 PRINTABLE DEMO                               
PKGDEM3  DS    CL3                 INTERNAL DEMO CODE                           
CLICPRD  DS    CL3                 CORPORATE PRODUCT                            
POSTYPE  DS    CL1                 REQUESTED STATION POSTING TYPE               
PKG09SV  DS    XL(NAUDELN)                                                      
*                                                                               
TRDPCT   DS    XL2                 TRADE PERCENTAGE                             
CASHPCT  DS    XL2                 CASH PERCENTAGE                              
SREPCODE DS    CL2                 SPECIAL REP CODE BINARY                      
*                                                                               
MYFLAG   DS    XL1                                                              
UNITBILL EQU   X'01'               UNIT HAS BILLING                             
ADDLKDEM EQU   X'02'               ADDED LOCKED DEMO ELEMENT                    
*                                                                               
BILLTOT  DS    PL8                                                              
BILLDOL  DS    XL16                RETURNED BILL $ FROM NETBILLRDR              
*                                                                               
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE NEGENUBILL                                                     
       ++INCLUDE NETBILLRD                                                      
       ++INCLUDE NEGENDPT                                                       
       ++INCLUDE FASSB                                                          
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALOCKETD                                                      
         PRINT OFF                                                              
       ++INCLUDE NETINCLN                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIEFD                                                       
       ++INCLUDE DDGENTWA                                                       
*                                                                               
PLINED   DSECT                                                                  
PCLI     DS    CL6                 CLIENT                                       
         DS    CL1                                                              
PPROG    DS    CL7                 PROGRAM                                      
         DS    CL1                                                              
PNET     DS    CL7                 NETWORK                                      
         DS    CL1                                                              
PEST     DS    CL8                 ESTIMATE                                     
         DS    CL1                                                              
PDATE    DS    CL9                 UNIT DATE                                    
         DS    CL1                                                              
PSUB     DS    CL7                 SUBLINE                                      
         DS    CL1                                                              
PPACK    DS    CL7                 PACKAGE                                      
PLINEDQ  EQU   *-PCLI                                                           
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'140NEWRI70   12/02/20'                                      
         END                                                                    
