*          DATA SET NEWRI70S   AT LEVEL 083 AS OF 05/01/02                      
*PHASE T32070A,+0                                                               
*INCLUDE CLUNPK                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'T32070 - UTILITY PROGRAM'                                       
                                                                                
*****************************************************                           
*  OPTIONS:                                                                     
*                                                                               
*  ACT = Y     - OPTTYPE=X'01' SET ACTUAL DOLLARS TO ASSIGNED                   
*  UNM = Y     - OPTTYPE=X'02' UNMATCHED SPOTS DELETED                          
*  RATE = N    - OPTTYPE=X'04' CHANGE RATE TYPE TO N                            
*  LOCK        - OPTTYPE=X'08' LOCK UNITS                                       
*  UNLOCK      - OPTTYPE=X'10' UNLOCK UNITS                                     
*  SPLIT       - OPTTYPE=X'20' SPLIT 30'S INTO 15'S                             
*  PF=         - OPTTYPE=X'40' PACKAGE FILTERS                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*******************************************************                         
T32070   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEUT**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            ANETWS1=CLIENT RECORD                        
         USING NETSYSD,R9                                                       
         LA    R1,HEADSPC                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         L     R7,ANETWS2          ANETWS2+300=WORKING STORAGE                  
         A     R7,=F'300'                                                       
         USING WORKD,R7                                                         
* - NEED TO HAVE COPIES WRITTEN TO RECOVERY FILE                                
         L     R1,TWAMASTC                                                      
         USING MCBLOCK,R1                                                       
         L     R1,MCSSB                                                         
         USING SSBD,R1                                                          
         OI    SSBSTAT2,SSBSROLC   RECOVER OFFLINE COPIES                       
         DROP  R1                                                               
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
         BAS   RE,REPMOD                                                        
         B     XIT                                                              
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   RP4                                                              
         BAS   RE,EDITMOD                                                       
         B     XIT                                                              
RP4      EQU   *                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE                                                                  
EDITMOD  NTR1                                                                   
*                                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
         MVI   BYT1,0              SET LOCK/UNLOCK FLAG IN BYT1                 
*                                                                               
         CLC   =C'LOCK',SPLOPT                                                  
         BNE   *+8                                                              
         MVI   BYT1,C'L'                                                        
         CLC   =C'UNLOCK',SPLOPT                                                
         BNE   *+8                                                              
         MVI   BYT1,C'U'                                                        
*                                                                               
         LA    R2,SPLCLIH               CLIENT                                  
         NETGO NVCLI,DMCB,                                                      
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
                                                                                
         LA    R2,SPLPROH               PRODUCT                                 
         NETGO NVPRDALL,DMCB                                                    
         CLI   BYT1,0                  ,,IF LOCK/UNLOCK                         
         BE    EDT03                                                            
         CLC   =C'POL',SPLPRO          ,,PROD MUST BE POL                       
         BNE   EDINV                                                            
*                                                                               
EDT03    DS    0H                       ESTIMATE                                
         CLI   BYT1,0              ,,IF LOCK/UNLOCK                             
         BE    *+8                                                              
         MVI   FTERMFLG,0          ,,ESTIMATE IS REQUIRED                       
         LA    R2,SPLESTH                                                       
         NETGO NVESTALL,DMCB                                                    
         CLI   BYTE,0                                                           
         BE    EDT03D                                                           
         CLC   =C'ALL',SPLEST      ,,EST CAN NOT = ALL                          
         BE    EDINV                                                            
         L     R2,NBAIO                                                         
         USING EKEY,R2                                                          
         MVC   NBSELSTR,ESTART      ,,SET ESTIMATE START                        
         MVC   NBSELEND,EEND        ,,SET ESTIMATE END                          
         DROP  R2                                                               
*                                                                               
EDT03D   LA    R2,SPLNETH               NETWORK                                 
         NETGO NVNETALL,DMCB                                                    
         CLI   BYT1,0                                                           
         BE    EDT03E                                                           
         CLC   =C'ALL',SPLNET      IF LOCK/UNLOCK                               
         BE    EDINV               ALL IS INVALID                               
*                                                                               
EDT03E   CLI   BYT1,0              IF LOCK/UNLOCK                               
         BE    EDT04                                                            
         XC    SPLDPT,SPLDPT       CLEAR DPT FIELD                              
         OI    SPLDPTH+6,X'80'                                                  
         B     EDT04A                                                           
EDT04    LA    R2,SPLDPTH               DAYPART                                 
         NETGO NVDPTALL,DMCB                                                    
*                                                                               
EDT04A   LA    R2,SPLPAKH               PACKAGE                                 
         NETGO NVPAKLOK,DMCB                                                    
         CLI   BYT1,0              ,,IF LOCK/UNLOCK                             
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
EDT04B   CLI   BYT1,0              IF LOCK/UNLOCK                               
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
         CLC   =C'PF',SPLOPT       IF PKG FILTER CHANGE                         
         BE    EDINV               DON'T ALLOW FOR SOON                         
         MVI   HALF,C'L'           LOCK FOR UPDATIVE SOON                       
         BAS   RE,LOCKEM                                                        
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
         B     EDT20                                                            
                                                                                
EDT11    DS    0H                                                               
         CLC   12(3,R3),=C'UNM'    DELETE UNMATCHED SPOTS                       
         BNE   EDT12                                                            
         OI    OPTTYPE,X'02'                                                    
         B     EDT20                                                            
*                                                                               
EDT12    DS    0H                                                               
         CLC   12(4,R3),=C'RATE'   RATE TYPE CHANGE                             
         BNE   EDT14                                                            
         CLC   =C'NULL',22(R3)     DELETE RATE TYPES                            
         BNE   EDT12A              NO                                           
         MVI   NEWRCOV,0           YES                                          
         XC    NEWRATE,NEWRATE     YES                                          
         OI    OPTTYPE,X'04'                                                    
         B     EDT20                                                            
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
         BE    EDT20                                                            
         CLI   NEWRCOV,C'I'                                                     
         BE    EDT20                                                            
         CLI   NEWRCOV,C'A'        IF ALL COST                                  
         BNE   EDINV                                                            
         MVI   NEWRCOV,0           SET FIELD TO BLANK                           
         B     EDT20                                                            
                                                                                
RTYPE    DC    C'FCWYHTR0'                                                      
*                                                                               
EDT14    DS    0H                                                               
         CLC   =C'LOCK',12(R3)     LOCK UNITS                                   
         BNE   EDT15                                                            
         OI    OPTTYPE,X'08'                                                    
         B     EDT20                                                            
*                                                                               
EDT15    DS    0H                                                               
         CLC   =C'UNLOCK',12(R3)     UNLOCK UNITS                               
         BNE   EDT17                                                            
         OI    OPTTYPE,X'10'                                                    
         B     EDT20                                                            
*                                                                               
EDT17    DS    0H                                                               
         CLC   =C'PF',12(R3)         PACKAGE FILTERS                            
         BNE   EDT18                                                            
         CLI   1(R3),6             2ND HALF LENGTH GREATER THAN 6               
         BH    EDINV                                                            
         MVC   PKGFLTRS,22(R3)      SAVE PACKAGE FILTERS                        
         OI    OPTTYPE,X'40'                                                    
         B     EDT20                                                            
*                                                                               
EDT18    DS    0H                                                               
         B     EDINV                                                            
*                                                                               
EDT20    LA    R3,32(R3)                                                        
         LA    R5,1(R5)                                                         
         BCT   R4,EDT10                                                         
*                                                                               
EDTX     LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         EJECT                                                                  
* - REPORT MODE                                                                 
* - NETIO READS UNIT AND HOOKS TO MAINLINE FOR PROCESSING                       
                                                                                
REPMOD   NTR1                                                                   
                                                                                
* - SET UP NETIO PARAMETERS                                                     
         XC    COUNTER,COUNTER                                                  
         ZAP   ASSTOT,=P'0'                                                     
         ZAP   ACTTOT,=P'0'                                                     
         MVI   NBDATA,C'B'         PACKAGES/UNITS                               
         MVI   NBSELUOP,0          EST+ACT SCHEDULE                             
         MVI   NBUSER+13,C'N'      OVERRIDE NO PREEMPTS PROFILE                 
         MVI   NBRESUME,NBPROCPK   START AGAIN AT PACKAGES                      
         MVI   NBSELPST,C'B'       LOCKED/UNLOCKED                              
                                                                                
         TM    OPTTYPE,X'08'       LOCK UNITS                                   
         BNO   *+8                                                              
         BAS   RE,CHKACC             SEE IF UNITS HAVE BILL/PAY                 
                                                                                
         LA    R1,MAINLINE                                                      
         ST    R1,NBHOOK                                                        
                                                                                
RP10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    RPLAST                                                           
         B     RP10                                                             
                                                                                
                                                                                
* - REQUEST LAST PUTS OUT CHANGED REC COUNT                                     
RPLAST   MVC   P+1(13),=C'UNITS UPDATED'                                        
         L     R2,COUNTER                                                       
         EDIT  (R2),(10,P+15),ALIGN=LEFT                                        
         LTR   R2,R2                                                            
         BZ    RPL10                                                            
         EDIT  (P8,ASSTOT),(12,P+34),2                                          
         EDIT  (P8,ACTTOT),(12,P+48),2                                          
         B     *+10                                                             
RPL10    MVC   P+15(5),=C'=NONE'                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         CLI   TESTRUN,C'Y'        IF TEST RUN                                  
         BE    RPLX                THAT'S ALL                                   
         MVI   HALF,C'U'           ELSE/UNLOCK FOR UPDATIVE SOON                
         BAS   RE,LOCKEM                                                        
RPLX     B     XIT                                                              
                                                                                
         EJECT                                                                  
* - UNIT PROCESSING                                                             
MAINLINE NTR1                                                                   
         MVI   NBUPUNIT,C'N'       SET DON'T WRITE SWITCHES                     
         MVI   NBNOWRIT,C'N'                                                    
                                                                                
                                                                                
* - ACTUAL$ -> ASSIGNED$                                                        
         TM    OPTTYPE,X'01'                                                    
         BNO   MN10                                                             
         CLI   NBMODE,NBPROCUN     UNITS ONLY                                   
         BNE   MNX                                                              
         BAS   RE,ASSACT                                                        
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
                                                                                
* PACKAGE FILTERS                                                               
MN50     DS    0H                                                               
         TM    OPTTYPE,X'40'                                                    
         BNO   MN60                                                             
         BAS   RE,PKGFLTR                                                       
         B     MN100                                                            
                                                                                
MN60     DS    0H                                                               
         DC    H'0'                                                             
         EJECT                                                                  
                                                                                
* - PRINT OUT CHANGED RECORDS                                                   
                                                                                
MN100    MVC   P+1(3),NBCLICOD     PRINTABLE CLIENT CODE                        
         MVC   P+5(6),NBACTPRG     PROGRAM                                      
         MVC   P+13(4),NBACTNET    NETWORK                                      
         ZIC   R3,NBACTEST         ESTIMATE                                     
         EDIT  (R3),(3,P+19)                                                    
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(8,P+24)                                
                                                                                
         L     R2,NBAIO                                                         
         CLI   0(R2),X'04'         IS IT UNIT                                   
         BNE   MN110               NO                                           
         TM    OPTTYPE,X'40'       IS IT PKG FILTER OPTION?                     
         BNO   MN105                                                            
         MVI   ELCODE,8            YES- GET PKG FILTER ELEM                     
         BAS   RE,GETEL                                                         
MN103    BE    *+6                                                              
         DC    H'0'                GOT TO BE ONE                                
         USING NUFILD,R2                                                        
         CLI   NUFILSCM,C'K'       IS IT PKG ELEM?                              
         BE    *+12                                                             
         BAS   RE,NEXTEL                                                        
         B     MN103                                                            
         MVC   P+34(6),NUFILTER                                                 
         B     MN110                                                            
         DROP  R2                                                               
*                                                                               
MN105    MVI   ELCODE,1                                                         
         USING NUMAINEL,R2                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (B4,NUASSIGN),(12,P+34),2                                        
         EDIT  (B4,NUACTUAL),(12,P+48),2                                        
         ICM   R1,15,NUASSIGN                                                   
         CVD   R1,DUB                                                           
         AP    ASSTOT,DUB                                                       
         ICM   R1,15,NUACTUAL                                                   
         CVD   R1,DUB                                                           
         AP    ACTTOT,DUB                                                       
         B     MN110                                                            
*                                                                               
MN110    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLI   TESTRUN,C'Y'        WRITE UNITS TO FILE                          
         BE    MN200               NO                                           
         MVI   NBUPUNIT,C'Y'       YES                                          
         MVI   NBNOWRIT,C'Y'                                                    
*                                                                               
MN200    L     R1,COUNTER                                                       
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
ASSX     XIT1                                                                   
*                                                                               
         EJECT                                                                  
* - DELETE UNMATCHED SPOTS                                                      
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
*                                                                               
*                                  ...AND DELETE KEYS                           
         XC    KEY,KEY                                                          
         L     R2,NBAIO                                                         
         MVC   KEY(20),0(R2)        READ IT AND TEST TO BE SURE                 
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UNMAT20  LA    R2,KEY              PASSIVE KEY                                  
         USING NUKPKEY,R2                                                       
         XC    KEY+4(16),KEY+4                                                  
         MVI   KEY,X'84'                                                        
         MVC   NUKPNET,NBACTNET                                                 
         MVC   NUKPPROG,NBACTPRG                                                
         MVC   NUKPDATE,NBACTDAT                                                
         MVC   NUKPEST,NBACTEST                                                 
         MVC   NUKPSUB,NBACTSUB                                                 
         MVC   NUKPDP,NBACTDP                                                   
         OI    KEY+20,X'80'                                                     
         MVC   FILENAME,=C'UNTDIR  '                                            
         BAS   RE,WRTDIR                                                        
*                                                                               
         USING NUKDKEY,R2            SECOND PASSIVE KEY                         
UNMAT30  XC    KEY+4(16),KEY+4                                                  
         MVI   KEY,X'94'                                                        
         MVC   NUKDEST,NBACTEST                                                 
         MVC   NUKDNET,NBACTNET                                                 
         BAS   RE,DAYLOOK           PUTS ONE BYTE DAY CODE IN NUKDDAY           
         MVC   NUKDTIME,NBACTSQH                                                
         MVC   NUKDPROG,NBACTPRG                                                
         MVC   NUKDDATE,NBACTDAT                                                
         MVC   NUKDSUB,NBACTSUB                                                 
         OI    KEY+20,X'80'                                                     
         MVC   FILENAME,=C'UNTDIR  '                                            
         BAS   RE,WRTDIR                                                        
                                                                                
* - NOW DO ACTIVE KEY                                                           
         L     R2,NBAIO                                                         
         MVC   KEY(20),0(R2)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+20,X'80'                                                     
         MVC   FILENAME,=C'UNTDIR  '                                            
         BAS   RE,WRTDIR                                                        
*                                                                               
UNMX     B     XIT                                                              
*                                                                               
*                                                                               
* DATAMGR WRITE TO DIRECTORY                                                    
WRTDIR   NTR1                                                                   
         B     WRTD0                                                            
         GOTO1 =V(PRNTBL),DMCB,=C'DIRWRT',KEY,C'DUMP',25,=C'1D'                 
WRTD0    CLI   TESTRUN,C'N'                                                     
         BNE   WRTDX                                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',FILENAME,KEY,KEY                          
WRTDX    B     XIT                                                              
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
         EJECT                                                                  
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
         LA    R1,H3                                                            
         MVC   H3+10(5),SPLCLI                                                  
         LA    R1,H4                                                            
         MVC   H4+10(5),SPLPRO                                                  
         LA    R1,H5                                                            
         MVC   H5+10(8),SPLEST                                                  
         LA    R1,H6                                                            
         MVC   H6+10(8),SPLNET                                                  
         MVI   H7,0                                                             
         XIT1                                                                   
         EJECT                                                                  
* - UNLOCK UNITS/PKGS                                                           
UNLOCKU  NTR1                                                                   
         DROP  R2                                                               
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
UNLX     XIT1                                                                   
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
UNIX     XIT1                                                                   
         DROP  R2                                                               
                                                                                
*                                                                               
* - CHECKS TO ENSURE UNITS TO BE LOCKED ARE NOT BILLED/PAID                     
* - SETS NBRESUME SO NETIO                                                      
* - STARTS AGAIN AT PACKAGE READ                                                
CHKACC   NTR1                                                                   
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
         L     R3,NBAIO            R3=ELEMENT POINTER                           
         LA    R3,NUMAINEL-NUKEY(R3)                                            
         SR    R0,R0                                                            
*                                                                               
CHKACC4  CLI   0(R3),0             TEST FOR EOR                                 
         BE    CHKACC2             YES-GET NEXT RECORD                          
         CLI   0(R3),X'10'         TEST FOR BILLING ELEMENT                     
         BNE   CHKACC4A            NO                                           
         USING NUBILD,R3                                                        
         TM    NUBILST,X'20'       TEST FOR UNBILLED ELEMENT                    
         BO    CHKACC5             YES-LOOK AT NEXT ELEMENT                     
         B     CHKACC6                                                          
*                                                                               
CHKACC4A CLI   0(R3),X'12'         TEST FOR PAYING ELEMENT                      
         BE    CHKACC6                                                          
*                                                                               
CHKACC5  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CHKACC4                                                          
*                                                                               
CHKACC6  DS    0H                  ERROR                                        
         CLI   TESTRUN,C'Y'                                                     
         BE    *+12                                                             
         MVI   HALF,C'U'           UNLOCK IF UPDATIVE SOON                      
         BAS   RE,LOCKEM                                                        
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(34),=C'*** ERROR - UNITS HAVE BILLED/PAID'               
         GOTO1 ERREX2                                                           
*                                                                               
CHKACC8  DS    0H                  SUCCESSFUL CHECK - EXIT                      
         MVI   NBRESUME,NBPROCPK      START AT PACKAGES                         
         B     UNIX                                                             
         DROP  R3                                                               
         EJECT                                                                  
* LOCKER FOR UPDATIVE SOON                                                      
         SPACE                                                                  
LOCKEM   NTR1                                                                   
         CLC   =C'SOON',CONWHEN    IS IT SOON                                   
         BNE   LOCKX               NO/FORGET IT                                 
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
         LA    R3,MYKEY                                                         
         USING LKKEYD,R3                                                        
         XC    MYKEY(L'LOCKEY),MYKEY                                            
         MVC   LOCKSE,BYTE         SET SE NUMBER                                
         MVC   LOCKAGY,NBSELAGY    AGENCY                                       
         MVC   LOCKRTY,=C'UN'      UNIT RECORDS                                 
         MVC   LOCKKEY,NBSELCLI    3 BYTE CLIENT CODE                           
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,X'7E'                                                     
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R2,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         L     R6,ACOMFACS                                                      
         LTR   R6,R6                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (R2),(R1),(HALF,MYKEY),(R6)                                      
         CLI   DMCB+4,0            ANY ERRORS                                   
         BE    LOCKX                                                            
         XC    CONHEAD,CONHEAD    YES/ERRORS                                    
         MVC   CONHEAD(33),=C'*** CLIENT LOCKED - TRY LATER ***'                
         GOTO1 ERREX2                                                           
         DROP  R3                                                               
LOCKX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
WORKD    DSECT                                                                  
         DS    0D                                                               
MYDMWRK  DS    CL96                                                             
COUNTER  DS    F                                                                
ASSTOT   DS    PL8                                                              
ACTTOT   DS    PL8                                                              
PKGFLTRS DS    CL6                                                              
TESTRUN  DS    CL1                                                              
OPTTYPE  DS    CL1                                                              
NEWRATE  DS    CL1                 NEW RATE TYPE                                
NEWRCOV  DS    CL1                 NEW RATE COVERAGE                            
NEWDTYP  DS    CL1                 NEW DEMO TYPE                                
FIRST    DS    CL1                                                              
KEYSV    DS    CL20                                                             
MYKEY    DS    CL20                                                             
HOOKSAVE DS    CL4                                                              
BYT1     DS    CL1                                                              
SCNBLOCK DS    CL400                                                            
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE FASSB                                                          
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALOCKETD                                                      
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIEFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'083NEWRI70S  05/01/02'                                      
         END                                                                    
