*          DATA SET REREPK305  AT LEVEL 005 AS OF 05/01/02                      
*PHASE REK302B,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREPK302A (REK302A) --- KATZ STATION CONVERSION'               
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPK302A -- KATZ CONVERSION:  STATION FILE CONVERSION.  *            
*                      ACCEPT KATZ TAPE, CONVERT MASTER RECORDS    *            
*                      AND GENERATE ASSOCIATED DDS FORMAT RECORDS  *            
*                                                                  *            
*        SPECIAL VERSION:  UPDATE TVB REGION FIELD                 *            
*                                                                  *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* JUL21/95 (BU ) --- ORIGINAL ENTRY, BASED ON SWITCHER (REREPSW02) *            
*                                                                  *            
* NOV21/95 (BU ) --- SPLIT OUT STATION RECS BY COMPANY             *            
*                                                                  *            
* MAY06/96 (BU ) --- OFFTEAM ELEMENT UPDATE                        *            
*                                                                  *            
********************************************************************            
*   NOTE:  ADJUST COMPTABL FOR PRODUCTION RUN!!                    *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  QRECORD+20 - COUNTERS FOR DISPLAYING RANGE OF RECORDS PROCESSED.*            
*               20-25 =  START   26-31 =  END                      *            
*  QRECORD+36 - REP CODE TO INSERT INTO RECORDS.                   *            
*                                                                  *            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR+0   =   Y  =  DISPLAY INPUT RECORDS                  *            
*     QUESTOR+0   =   D  =  DISPLAY ALL INPUT RECORDS              *            
*     QUESTOR+1   =   Y  =  DISPLAY RECORDS PUT TO OUTPUT          *            
*     QUESTOR+2   =   Y  =  MARKET#/NAME LIST                      *            
*     QUESTOR+3   =   Y  =  DISPLAY MARKET RECORDS PUT TO OUTPUT   *            
*     QUESTOR+4   =                                                *            
*     QUESTOR+5   =                                                *            
*     QUESTOR+6   =                                                *            
*     QUESTOR+7   =                                                *            
*     QUESTOR+8   =                                                *            
*     QUESTOR+9   =                                                *            
*     QUESTOR+10  =                                                *            
*     QUESTOR+11  =                                                *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REK302   CSECT                                                                  
         NMOD1 0,**REK3**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   MAINEXIT                                                         
         DS    0H                                                               
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
         MVC   P+1(28),=C'BEGINNING STATION CONVERSION'                         
         GOTO1 REPORT                                                           
*                                                                               
         L     R4,ARECAREA         SET A(TAPE RECD DELIVERY AREA)               
MAIN0040 EQU   *                                                                
         GET   INTAPE,(R4)         READ TAPE RECORD INTO RDA                    
*                                  EOF --> MAIN0080                             
*                                                                               
         GOTO1 STAPROC,DMCB,(RC)   PROCESS STATION RECORD                       
         B     MAIN0040            GO BACK FOR NEXT RECORD                      
*                                                                               
MAIN0080 EQU   *                                                                
         MVC   P+1(25),=C'ENDING STATION CONVERSION'                            
         GOTO1 REPORT                                                           
         GOTO1 DISPTOTS,DMCB,(RC)  DISPLAY TOTALS FOR RUN                       
*                                                                               
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE FILOUTB                                                          
         CLOSE (INTAPE,REWIND)                                                  
MAINEXIT XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  STAPROC:  CONVERT KATZ STATIONS TO DDS FORMAT                 *              
******************************************************************              
*                                                                               
STAPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,ARECAREA         SET A(INPUT RECORD)                          
         USING KTZSTAD,R4          SET DSECT FOR KATZ RECORD                    
*                                                                               
         CLI   KMKTREGN,C' '       ANY TVB REGION?                              
         BE    STAP0480            NO  - SKIP RECORD                            
*                                                                               
         LA    R3,REC                                                           
         USING RSTAREC,R3                                                       
         XCEFL REC,1008                                                         
         CLI   QUESTOR+0,C'D'      DISPLAY STATION RECORD?                      
         BNE   STAP0020            NOT SPECIAL REQUEST                          
         GOTO1 DISPIPUT,DMCB,(RC)                                               
STAP0020 EQU   *                                                                
         L     RF,OLDSTAS                                                       
         LA    RF,1(RF)                                                         
         ST    RF,OLDSTAS                                                       
         CLI   QUESTOR+2,C'Y'      DISPLAY STATION #/MARKET NAME?               
         BNE   STAP0040            NOT SPECIAL REQUEST                          
         MVC   P+1(3),KMKTNUM                                                   
         MVC   P+5(18),KMKTNAM                                                  
         MVC   P+25(5),KMKTSTA                                                  
         GOTO1 REPORT                                                           
STAP0040 EQU   *                                                                
         CLC   KMKTSTA,SPACES      ANY STATION IN FIELD?                        
         BE    STAP0480            NO  - SKIP IT!                               
         CLI   QGROUP,C'K'         KATZ TV STATIONS PROCESSING?                 
         BNE   STAP0080            NO  - SELTEL PROCESSING                      
         CLI   KMKTDIV,C'A'        YES - KATZ AMERICAN?                         
         BE    STAP0120            YES - USE IT                                 
         CLI   KMKTDIV,C'C'        YES - KATZ CONTINENTAL?                      
         BE    STAP0120            YES - USE IT                                 
         CLI   KMKTDIV,C'I'        YES - KATZ NATIONAL?                         
         BE    STAP0120            YES - USE IT                                 
         B     STAP0480            NO  - NOT KATZ TV - SKIP IT                  
STAP0080 EQU   *                   TEST FOR SELTEL                              
         CLI   QGROUP,C'S'         SELTEL TV STATIONS PROCESSING?               
         BE    *+6                 YES - SELTEL PROCESSING                      
         DC    H'0'                NOT RECOGNIZED:  DUMP IT OUT                 
         CLI   KMKTDIV,C'S'        SELTEL WARRIORS?                             
         BE    STAP0120            YES - USE IT                                 
         CLI   KMKTDIV,C'T'        SELTEL KNIGHTS?                              
         BE    STAP0120            YES - USE IT                                 
         CLI   KMKTDIV,C'N'        YES - SELTEL INTERNATIONAL?                  
         BE    STAP0120            YES - USE IT                                 
         B     STAP0480            NO  - NOT SELTEL TV - SKIP IT                
STAP0120 EQU   *                                                                
         CLC   LASTSTAT,KMKTSTA    STATION ALREADY SEEN?                        
         BE    STAP0480            YES - SKIP IT                                
         MVC   LASTSTAT,KMKTSTA    NO  - PROCESS IT                             
         CLI   QUESTOR+0,C'Y'      DISPLAY STATION RECORD?                      
         BNE   STAP0160            NOT SPECIAL REQUEST                          
         GOTO1 DISPIPUT,DMCB,(RC)                                               
STAP0160 EQU   *                                                                
*                                                                               
*   BUILD KEY TO ACCESS RECORD ON FILE.                                         
*                                                                               
         MVI   RSTAKTYP,2          INSERT RECORD TYPE                           
         LA    RF,COMPTEST         SET A(COMPANY TABLE)                         
         CLI   QOPTION2,C'P'       PRODUCTION POWER CODES?                      
         BNE   STAP0180            NO  - USE V4/V5/V6                           
         LA    RF,COMPPROD         YES - USE AM/CQ/NK                           
STAP0180 EQU   *                                                                
         CLC   KMKTDIV,0(RF)                                                    
         BE    STAP0190                                                         
         LA    RF,3(RF)            BUMP TO NEXT ENTRY                           
         CLI   0(RF),0             END OF TABLE REACHED?                        
         BNE   STAP0180            NO  - GO BACK AND CHECK                      
         DC    H'0'                YES - SHOULDN'T HAPPEN                       
COMPTEST EQU   *                                                                
         DC    C'AV4'              AV4                                          
         DC    C'CV5'              CV5                                          
         DC    C'IV6'              IV6                                          
         DC    X'0000'                                                          
COMPPROD EQU   *                                                                
         DC    C'AAM'              LIVE VALUES FOR PRODUCTION RUN!!             
         DC    C'CCQ'              LIVE VALUES FOR PRODUCTION RUN!!             
         DC    C'INK'              LIVE VALUES FOR PRODUCTION RUN!!             
         DC    X'0000'                                                          
         DS    H'0'                                                             
STAP0190 EQU   *                                                                
         MVC   RSTAKREP,1(RF)      INSERT NEW REP CODE                          
         MVC   SAVEREP,1(RF)       SAVE THE REP CODE                            
         MVC   RSTAKSTA(4),KMKTSTA INSERT STATION W/O MEDIA                     
         MVI   RSTAKSTA+4,C' '     CLEAR MEDIA BYTE                             
         XC    KEY,KEY                                                          
         MVC   KEY(27),RSTAKEY     SET KEY FOR READ                             
         BAS   RE,HIGHDIR          READ KEY                                     
*                                                                               
*   TEST                                                                        
         MVC   P+1(5),=C'KEYS:'                                                 
         MVC   P+10(27),KEY                                                     
         MVC   P+40(27),KEYSAVE                                                 
         GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    STAP0195            YES                                          
         MVC   P+1(18),=C'STATION NOT FOUND:'                                   
         MVC   P+20(7),KEYSAVE+20                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     STAP0480                                                         
STAP0195 EQU   *                                                                
         BAS   RE,GETRECRD         RETRIEVE RECORD FROM FILE                    
         CLI   QUESTOR+1,C'Y'      DISPLAY STATION PUT RECORD?                  
         BNE   STAP0200            NOT SPECIAL REQUEST                          
         GOTO1 REPORT                                                           
         MVC   P+1(14),=C'STATION INPUT '                                       
         EDIT  OLDSTAS,(5,P+20)                                                 
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC),0                                              
         GOTO1 REPORT              INSERT A BLANK LINE                          
STAP0200 EQU   *                                                                
*                                                                               
         LA    RF,TVBTABLE         SET A(TVB CONVERT TABLE)                     
STAP0220 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                UNRECOGNIZED KATZ CODE                       
         CLC   KMKTREGN,0(RF)      KATZ CODE IN TABLE?                          
         BE    STAP0240            YES - INSERT NEW CODE                        
         LA    RF,3(RF)            NO  - BUMP TO NEXT ENTRY                     
         B     STAP0220            GO BACK FOR NEXT                             
*                                                                               
TVBTABLE EQU   *                                                                
         DC    C'ANE'              NORTHEAST                                    
         DC    C'BMA'              MIDATLANTIC                                  
         DC    C'CSA'              SOUTH ATLANTIC                               
         DC    C'DES'              SOUTHEAST CENTRAL                            
         DC    C'HMO'              MOUNTAIN                                     
         DC    C'IPA'              PACIFIC                                      
         DC    C'EWS'              SOUTHWEST CENTRAL                            
         DC    C'FEN'              NORTHEAST CENTRAL                            
         DC    C'GWN'              NORTHWEST CENTRAL                            
         DC    X'0000'                                                          
         DS    0F                                                               
STAP0240 EQU   *                                                                
         MVC   RSTATVB,1(RF)       INSERT NEW CODE                              
         CLI   QOPTION1,C'U'       HARD UPDATE?                                 
         BNE   STAP0250            NO                                           
         BAS   RE,PUTRECRD         GENERATE NEW OUTPUT                          
STAP0250 EQU   *                                                                
         CLI   QUESTOR+1,C'Y'      DISPLAY STATION PUT RECORD?                  
         BNE   STAP0480            NOT SPECIAL REQUEST                          
         GOTO1 REPORT                                                           
         MVC   P+1(14),=C'STATION OUTPUT'                                       
         EDIT  OLDSTAS,(5,P+20)                                                 
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC),0                                              
         GOTO1 REPORT              INSERT A BLANK LINE                          
STAP0480 EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
INITIAL  NTR1                                                                   
         OPEN  (INTAPE,(INPUT))                                                 
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',100000,100000                                   
*                                  GET 100K STORAGE SPACE                       
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ARECAREA,P2         TAPE RECORD DELIVERY AREA                    
         L     RF,ARECAREA                                                      
         A     RF,=F'40000'        TAPE BUFFER AREA:                            
*                                     AT THIS TIME SIZE IS UNKNOWN              
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
****     CLC   QRECORD+36(2),SPACES                                             
*                                  NEW REP CODE = SPACE?                        
***      BNE   *+6                 NO                                           
****     DC    H'0'                YES - NEED CODE                              
****     MVC   KATZREP,QRECORD+36  SAVE CODE                                    
         CLC   QRECORD+20(12),SPACES                                            
*                                  ANY DISPLAY VALUES?                          
         BE    INIT0060            NO                                           
         PACK  DUB,QRECORD+20(6)                                                
         CVB   RF,DUB                                                           
         ST    RF,LOWCTR           SAVE LOW VALUE                               
         PACK  DUB,QRECORD+26(6)                                                
         CVB   RF,DUB                                                           
         ST    RF,HIGHCTR          SAVE HI  VALUE                               
INIT0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
HELOSTA1 NTR1                                                                   
         LA    R3,REC                                                           
         USING RSTAREC,R3                                                       
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RSTAREC,ELTBILD1,0             
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
HELOSTAD NTR1                                                                   
         LA    R3,REC                                                           
         USING RSTAREC,R3                                                       
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(4,RSTAREC),0,0                
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
HELOSTA2 NTR1                                                                   
         LA    R3,REC                                                           
         USING RSTAREC,R3                                                       
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RSTAREC,ELTBILD1,     X        
               =C'ADD=CODE'                                                     
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
HELOMKT1 NTR1                                                                   
         LA    R3,REC                                                           
         USING RMKTREC,R3                                                       
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RMKTREC,ELTBILD1,0             
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
HIGHDIR  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 2                                                                
SEQDIR   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 3                                                                
GETRECRD LA    R6,GETREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
PUTRECRD LA    R6,PUTREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               REC,(0,DMWORK)                                                   
         B     DMCHECK                                                          
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK  TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT                                                            
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIT    CR    RB,RB                                                            
         B     MAINEXIT                                                         
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         B     MAINEXIT                                                         
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,TRACEKEY                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
         SPACE 1                                                                
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     MAINEXIT                                                         
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   OLDSTAS,LOWCTR      DISPLAY RANGE OF RECORDS                     
         BL    DIPU0090                                                         
         CLC   OLDSTAS,HIGHCTR                                                  
         BH    DIPU0090                                                         
         LA    R4,REC              A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         ZICM  RF,REC+27,2                                                      
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIPU0090 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPIPUT:  DISPLAY KATZ RECORD INPUT.                         *              
*                                                                *              
******************************************************************              
*                                                                               
DISPIPUT NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   OLDSTAS,LOWCTR      DISPLAY RANGE OF RECORDS                     
         BL    DIPI0120                                                         
         CLC   OLDSTAS,HIGHCTR                                                  
         BH    DIPI0120                                                         
         B     DIPI0040                                                         
DISPIPT1 NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
DIPI0040 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+1(14),=C'STATION INPUT'                                        
***      GOTO1 REPORT                                                           
         L     R4,ARECAREA         A(RECORD LENGTH FIELD)                       
****>>>  GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',80,=C'1D'                  
*                                  DISPLAY 80 CHARACTER RECORD                  
         MVC   P+16(80),0(R4)      DISPLAY INPUT RECORD                         
         GOTO1 REPORT                                                           
DIPI0120 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPTOTS:                                                     *              
******************************************************************              
*                                                                               
DISPTOTS NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(24),=C'STATION RECORDS READ   :'                             
         EDIT  OLDSTAS,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'STATION RECORDS WRITTEN:'                             
         EDIT  NEWSTAS,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'MARKET  RECORDS WRITTEN:'                             
         EDIT  NEWMKTS,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
LBLDAREA DS    F                                                                
OLDSTAS  DS    F                                                                
NEWSTAS  DS    F                                                                
NEWMKTS  DS    F                                                                
CONCTR   DS    F                                                                
LOWCTR   DS    F                                                                
HIGHCTR  DS    F                                                                
KATZREP  DS    CL2                                                              
SAVEREP  DS    CL2                                                              
FLAGBYTS DS    0CL12               FLAGS                                        
ELTBILD1 DS    CL128                                                            
LASTSTAT DS    CL4                                                              
TEAMWORK DS    CL6                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,RECFM=FB,LRECL=80,               X        
               BLKSIZE=800,MACRF=GM,EODAD=MAIN0080                              
*                                                                               
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL1008              AREA FOR RECORD                              
         SPACE 2                                                                
         DS    0H                  HALFWORD ALIGNMENT NEEDED.                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
KTZSTAD  DSECT                                                                  
KMKTNUM  DS    CL3      +0         MARKET NUMBER                                
KMKTNAM  DS    CL18     +3         MARKET NAME                                  
KMKTNETW DS    CL1      +21        MARKET NETWORK                               
KMKTCHNL DS    CL2      +22        MARKET CHANNEL                               
KMKTSTA  DS    CL5      +24        STATION CALL LETTERS                         
KMKTCOMP DS    CL30     +29        COMP STATIONS: 6 X 5 CHARS                   
KMKTATTM DS    CL1      +59        ATLANTA TEAM                                 
KMKTDATM DS    CL1      +60        DALLAS TEAM (SELTEL ONLY)                    
         DS    CL4      +61        SPARE                                        
KMKTTMZN DS    CL1      +65        TIME ZONE                                    
KMKTCOMS DS    CL1      +66        CUSTOM COMMENTS                              
KMKTLATM DS    CL1      +67        LOS ANGELES TEAM                             
KMKTSFTM DS    CL1      +68        SAN FRANCISCO TEAM                           
KMKTSRVC DS    CL1      +69        SERVICE                                      
KMKTRANK DS    CL1      +70        RANK                                         
KMKTLOST DS    CL1      +71        LOST MARKET (L=LOST)                         
KMKTDIV  DS    CL1      +72        DIVISION                                     
KMKTNYTM DS    CL1      +73        NEW YORK TEAM                                
KMKTCHTM DS    CL1      +74        CHICAGO  TEAM                                
KMKTKWIX DS    CL1      +75        KWIX                                         
KMKTREGN DS    CL1      +76        REGION                                       
KMKTPTCH DS    CL1      +77        PITCH STATION                                
KMKTSTCD DS    CL2      +78        STATE CODE                                   
*                                                                               
*                                                                               
         EJECT                                                                  
*  INCLUDE REGENSTA                STATION     RECORD                           
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
RECD     DSECT                                                                  
RECORD   DS    CL1008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION     RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENMKT          MARKET      RECORD                           
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
         SPACE 4                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005REREPK305 05/01/02'                                      
         END                                                                    
