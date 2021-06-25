*          DATA SET RERES35    AT LEVEL 114 AS OF 05/01/02                      
*          DATA SET RERES35    AT LEVEL 112 AS OF 03/01/96                      
*PHASE T81935A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T81935 --- RERES35 --- REFETCH TEST'                            
*                                                                               
********************************************************************            
*                                                                  *            
*     RERES35 (T81935) --- REFETCH TEST                            *            
*                                                                  *            
********************************************************************            
*                                                                               
         SPACE                                                                  
T81935   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1935**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
*                                                                               
         MVI   USEIO,C'Y'                                                       
         MVI   IOOPT,C'Y'                                                       
*                                                                               
         CLI   MODE,PROCPFK        HANDLE PF KEYS                               
         BE    PPFK                                                             
         CLI   MODE,VALKEY         HANDLE VALKEY                                
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPREC        DISPLAY RESULTS                              
         BE    DREC                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*        READ REP RECORD FOR A KEY FOR GENCON                                   
*                                                                               
VKEY     DS    0H                                                               
*                                                                               
         LA    R4,KEY                                                           
         USING RREPKEY,R4                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGENCY                                                  
         DROP  R4                                                               
*                                                                               
VKEYX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
*        PROCESS PFKEYS                                                         
*                                                                               
PPFK     DS    0H                                                               
*                                     ANALYZE PFKEYS                            
         CLI   PFAID,0             IF PFKEY ENTERED                             
         BE    PPFKX                                                            
*                                                                               
         BAS   RE,PFKEYS              GO ANALYZE                                
         BE    PPFKX                  NO ERRORS                                 
*                                                                               
         GOTO1 ERREX                  CHECK FOR ERRORS                          
*                                                                               
PPFKX    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*        VALIDATE RECORD                                                        
*                                                                               
VREC     DS    0H                                                               
         OI    GENSTAT2,RETEQSEL   RE-DISPLAY AFTER CHANGE                      
         TWAXC MASSTNFH            CLEAR END OF SCREEN                          
*                                                                               
         SPACE 1                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *           SOURCE               *           
*                                  *                                *           
*                                  **********************************           
         LA    R2,MASSRCH          VALIDATE SOURCE                              
         GOTO1 VVALSRC                                                          
         SPACE 1                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *           BOOK(S)              *           
*                                  *                                *           
*                                  **********************************           
         LA    R2,MASBKSH          VALIDATE BOOK                                
         CLI   5(R2),0             ENTRY NOT REQUIRED                           
         BE    BK65                                                             
         GOTO1 ANY                                                              
         MVC   DMCB+8(4),=C',=,-'                                               
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK1)                                     
         LA    R4,BLOCK1                                                        
         CLI   DMCB+4,1            MORE THAN 1 FIELD                            
         BH    BK10                                                             
         MVI   ERROR,2             INVALID INPUT FIELD                          
         BL    ERREND                                                           
         SPACE 1                                                                
         CLI   1(R4),0             1 FIELD MAY BE RANGE                         
         BE    BK10                OR JUST 1 BOOK                               
         SPACE 1                                                                
         ZIC   R5,0(R4)            LENGTH OF 1ST HALF OF FIELD                  
         LA    R5,8(R2,R5)                                                      
         CLI   0(R5),C'-'                                                       
         BNE   ERREND                                                           
         MVI   0(R5),C','          DUMMY FOR BOOKVAL (RESTORED BELOW)           
         SPACE 1                                                                
BK10     MVI   MAX,10              DUMMY SO I CAN DO ERROR MSG                  
         GOTO1 VVALBOOK                                                         
         MVC   NUMBOOK,ACTUAL      SAVE NUMBER OF BOOKS                         
         CLI   ACTUAL,7            REALLY ONLY 7 BOOKS ALLOWED                  
         BNH   BK20                                                             
         MVC   CONHEAD(L'MANYBKS),MANYBKS    TOO MANY BOOKS                     
         B     MYEND                                                            
         SPACE 1                                                                
BK20     DS    0H                                                               
*                                                                               
BK50     MVI   WORK,X'00'          BOOK(S) MUST BE SAME AS SOURCE               
         CLI   SVSOURCE,C'A'                                                    
         BE    BK55                ARB=X'00'  NSI=X'40'  SRC=X'41'              
         MVI   WORK,X'40'                                                       
         CLI   SVSOURCE,C'N'                                                    
         BE    BK55                                                             
         OI    WORK,X'01'                                                       
BK55     ZIC   R5,NUMBOOK          R5=LOOP CONTROL                              
         LA    R4,BOOK             R4=A(NEXT BOOK)                              
BK60     MVC   WORK+1(1),0(R4)                                                  
         NI    WORK+1,X'41'                                                     
         CLC   WORK(1),WORK+1                                                   
         BNE   BK90                DIFFERENT SOURCE                             
         LA    R4,4(R4)                                                         
         BCT   R5,BK60                                                          
BK65     DS    0H                                                               
         B     STA00                                                            
*                                                                               
BK90     MVC   CONHEAD(L'BADBK2),BADBK2                                         
         B     MYEND                                                            
         SPACE 1                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *           STATION              *           
*                                  *                                *           
*                                  **********************************           
STA00    LA    R2,MASSTNH          VALIDATE STATION                             
         MVI   STATSW,C'I'         STATION SWITCH - ALLOW INVALID STA           
         GOTO1 VVALSTA                                                          
         CLI   STATSW,C'I'         WAS STATION INVALID                          
         BE    STA02                                                            
*                                                                               
         MVC   STATSV(5),ACTSTAT                                                
         GOTO1 VVALMKT             GET MARKET NAME                              
* DON'T USE DEMO MARKET NAME                                                    
*******  MVC   MKTSV(29),WORK+8                                                 
* GET IT NOW FROM STATION RECORD                                                
STA02    EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(5),ACTSTAT                                                
         CLI   KEY+26,C'T'                                                      
         BE    STA05                                                            
         CLI   KEY+26,X'F0'                                                     
         BL    STA10                                                            
         CLI   KEY+26,X'F9'                                                     
         BH    STA10                                                            
STA05    MVI   KEY+26,C' '                                                      
STA10    GOTO1 HIGH                                                             
         MVI   ERROR,112          STATION RECORD NOT ON FILE                    
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         GOTO1 GETREC                                                           
         LA    R6,IO                                                            
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   DEMO00                                                           
         MVC   MKTSV(20),2(R6)    MARKET NAME                                   
         EJECT                                                                  
*                                  **********************************           
*                                  *                                *           
*                                  *     DEMOS (& DEMO MENU)        *           
*                                  *                                *           
*                                  **********************************           
DEMO00   LA    R2,MASDEMH          VALIDATE DEMOS                               
         MVI   NFLDS,1             SET FOR 1 DEMO FIELDS                        
         MVI   ERROR,2             INVALID INPUT FIELD                          
         CLC   8(3,R2),=C'ALL'     DEFAULT MENU                                 
         BE    DEM10                                                            
         CLC   8(2,R2),=C'M='      OR SPECIFIC MENU                             
         BNE   DEM50                                                            
         CLI   5(R2),4             MENU IS 2 A/N CHARACTERS                     
         BE    DEM10                                                            
         MVC   CONHEAD(L'BADMENU),BADMENU                                       
         B     MYEND                                                            
         SPACE 1                                                                
DEM10    XC    KEY,KEY             VALIDATE DEMO MENU                           
         MVI   KEY,X'23'                                                        
         MVC   KEY+23(2),AGENCY    USE MAIN REP (NOT PARENT REP)                
         MVC   KEY+25(2),=C'ZZ'    DEFAULT MENU                                 
         CLC   8(3,R2),=C'ALL'                                                  
         BE    *+10                                                             
         MVC   KEY+25(2),10(R2)    OR CHOSEN MENU                               
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         GOTO1 GETREC                                                           
         LA    R6,IO                                                            
         USING RDEMREC,R6                                                       
         MVC   NUMDEMS,RDEMNUM     SAVE NUMBER OF DEMOS                         
         DROP  R6                                                               
         CLI   NUMDEMS,7                                                        
         BNH   *+8                                                              
         MVI   NUMDEMS,7                                                        
*                                                                               
         MVI   ELCODE,X'02'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         USING RDEMDEL,R6                                                       
         MVC   DEMLST,RDEMDEM      SAVE DEMOS                                   
         B     DEMVALX                                                          
         DROP  R6                                                               
DEM50    MVI   MAX,19              DUMMY SO I CAN DO ERROR MSG                  
*                                                                               
*        ALWAYS ALLOW UP TO 24 DEMOS-DISPLAY MAY LOOK ODD                       
*                                                                               
         MVI   MAX,26              DUMMY SO I CAN DO ERROR MSG                  
         GOTO1 VVALDEM                                                          
         CLI   ACTUAL,7            REALLY ALLOW 7 DEMOS HERE                    
         BNH   DEM60                                                            
         B     DEM55                                                            
DEM52    DS    0H                                                               
         CLI   ACTUAL,7            REALLY ALLOW 7 DEMOS HERE                    
         BNH   DEM60                                                            
DEM55    DS    0H                                                               
         MVC   CONHEAD(L'MANYDEM),MANYDEM                                       
         B     MYEND                                                            
         SPACE 1                                                                
DEM60    MVC   NUMDEMS,ACTUAL      SAVE NUMBER OF DEMOS                         
         MVC   DEMLST,DEMOS                                                     
DEMVALX  DS    0H                                                               
         TITLE 'T81935 --- RERES35 --- REFETCH TEST DPTVAL'                     
*                                  **********************************           
*                                  *                                *           
*                                  *          DAYPART(S)            *           
*                                  *                                *           
*                                  **********************************           
DPTVAL   LA    R2,MASDPTH          VALIDATE DAYPART                             
*                                                                               
         XC    SVDTMS,SVDTMS       INIT DAY/TIMES SAVEAREA                      
*                                                                               
         XC    DPLIST,DPLIST       INIT DAYPART LIST                            
         XC    DPMENU,DPMENU       INIT DAYPART MENU CODE                       
*                                                                               
         CLI   5(R2),0             IF NOT ENTERED                               
         BE    *+10                                                             
         CLC   8(3,R2),=C'ALL'     OR ALL                                       
         BNE   *+14                                                             
         MVC   DPMENU,=C'ALL '        USE MENU 'ALL '                           
         B     DPTMENU                                                          
*                                                                               
         CLC   =C'M=',8(R2)        MENU IF IT STARTS 'M='                       
         BNE   DPT05                                                            
*                                                                               
         MVC   DPMENU,10(R2)       SAVE MENU CODE                               
         OC    DPMENU,SPACES       SPACE FILL                                   
*                                                                               
         B     DPTMENU                                                          
*                                                                               
DPT05    DS    0H                                                               
*                                                                               
         ZIC   RF,5(R2)            NUMBER OF DAYPARTS                           
         STC   RF,NOREQDPT         SAVE NUMBER OF REQUESTED DAYPARTS            
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DPLIST(0),8(R2)     SAVE DAYPART LIST                            
*                                                                               
         B     DPTMENUX                                                         
*                                                                               
*        READ SET RECORD FOR DAYPART MENU                                       
*                                                                               
DPTMENU  DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH SET RECORD KEY                     
         USING RSETKEY,R4                                                       
*                                                                               
         MVI   RSETKTYP,RSETKTYQ   SET AS SET RECORD RECORD                     
         MVC   RSETKREP,AGENCY     SET REP CODE                                 
         MVC   RSETKSET,=C'DP'     SET SET CODE                                 
         MVC   RSETKID,DPMENU      SET SET IDENTIFIER                           
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   DPTMENUE                                                         
*                                                                               
         GOTO1 GETREC              READ IN SET RECORD                           
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,RSETMCDQ     FIND MEMBERS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DPTMENUE            MUST FIND ELEMENT                            
*                                                                               
         USING RSETMEMD,R6         ESTABLISH MEMBERS ELEMENT                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSETMELN         GET ELEMENT LENGTH                           
         SH    RF,=Y(RSETMTOV)     DECREMENT BY OVERHEAD LENGTH                 
         BNP   DPTMENUE            MUST HAVE SOME MEMBERS                       
*                                                                               
         STC   RF,NOREQDPT         SET NUMBER OF DAYPARTS                       
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DPLIST(0),RSETMEMB  COPY DAYPARTS                                
*                                                                               
DPTMENUX DS    0H                                                               
*                                                                               
*        VALIDATE INDIVIDUALLY ENTERED DAYPARTS                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH DAYPART RECORD KEY                 
         USING RRDPKEY,R4                                                       
*                                                                               
         MVI   RRDPKTYP,RRDPKIDQ   SET AS RESEARCH DAYPART RECORD               
         MVC   RRDPKREP,AGENCY     SET REP CODE                                 
*                                                                               
         LA    R5,DPLIST           START OF INPUT                               
         LA    R2,SVDTMS           START OF SAVEAREA                            
*                                                                               
         ZIC   R0,NOREQDPT         # OF REQUESTED DAYPARTS                      
*                                                                               
         LA    R3,DPTBL            ESTABLISH DAYPART TABLE                      
         USING DPTBLD,R3                                                        
         XC    DPTBLD(DPTBLL),DPTBLD   INIT FIRST ENTRY                         
*                                                                               
DPTLOOP  DS    0H                                                               
*                                                                               
         MVC   RRDPKDPT,0(R5)      SET NEXT DAYPART IN KEY                      
         MVC   0(1,R2),0(R5)       SET IN DPT/DAY/TM BLOCK                      
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   DPTINVE                                                          
*                                                                               
         CLI   ACTNUM,ACTREP       READ RECORD IF DOING REPORT                  
         BNE   DPTCONT                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,IO               POINT TO FOUND RECORD                        
         MVI   ELCODE,X'01'        SEARCH FOR DAYPART ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   DPTCONT             IGNORE IF NOT FOUND                          
*                                                                               
         USING RRDPELEM,R6         ESTABLISH DAYPART ELEMENT                    
*                                                                               
         MVC   DPTBCODE,RRDPKDPT   SAVE DAYPART CODE                            
         MVC   DPTBSNAM,RRDPSNAM   SAVE SHORT NAME                              
         MVC   DPTBLNAM,RRDPLNAM   SAVE LONG NAME                               
*                                                                               
         LA    R3,DPTBLL(R3)       BUMP TO NEXT ENTRY IN DPTBL                  
         XC    DPTBLD(DPTBLL),DPTBLD  INIT NEXT ENTRY                           
*                                                                               
DPTCONT  DS    0H                                                               
*                                                                               
         LA    R5,1(R5)            BUMP TO NEXT ENTERED DAYPART                 
         LA    R2,6(R2)            BUMP TO NEXT SAVEAREA                        
         BCT   R0,DPTLOOP                                                       
*                                                                               
DPTDONE  DS    0H                                                               
*                                                                               
         B     DPTVALX                                                          
*                                                                               
DPTMENUE DS    0H                  INVALID DAYPART MENU ID                      
         LA    R2,MASDPTH          VALIDATE DAYPART                             
         MVC   CONHEAD(L'DPMENUER),DPMENUER                                     
         B     MYEND                                                            
*                                                                               
DPTINVE  DS    0H                  INVALID DAYPART                              
         LA    R2,MASDPTH          VALIDATE DAYPART                             
         MVC   CONHEAD(L'DPINVER),DPINVER                                       
         B     MYEND                                                            
*                                                                               
DPTVALX  DS    0H                                                               
*                                                                               
********************************************************************            
*                                                                  *            
*        VALIDATE DAY/TIMES- M-F/1-2P                              *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
DTMVAL   DS    0H                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *               DAY              *           
*                                  *                                *           
*                                  **********************************           
*                                                                               
         LA    R2,MASDTMSH         DAY/TIMES (OPTIONAL)                         
*                                                                               
         CLI   5(R2),0             SKIP IF NO ENTRY                             
         BE    DTMVALX                                                          
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         MVC   DMCB+8(4),=C',=,/'   '/' SEPARATES DAYS AND TIMES                
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(8,BLOCK1)   MAX 8 ENTRIES                     
*                                                                               
         LA    R4,BLOCK1           POINT TO FIRST SCANNER BLOCK                 
*                                                                               
         SR    R0,R0                                                            
         IC    R0,DMCB+4           NUMBER OF ENTRIES                            
*                                                                               
         LA    R5,SVDTMS           POINT TO FETCH DAY/TIME INPUT                
*                                                                               
DTMLOOP  DS    0H                                                               
*                                                                               
         GOTO1 DAYVAL,DMCB,(0(R4),12(R4)),1(R5),FULL   VALIDATE DAY             
         CLI   0(R5),0             MUST HAVE A RESULT                           
         BE    DTMDYNVE                                                         
*                                                                               
         CLI   1(R4),0             SKIP IF NO TIME ENTERED                      
         BE    DTMLPCN                                                          
*                                                                               
         GOTO1 TIMVAL,DMCB,(1(R4),22(R4)),2(R5)   VALIDATE TIME                 
         CLI   0(R1),X'FF'         CHECK FOR ERRORS                             
         BE    DTMTMNVE                                                         
*                                                                               
DTMLPCN  DS    0H                                                               
*                                                                               
         LA    R5,6(R5)            BUMP TO NEXT DAY/TIME ENTRY                  
         LA    R4,32(R4)           BUMP TO NEXT SCANNER BLOCK                   
         BCT   R0,DTMLOOP                                                       
*                                                                               
DTMLPDN  DS    0H                                                               
*                                                                               
         B     DTMVALX                                                          
*                                                                               
DTMDYNVE DS    0H                  INVALID DAY EXPRESSION                       
         MVC   CONHEAD(L'DPMENUER),DTMDAYE                                      
         B     MYEND                                                            
*                                                                               
DTMTMNVE DS    0H                  INVALID DAY EXPRESSION                       
         MVC   CONHEAD(L'DPMENUER),DTMTIME                                      
         B     MYEND                                                            
*                                                                               
DTMVALX  DS    0H                                                               
*                                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *             FILTER             *           
*                                  *                                *           
*                                  **********************************           
FTR      LA    R2,MASFTRH          FILTER (OPTIONAL)                            
         CLI   5(R2),0                                                          
         BE    INV                                                              
         MVC   NUMFILT,5(R2)       SAVE NUMBER OF FILTERS                       
         CLI   5(R2),6             CAN HAVE UP TO 6                             
         BNH   INV                                                              
         MVC   CONHEAD(L'MANYFLT),MANYFLT                                       
         B     MYEND                                                            
         SPACE 1                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *        INVENTORY NUMBER        *           
*                                  *                                *           
*                                  **********************************           
INV      DS    0H                  INVENTORY NUMBER (OPTIONAL)                  
         MVC   INVNO,SPACES                                                     
         LA    R2,MASINVH                                                       
         CLI   5(R2),0                                                          
         BE    INVX                                                             
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          INVOICE NUMBER LENGTH                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   INVNO(0),8(R2)      SAVE INVOICE NUMBER AND SPACE FILL           
*                                                                               
INVX     DS    0H                                                               
*                                                                               
         EJECT                                                                  
*                                  **********************************           
*                                  *                                *           
*                                  *     EFFECTIVE START DATE       *           
*                                  *                                *           
*                                  **********************************           
ESDT10   LA    R2,MASESDTH          EFFECTIVE START DATE (OPTIONAL)             
         CLI   5(R2),0                                                          
         BE    EEDT10                                                           
         GOTO1 VVALDATE                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,ESDT)   ESDT=2 BYTE COMPRESSED           
         SPACE 1                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *       EFFECTIVE END DATE       *           
*                                  *                                *           
*                                  **********************************           
EEDT10   LA    R2,MASEEDTH          EFFECTIVE END DATE (OPTIONAL)               
         MVC   EEDT,=X'FFFF'                                                    
         CLI   5(R2),0                                                          
         BE    EEDTX                                                            
         GOTO1 VVALDATE                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,EEDT)   EEDT=2 BYTE COMPRESSED           
*                                                                               
         CLC   ESDT,EEDT           START CANNOT BE GREATER THAN END             
         BH    ERREND                                                           
         SPACE 1                                                                
EEDTX    DS    0H                                                               
         EJECT                                                                  
*              CALL REFETCH AND DISPLAY RESULTS                                 
*              --------------------------------                                 
         SPACE 2                                                                
DREC     DS    0H                                                               
*                                                                               
         LA    R6,RFTBLKC          ESTABLISH RFTBLK                             
         USING RFTBLKD,R6                                                       
*                                                                               
         MVC   DMCB+4(4),=X'D9000AA4' LOAD REFETCH                              
         GOTO1 CALLOV,DMCB,0                                                    
         MVC   VFETCH,DMCB         SAVE ADDRESS                                 
*                                                                               
*        INITIALIZE RFTBLK                                                      
*                                                                               
         MVC   RFTACOM,ACOMFACS    PASS COMFACS ADDRESS                         
         MVC   RFTAIO1,AIO1                                                     
         MVC   RFTAIO2,AIO2                                                     
         LH    RF,=Y(BUFF+1000-SYSD)                                            
         LA    RF,SYSD(RF)                                                      
         ST    RF,RFTAWRK                                                       
*                                                                               
         LA    R1,HOOK                                                          
         ST    R1,RFTHOOKA         PASS HOOK ADDRESS                            
*                                                                               
         MVI   RFTAMODE,RFTAINVQ   SET TO FIND INVENTORY RECS                   
         MVI   RFTCNTL,RFTCHDRQ+RFTCDEMQ+RFTCFTNQ+RFTCTXTQ+RFTCSLVQ             
*                                                                               
         MVC   RFTCREP,AGENCY      PASS REP CODE                                
         MVC   RFTCSTAT,ACTSTAT    PASS STATION                                 
         MVI   RFTCSRC,C'N'        ALWAYS NIELSEN                               
*                                                                               
         LA    R5,SVDTMS           SAVEAREA                                     
         USING SVDTMS,R5                                                        
*                                                                               
         LA    RF,RFTCDTMS         SAVEAREA                                     
         USING RFTCDTMS,RF                                                      
*                                                                               
DRDTMLP  DS    0H                                                               
*                                                                               
         CLI   SVDTMS,C' '         EOT                                          
         BNH   DRDTMDN                                                          
*                                                                               
         MVC   RFTCDTDP,SVDTMS     PASS DAYPART                                 
         MVC   RFTCDTDY,SVDTMS+1   PASS DAY                                     
         MVC   RFTCDTST,SVDTMS+2   PASS TIMES                                   
         MVC   RFTCDTEN,SVDTMS+4   PASS TIMES                                   
*                                                                               
DRDTMCN  DS    0H                                                               
*                                                                               
         LA    R5,6(R5)                                                         
         LA    RF,RFTCDTLQ(RF)                                                  
         B     DRDTMLP                                                          
*                                                                               
DRDTMDN  DS    0H                                                               
*                                                                               
         DROP  R5,RF                                                            
*                                                                               
         MVI   RFTCDCTL,RFTCDC1Q   REPORT INV IN PRIME DPT                      
         MVC   RFTCINV,INVNO       PASS INVENTORY NUMBER                        
         MVC   RFTCEFST,ESDT       START EFFECTIVE DATE                         
         MVC   RFTCEFEN,EEDT       END   EFFECTIVE DATE                         
         MVC   RFTCFTRS,MASFTR     FILTERS                                      
         MVC   RFTCDEMS(21),DEMLST DEMOS                                        
         MVC   RFTCBKS,BOOK        BOOKS                                        
         MVI   RFTCTXTT,RFTCTXIQ   INVENTORY TEXT                               
         MVI   RFTCTXTW,60         TEXT WIDTH                                   
         MVI   RFTCUBBS,X'40'      UPGRADE SHR BOOK SOURCE                      
         LA    RF,UPGRADE                                                       
         ST    RF,RFTCUPGA                                                      
*                                                                               
         GOTO1 VFETCH,DMCB,RFTBLKD    CALL REFETCH                              
*                                                                               
DRECX    DS    0H                                                               
         B     XIT                 ALL DONE                                     
UPGRADE  DC    X'050E800003D75F055F0B000000000000'                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE RESVCTAB                                                       
         EJECT                                                                  
UPPRNT   NTR1                                                                   
         LA    R6,IO                                                            
         MVI   ELCODE,X'05'        ANY UPGRADE ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         CLC   SAVECODE,=C'PJ'     ONLY SHOW UPGRADE IF                         
         BNE   XIT                 CODE IS PJ                                   
******   L     R3,ABUFF                                                         
         LA    R2,28(R3)                                                        
         GOTO1 AUPOUT,DMCB,(R6),(R2)    EDIT UPGRADE EXPRESSION                 
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS HOOKS FROM REFETCH                                       
*              --------------------------                                       
         SPACE 2                                                                
HOOK     NTR1                                                                   
         LA    R6,RFTBLKC          ESTABLISH RFTBLK                             
         USING RFTBLKD,R6                                                       
*                                                                               
         TM    RFTMODE,RFTNINVQ    NEW INVENTORY RECORD                         
         BO    HINV                                                             
         TM    RFTMODE,RFTNBKQ     NEW DEMO BOOK                                
         BO    HBK                                                              
         TM    RFTMODE,RFTNTXTQ    NEW TEXT BOOK                                
         BO    HTXT                                                             
         DC    H'0'                                                             
         B     HOOKX                                                            
*                                                                               
HINV     DS    0H                  NEW INVENTORY REC - DISPLAY HEADER           
*                                                                               
         MVC   MASSTNF(5),RFTCSTAT STATION CALL LETTERS                         
         OI    MASSTNFH+6,X'80'    FORCE RE-DISPLAY                             
*                                                                               
         MVC   MASINVF(4),RFTFINV  INVENTORY ID                                 
         OI    MASINVFH+6,X'80'    FORCE RE-DISPLAY                             
*                                                                               
         MVC   MASDPTF(6),RFTFDPTS DAYPARTS FOUND                               
         OI    MASDPTFH+6,X'80'    FORCE RE-DISPLAY                             
*                                                                               
         MVC   MASFTRF(6),RFTFFTRS FILTERS  FOUND                               
         OI    MASFTRFH+6,X'80'    FORCE RE-DISPLAY                             
*                                                                               
         GOTO1 DATCON,DMCB,(2,RFTFEFST),(17,MASESDF)  EFF START                 
         OI    MASESDFH+6,X'80'    FORCE RE-DISPLAY                             
*                                                                               
         GOTO1 DATCON,DMCB,(2,RFTFEFEN),(17,MASEEDF)  EFF START                 
         OI    MASEEDFH+6,X'80'    FORCE RE-DISPLAY                             
*                                                                               
         MVC   MASSRCF(1),RFTCSRC  SOURCE                                       
         OI    MASSRCFH+6,X'80'    FORCE RE-DISPLAY                             
*                                                                               
*        PROGRAM DAY/TIMES                                                      
*                                                                               
         XC    MASDTMF,MASDTMF     INIT OUTPUT AREA                             
         LA    R3,MASDTMF          STARTING PRINT POSITION                      
         OI    MASDTMFH+6,X'80'    FORCE RE-DISPLAY                             
         LA    R2,L'MASDTMF        INIT MAX LENGTH COUNTER                      
*                                                                               
         LA    R5,RFTFDTMS         POINT TO FETCHED DAY-TIMES                   
         LA    R0,8                MAX NUMBER OF DAYTIMES                       
*                                                                               
         OC    RFTFDTMS,RFTFDTMS   SKIP IF NONE RETURNED                        
         BZ    HINVDTX                                                          
*                                                                               
HINVDTLP DS    0H                                                               
*                                                                               
         OC    0(5,R5),0(R5)       DONE AT END OF LIST                          
         BZ    HINVDTDN                                                         
*                                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 UNDAY,PARAS,(1,0(R5)),WORK       DAY                             
*                                                                               
         LA    RE,20               MAX LENGTH OF DAY EXPRESSION                 
         LA    RF,WORK+19          FIND LAST PRINTABLE CHARACTER                
*                                                                               
         CLI   0(RF),C' '                                                       
         BH    *+14                                                             
         BCTR  RF,0                                                             
         BCT   RE,*-10                                                          
         B     *+8                 NO DATA TO PRINT                             
         LA    RF,2(RF)            NEXT PRINT POSITION                          
*                                                                               
         LA    RE,WORK             CALCULATE EXPRESSION LENGTH                  
         SR    RF,RE                                                            
*                                                                               
         CR    RF,R2               STOP IF TOO BIG                              
         BH    HINVDTDN                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK        DISPLAY DAYS                                 
*                                                                               
         LA    RF,1(RF)            RESET LENGTH COUNTER                         
*                                                                               
         LA    R3,0(RF,R3)         NEXT DISPLAY POSITION                        
*                                                                               
         SR    R2,RF               REMAINING SPACE                              
         BNP   HINVDTDN            NO MORE ROOM                                 
*                                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 UNTIME,PARAS,(4,1(R5)),WORK     TIME                             
*                                                                               
         LA    RE,20               MAX LENGTH OF TIME EXPRESSION                
         LA    RF,WORK+19          FIND LAST PRINTABLE CHARACTER                
*                                                                               
         CLI   0(RF),C' '                                                       
         BH    *+14                                                             
         BCTR  RF,0                                                             
         BCT   RE,*-10                                                          
         B     *+12                NO DATA TO PRINT                             
         MVI   1(RF),C','          SET SEPARATOR                                
         LA    RF,2(RF)            NEXT PRINT POSITION                          
*                                                                               
         LA    RE,WORK             CALCULATE EXPRESSION LENGTH                  
         SR    RF,RE                                                            
*                                                                               
         CR    RF,R2               STOP IF TOO BIG                              
         BH    HINVDTDN                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK        DISPLAY DAYS                                 
*                                                                               
         LA    RF,1(RF)            RESET LENGTH COUNTER                         
*                                                                               
         LA    R3,0(RF,R3)         NEXT DISPLAY POSITION                        
*                                                                               
         SR    R2,RF               REMAINING SPACE                              
         BNP   HINVDTDN            NO MORE ROOM                                 
*                                                                               
HINVDTCN DS    0H                                                               
*                                                                               
         LA    R5,5(R5)            NEXT DAY/TME ELEMENT                         
         BCT   R0,HINVDTLP                                                      
*                                                                               
HINVDTDN DS    0H                  END OF DAY/TIME ELEMENTS                     
*                                                                               
         BCTR  R3,0                BACK UP A POSITION                           
         CLI   0(R3),C','          IF THERE IS A TRAILING COMMA                 
         BNE   *+8                                                              
         MVI   0(R3),C' '             ELIMINATE IT                              
*                                                                               
HINVDTX  DS    0H                                                               
*                                                                               
*        PRINT PROGRAM NAMES                                                    
*                                                                               
         XC    MASPGMF,MASPGMF     INIT PROGRAMS OUTPUT AREA                    
         LA    R3,MASPGMF          PROGRAMS OUTPUT AREA                         
         OI    MASPGMFH+6,X'80'    FORCE RE-DISPLAY                             
         LA    R2,L'MASPGMF        MAX OUTPUT LENGTH                            
*                                                                               
         OC    RFTFPGMS,RFTFPGMS   SKIP IF NO PROGRAMMING                       
         BZ    HINVPGX                                                          
*                                                                               
         LA    R5,RFTFPGMS         RETURNED PROGRAMS                            
         LA    R0,8                MAX NUMBER OF RETURNED PROGRAMS              
*                                                                               
HINVPGLP DS    0H                                                               
*                                                                               
         OC    0(27,R5),0(R5)      DONE IF NO MORE NAMES                        
         BZ    HINVPGDN                                                         
*                                                                               
         LA    RF,26(R5)           END OF PROGRAM                               
         LA    RE,27               MAX PROGRAM LENGTH                           
*                                                                               
         CLI   0(RF),C' '          FIND LAST PRINTABLE CHARACTER                
         BH    *+14                                                             
         BCTR  RF,0                                                             
         BCT   RE,*-10                                                          
         B     HINVPGCN                                                         
*                                                                               
         LA    RF,1(RF)            RECTIFY PROGRAM LENGTH                       
*                                                                               
         SR    RF,R5               PROGRAM LENGTH                               
*                                                                               
         CR    RF,R2               CAN'T EXCEED ROOM LEFT                       
         BNH   *+6                                                              
         LR    RF,R2                                                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R5)       PRINT PROGRAM NAME                           
*                                                                               
         LA    RF,1(RF)            RESTORE LENGTH POINTER                       
*                                                                               
         LA    R3,0(RF,R3)         NEXT PRINT POSITION                          
*                                                                               
         SR    R2,RF               REMAINING ROOM LEFT                          
         BNP   HINVPGDN            NO ROOM LEFT                                 
*                                                                               
         MVI   0(R3),C','          DATA SEPARATOR                               
         LA    R3,2(R3)            BUMP POINTER                                 
*                                                                               
         SH    R2,=H'2'            DECREMENT LENGTH LEFT COUNTER                
         BNP   HINVPGDN            NO ROOM LEFT                                 
*                                                                               
HINVPGCN DS    0H                                                               
*                                                                               
         LA    R5,27(R5)           NEXT PROGRAM NAME                            
         B     HINVPGLP                                                         
*                                                                               
HINVPGDN DS    0H                                                               
*                                                                               
         BCTR  R3,0                BACK UP A POSITION                           
         BCTR  R3,0                BACK UP A POSITION                           
         CLI   0(R3),C','          IF THERE IS A TRAILING COMMA                 
         BNE   *+8                                                              
         MVI   0(R3),C' '             ELIMINATE IT                              
*                                                                               
*                                                                               
HINVPGX  DS    0H                                                               
*                                                                               
HINVX    DS    0H                                                               
         B     HOOKX                                                            
*                                                                               
         DROP  R3                                                               
*                                                                               
         TITLE 'RERES35 - MASTER REPORT - HBK'                                  
***********************************************************************         
*                                                                     *         
*        PRINT DEMOS AND BOOK                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HBK      DS    0H                                                               
*                                                                               
*              FORMAT BOOK NAME                                                 
*              ----------------                                                 
*                                                                               
         LA    R3,MASBKSF          BOOK DISPLAY AREA                            
         OI    MASBKSFH+6,X'80'    FORCE TRANSMISSION                           
*                                                                               
         MVC   0(1,R3),RFTFBOOK    PRECEDE WITH P,T,S OR E                      
*                                                                               
         LA    R3,1(R3)                                                         
*                                                                               
         CLI   RFTFBOOK+2,0    UNSPECIFIED MONTH                                
         BNE   BKSQ70                                                           
*                                                                               
         BCTR  R3,0                                                             
         MVC   0(3,R3),=C'EST'     E BECOMES EST                                
*                                                                               
         B     BKSQ80                                                           
*                                                                               
BKSQ70   ZIC   R1,RFTFBOOK+2                                                    
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,CMONTHS(R1)                                                   
*                                                                               
         MVC   0(3,R3),0(R1)                                                    
*                                                                               
BKSQ80   MVI   3(R3),C'/'                                                       
*                                                                               
         LA    RF,RFTFBOOK+1       POINT TO YYMM OF BOOK                        
*                                                                               
         EDIT  (1,(RF)),(2,4(R3)),WRK=DMCB                                      
*                                                                               
         EJECT                                                                  
*              --------------------                                             
*              BUILD DEMO HEADLINES                                             
*              --------------------                                             
*                                                                               
         XC    MASDEMT,MASDEMT     INIT DISPLAY FIELD                           
         OI    MASDEMTH+6,X'80'    FORCE RE-TRANSMISSION                        
*                                                                               
         LA    R3,MASDEMT          POINT TO WHERE 1ST DEMO SHOULD PRINT         
*                                                                               
         LA    R2,DEMLST           LIST OF DEMOS                                
         LA    R0,7                MAX 7 DEMOS                                  
*                                                                               
HBKDMTLP DS    0H                                                               
*                                                                               
         OC    0(3,R2),0(R2)       DONE IF END OF LIST REACHED                  
         BZ    HBKDMTDN                                                         
*                                                                               
         LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
*                                                                               
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
*                                                                               
         CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
*                                                                               
         GOTO1 DEMOCON,PARAS,(0,(R2)),(6,WORK),(0,DBLOCKD)                      
*                                                                               
         CLI   WORK+1,C'*'         CHECK FOR END OF LIST                        
         BE    HBKDMTDN                                                         
*                                                                               
         CLI   1(R2),C'I'          RESET FOR NEXT TIME                          
         BNE   *+8                                                              
         MVI   1(R2),C'T'                                                       
*                                                                               
         MVC   0(6,R3),WORK        FORMAT - SW1849                              
*                                                                               
         CLC   1(2,R2),=C'D702'    PUT=HUT FOR MET                              
         BNE   *+8                                                              
         MVI   0(R3),C'H'                                                       
         CLC   1(2,R2),=X'D703'    OR METB                                      
         BNE   *+8                                                              
         MVI   0(R3),C'H'                                                       
*                                                                               
HBKDMTCN DS    0H                                                               
*                                                                               
         LA    R2,3(R2)                                                         
         LA    R3,7(R3)                                                         
*                                                                               
         BCT   R0,HBKDMTLP                                                      
*                                                                               
HBKDMTDN DS    0H                                                               
*                                                                               
*        PRINT DEMO VALUES                                                      
*                                                                               
         LA    R5,RFTFDEMS         DEMO VALUES TO DISPLAY                       
*                                                                               
         LA    R3,MASDEMF          DEMO DISPLAY AREA                            
         OI    MASDEMFH+6,X'80'    FORCE TRANSMISSION                           
         LA    R2,DEMLST           LIST OF DEMOS                                
*                                                                               
         LA    R4,7                MAX DEMOS                                    
*                                                                               
HBKDEMLP DS    0H                                                               
*                                                                               
         ICM   R1,15,0(R5)         GET NEXT NEXT DEMO                           
         BZ    HBKDEMCN              SKIP IF NO DEMO AVAILABLE                  
*                                                                               
         EDIT  (R1),(6,0(R3)),1                                                 
*                                                                               
HBKDEMCN DS    0H                                                               
*                                                                               
         LA    R2,3(R2)            NEXT DEMO                                    
         LA    R3,7(R3)            NEXT DEMO PRINTING AREA                      
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         BCT   R4,HBKDEMLP         LOOP FOR MORE DEMOS                          
*                                                                               
*        PRINT SHARE VALUES                                                     
*                                                                               
         LA    R5,RFTFSHRS         SHARE VALUES TO DISPLAY                      
*                                                                               
         LA    R3,MASSHRS          SHARES DISPLAY AREA                          
         OI    MASSHRSH+6,X'80'    FORCE TRANSMISSION                           
         LA    R2,DEMLST           LIST OF DEMOS                                
*                                                                               
         LA    R4,7                MAX DEMOS                                    
*                                                                               
HBKSHRLP DS    0H                                                               
*                                                                               
         ICM   R1,15,0(R5)         GET NEXT NEXT DEMO                           
         BZ    HBKSHRCN              SKIP IF NO DEMO AVAILABLE                  
*                                                                               
         EDIT  (R1),(6,0(R3)),1                                                 
*                                                                               
HBKSHRCN DS    0H                                                               
*                                                                               
         LA    R2,3(R2)            NEXT DEMO                                    
         LA    R3,7(R3)            NEXT SHARE PRINTING AREA                     
         LA    R5,4(R5)            NEXT SHARE VALUE                             
         BCT   R4,HBKSHRLP         LOOP FOR MORE DEMOS                          
*                                                                               
*        PRINT LEVEL VALUES                                                     
*                                                                               
         LA    R5,RFTFLVLS         LEVEL VALUES TO DISPLAY                      
*                                                                               
         LA    R3,MASLVLS          LEVELS DISPLAY AREA                          
         OI    MASLVLSH+6,X'80'    FORCE TRANSMISSION                           
         LA    R2,DEMLST           LIST OF DEMOS                                
*                                                                               
         LA    R4,7                MAX DEMOS                                    
*                                                                               
HBKLVLLP DS    0H                                                               
*                                                                               
         ICM   R1,15,0(R5)         GET NEXT NEXT DEMO                           
         BZ    HBKLVLCN              SKIP IF NO DEMO AVAILABLE                  
*                                                                               
         EDIT  (R1),(6,0(R3)),1                                                 
*                                                                               
HBKLVLCN DS    0H                                                               
*                                                                               
         LA    R2,3(R2)            NEXT DEMO                                    
         LA    R3,7(R3)            NEXT LEVEL PRINTING AREA                     
         LA    R5,4(R5)            NEXT LEVEL VALUE                             
         BCT   R4,HBKLVLLP         LOOP FOR MORE DEMOS                          
*                                                                               
HBKX     DS    0H                                                               
         B     HOOKX                                                            
*                                                                               
HTXT     DS    0H                                                               
*                                                                               
         OI    MASTXT1H+6,X'80'    FORCE TRANSMISSION                           
         OI    MASTXT2H+6,X'80'    FORCE TRANSMISSION                           
*                                                                               
         L     R3,RFTFTX1A         POINT TO TEXT                                
         SR    R0,R0                                                            
         ICM   R0,1,RFTFTX1N       NUMBER OF TEXT LINES                         
         BZ    HKTXTX                                                           
*                                                                               
         CLC   0(60,R3),SPACES     FIND TEXT                                    
         BH    *+16                                                             
         LA    R3,132(R3)                                                       
         BCT   R0,*-14                                                          
         B     HKTXTX                                                           
*                                                                               
         MVC   MASTXT1,0(R3)                                                    
*                                                                               
         LA    R3,132(R3)                                                       
*                                                                               
         CLC   0(60,R3),SPACES     FIND TEXT                                    
         BH    *+16                                                             
         LA    R3,132(R3)                                                       
         BCT   R0,*-14                                                          
         B     HKTXTX                                                           
*                                                                               
         MVC   MASTXT2,0(R3)                                                    
*                                                                               
HKTXTX   DS    0H                                                               
*                                                                               
HOOKX    B     XIT                                                              
*                                                                               
         TITLE 'RERES35 - MASTER REPORT - PFKEYS'                               
***********************************************************************         
*                                                                     *         
*        ANALYZE PFKEYS                                               *         
*                                                                     *         
*              PF10 - TITLES REPORT                                   *         
*                                                                     *         
*NTRY                                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYS   NTR1                                                                   
*                                                                               
         L     R3,SYSPARMS                                                      
         ICM   R3,15,0(R3)         POINT TO TIOB                                
         USING TIOBD,R3                                                         
*                                                                               
         B     PFKEYSX             IGNORE ALL OTHER KEYS                        
*                                                                               
         TITLE 'RERES35 - REFETCH TEST -RFEXITS'                                
***********************************************************************         
*                                                                     *         
*        EXIT ROUTINES                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYSX  DS    0H                  NORMAL EXIT                                  
         CR    RB,RB               FORCE EQ CC                                  
         XIT1                                                                   
*                                                                               
PFKEYERX DS    0H                  ERROR EXIT                                   
         LTR   RB,RB               FORCE NE CC                                  
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
ERREND   GOTO1 VERRXIT                                                          
         SPACE 2                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         B     XIT                                                              
         SPACE 2                                                                
         GETEL (R6),34,ELCODE                                                   
         SPACE 2                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DC    XL64'00'                                                         
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
         SPACE 2                                                                
*  MY OWN ERROR MESSAGES                                                        
*                                                                               
MANYBKS  DC    C'* ERROR * TOO MANY BOOKS - LIMIT IS 7'                         
MANYDEM  DC    C'* ERROR * TOO MANY DEMOS - LIMIT IS 7'                         
MANYFLT  DC    C'* ERROR * TOO MANY FILTERS - LIMIT IS 6'                       
BADBK1   DC    C'* ERROR * BOOK CAN NOT HAVE PREFIX'                            
BADBK2   DC    C'* ERROR * BOOKS MUST BE THE SAME SOURCE'                       
BADBK3   DC    C'* ERROR * END BOOK LESS THAN START BOOK'                       
BADMENU  DC    C'* ERROR * MENU IS 2 A/N CHARACTERS'                            
DPMENUER DC    C'* ERROR * MENU NOT FOUND'                                      
DPINVER  DC    C'* ERROR * DAYPART NOT FOUND'                                   
DTMDAYE  DC    C'* ERROR * DAY EXPRESSION INVALID'                              
DTMTIME  DC    C'* ERROR * TIME EXPRESSION INVALID'                             
         SPACE 3                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 4                                                                
TYPTAB   DC    C'TTSA '            TSA IMPS                                     
         DC    C'RRTG '            ADI/DMA RTGS                                 
         DC    C'SSHR '            ADI/DMA SHRS                                 
         DC    C'PPUT '            ADI/DMA LVLS (PUTS)                          
         DC    C'CCHR '                                                         
         DC    C'XTSH '            TSA SHRS                                     
         DC    C'QTOT '            TSA LVLS (TOTS)                              
         DC    C'UUNV '            UNVS                                         
         DC    X'FF'                                                            
         DC    C'    '                                                          
*                                                                               
         DROP  R4                                                               
*                                                                               
CMONTHS  DC    C'JANFEBMARAPRMAYJUN'                                            
         DC    C'JULAUGSEPOCTNOVDEC   '                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
       ++INCLUDE RERESWRK                                                       
         EJECT                                                                  
* DSECT TO COVER SCREEN                                                         
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RERESC5D                                                       
         SPACE 1                                                                
* SAVE AREA FIELDS FOLLOWING SCREEN                                             
*                                                                               
       ++INCLUDE REFETCHDA                                                      
RESETRECD      DSECT                                                            
       ++INCLUDE REGENSET                                                       
RERDPRECD      DSECT                                                            
       ++INCLUDE REGENRDP                                                       
         TITLE 'T81935 --- RERES35 --- REFETCH TEST-SYSSPARE'                   
***********************************************************************         
*                                                                     *         
*        SAVE AREA VALUES                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE            LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
VFETCH   DS    A                   SPARE                                        
*                                                                               
SBK      DS    CL4                 START BOOK IN RANGE                          
EBK      DS    CL4                 END BOOK IN RANGE                            
*                                                                               
DPLIST   DS    CL20                DAYPART LIST                                 
SVDTMS   DS    CL(8*6)             DPT/DAY/TIMES LIST                           
*                                                                               
ESDT     DS    XL2                 EFFECTIVE START DATE (COMPRESSED)            
EEDT     DS    XL2                 EFFECTIVE END DATE (COMPRESSED)              
*                                                                               
INVNO    DS    CL4                 INVENTORY FILTER                             
SAVEKSRC DS    CL1                 SAVE RINVKSRC                                
XTODAY   DS    XL2                 TODAY'S DATE (COMPRESSED)                    
SAVECODE DS    CL2                 RINVCODE SAVED FOR PRINTING                  
NUMFILT  DS    XL1                 NUMBER OF FILTER CODES                       
*                                                                               
DEMLST   DS    CL73                DEMO LIST...ALLOW 24 DEMOS + X'FF'           
SVDEMLST DS    CL73                DEMO LIST...SAVE AREA                        
*                                                                               
REPPAR   DS    CL2                 PARENT REP                                   
AUPOUT   DS    A                   REUPOUT                                      
SVLST    DS    CL10                VALID ARB, NSI OR SRC SOURCE CODES           
RNGLST   DS    CL210               LIST OF BOOKS WITHIN RANGE                   
*                                     7 BYTES X 30 BOOKS MAX                    
*                                        2 BYTES RINVKBK                        
*                                        1 BYTE RINVKSRC                        
*                                        4 BYTES DISK ADDRESS                   
RNGLSTND DS    CL1                 END OF TABLE                                 
INVSRC   DS    XL1                 1ST BYTE FROM BOOKVAL                        
ACTDEMOS DS    CL96                24 DEMO VALUES (4 BYTES EACH)                
NOREQDPT DS    XL1                 # OF REQUESTED DAYPARTS                      
SKIPREC  DS    XL1                 FLAG TO SKIP RECORD OR NOT                   
OPTFLAG  DS    XL1                 FLAG INDICATE Y/N TO OPTION 3                
*                                                                               
DPMENU   DS    CL4                 DAYPART MENU CODE                            
*                                                                               
         DS    0F                                                               
DPTBL    DS    XL(24*DPTBLL)       DAYPART TABLE                                
*                                                                               
         DS    0D                                                               
RFTBLKC  DS    XL1536              RFTBLK AREA                                  
*                                                                               
         TITLE 'T81935 --- RERES35 --- REFETCH TEST-DPTBL'                      
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF DAYPART CODES AND NAMES                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DPTBLD   DSECT                                                                  
DPTBCODE DS    CL(L'RRDPCODE)      DAYPART CODE                                 
DPTBSNAM DS    CL(L'RRDPSNAM)      DAYPART SHORT NAME                           
DPTBLNAM DS    CL(L'RRDPLNAM)      DAYPART LONG NAME                            
DPTBLL   EQU   *-DPTBLD            LENGTH OF TABLE ENTRY                        
*                                                                               
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'114RERES35   05/01/02'                                      
         END                                                                    
