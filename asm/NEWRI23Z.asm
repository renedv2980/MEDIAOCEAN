*          DATA SET NEWRI23Z   AT LEVEL 104 AS OF 08/30/00                      
*PHASE T32023A                                                                  
*INCLUDE NETWEEK                                                                
         TITLE 'T32023 - DEMOSEED REPORT  PHASE'                                
T32023   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE23**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD        * ANETWS1+2 WORK AREAS                         
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R6,ANETWS4        * ANETWS3 = CLIST                              
         USING NDDEMBLK,R6                                                      
         ST    R6,NBADEM                                                        
         L     R7,ANETWS1        * ANETWS4 = NDDEMBLK                           
         USING MYD,R7                                                           
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         BAL   RE,REPMOD                                                        
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
*              REPORT INITIALIZATION                                            
         SPACE 3                                                                
REPMOD   NTR1                                                                   
*******************************************************                         
* SET UP YR/WK LIST FROM NTI REQUESTED START/END DATES*                         
*******************************************************                         
         XC    YRWKLST,YRWKLST     CLEAR LIST                                   
*                                                                               
         LA    R1,ACCUMLST         PREPARE PACKED ACCUMLST                      
         LA    R2,25               MAX # OF DEMOS IN LIST                       
REP08    ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R2,REP08                                                         
*                                                                               
         LA    R3,YRWKLST                                                       
         LA    R4,NWEEKS           MAX NTI WEEKS                                
         GOTO1 DATCON,DMCB,NTIEND,WORK+12    CONVERT TO DDS DATE                
         MVC   WORK(6),NTISTART                                                 
REP10    GOTO1 =V(NETWEEK),DMCB,WORK,NBGETDAY,NBADDAY                           
         MVC   0(1,R3),4(R1)                           NTI YEAR                 
         MVC   1(1,R3),8(R1)                           NTI WEEK                 
         GOTO1 NBADDAY,DMCB,WORK,(X'20',WORK),F'7'     BUMP DATE                
         GOTO1 DATCON,DMCB,WORK,WORK+6       CONVERT TO DDS DATE                
         CLC   WORK+6(6),WORK+12             GREATER THAN END?                  
         BH    REP12                         YES                                
         LA    R3,2(R3)                      NO/BUMP YR/WK LIST                 
         BCT   R4,REP10                                                         
REP12    EQU   *                                                                
*                                                                               
         MVI   NBDATA,C'U'         UNITS ONLY                                   
REP20    DS    0H                                                               
         NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBPROCUN                                                  
         BE    REP30                                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    REP200                                                           
         B     REP20                                                            
REP30    CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         BAL   RE,INITDB           INITIALIZE DBLOCK                            
         B     REP35                                                            
         EJECT                                                                  
**********************************************************                      
* PREPARE FOR NDDEMOS BY REMOVING DEMO CATEGORIES ON UNIT*                      
* FROM LIST OF DEMOS TO LOOK UP                          *                      
**********************************************************                      
REP35    DS    0H                                                               
         XC    DEMWORK,DEMWORK                                                  
         MVC   DEMWORK(78),SVDEMOS     SVDEMOS HAS ESTIMATE DEMOS               
         MVI   ELCODE,X'DD'        GET OVERRIDE CATEGORIES                      
         L     R2,NBAIO                                                         
         USING NUOVEL,R2                                                        
         BAL   RE,GETEL                                                         
         BE    REP50                                                            
         DC    H'0'                MUST BE AT LEAST ONE                         
REP45    BAL   RE,NEXTEL                                                        
         BNE   REP60                                                            
         BE    REP50                                                            
                                                                                
REP50    DS    0H                                                               
         LA    R3,DEMWORK                                                       
REP52    CLC   2(1,R3),NUOVNUM     DOES IT MATCH DEMO NUMBER?                   
         BE    REP54                                                            
         LA    R3,3(R3)            BUMP TO NXT DEMO IN LIST                     
         OC    0(3,R3),0(R3)                                                    
         BNZ   REP52               END OF LIST?                                 
         DC    H'0'                SHOULD ALWAYS FIND MATCH                     
REP54    CLI   NUOVNUM,1           IF HOMES?                                    
         BE    REP45               YES-LET IT BE                                
         MVC   0(3,R3),=3X'FF'     ELSE REMOVE IT                               
         B     REP45               AND GET NEXT ELEMENT                         
                                                                                
* MOVE VALID CATEGORIES TO NDDEMOS                                              
REP60    LA    R2,NDDEMOS                                                       
         LA    R3,DEMWORK                                                       
         SR    R1,R1               NDNDEMOS COUNT                               
REP62    CLC   0(3,R3),=3X'FF'                                                  
         BE    REP64                                                            
         MVC   0(3,R2),0(R3)       SET DEMO TO NDDEMOS                          
         LA    R1,1(R1)            AND BUMP NDNDEMOS                            
REP64    LA    R2,3(R2)            BUMP NDDEMOS                                 
         LA    R3,3(R3)            BUMP SVDEMO2                                 
         OC    0(3,R3),0(R3)       EOF?                                         
         BNZ   REP62               NO                                           
         STC   R1,NDNDEMOS         YES/SET NUMBER TO NDNDEMOS                   
         CLI   NDNDEMOS,0          ARE ALL DEMOS SEEDED?                        
         BE    REP20               YES/GET NEXT UNIT                            
*                                                                               
         LA    R1,NDDEMOS          SET DEMOS LIST TO IMPS                       
         ZIC   R2,NDNDEMOS                                                      
         MVI   1(R1),C'I'                                                       
         LA    R1,3(R1)                                                         
         BCT   R2,*-8                                                           
*                                                                               
**->     BAL   RE,SETDMPRE         SET DEMO PRECISSION                          
**->                               FOR 2 DECIMAL RTG                            
**->                               NOT NEEDED FOR IMPS?                         
**->                               (IN NENTVLDEMO)                              
         EJECT                                                                  
****************************************************                            
* REQUIRED DEMOS ARE IN NDDEMOS                                                 
* GENERAL DBLOCK FOR UNIT SET                                                   
* YRWKLST HAS YR/WK LIST OF BOOK WEEKS                                          
* NOW STEP THROUGH YRWKLST AND PASS TO DEMAND                                   
****************************************************                            
         LA    R3,YRWKLST                                                       
         LA    R4,NWEEKS                                                        
*                                                                               
REP80    CLI   SVSTA4,0              DBSELSTA+4 CHANGED?                        
         BE    REP82                 NO                                         
         MVC   DBSELSTA+4(1),SVSTA4  YES-SET IT BACK                            
         MVI   SVSTA4,0              CLEAR FLAG                                 
*                                                                               
REP82    MVC   DBSELBK,0(R3)       SET FROM YR/WK LIST                          
         CLC   DBSELBK,=X'5D25'    SWITCH TO QH DATA                            
         BL    REP85                                                            
         CLC   DBSELBK,=X'5D31'    BLACK WEEK                                   
         BE    REP85                                                            
         CLC   DBSELBK,=X'5D32'    BLACK WEEK                                   
         BE    REP85                                                            
         CLC   DBSELBK,=X'5D33'    BLACK WEEK                                   
         BE    REP85                                                            
         CLI   DBSELSTA+4,C'C'                                                  
         BNE   REP85                                                            
         MVC   SVSTA4,DBSELSTA+4   SAVE STATION TYPE                            
         MVI   DBSELSTA+4,C'Q'                                                  
REP85    DS    0H                                                               
         CLC   DBSELSTA,=C'PREVC'                                               
         BNE   *+8                                                              
         MVI   DBSELDUR,X'FF'      PICK UP ALL DUR INCL < 15MIN DURS            
         GOTO1 NBDEMAND,DMCB,DBLOCK,DEMHOOK                                     
                                                                                
*                           *STORE GIVEN HOME IMPS IN GVNHIMP4                  
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'DD'        FIND IT                                      
         USING NUOVEL,R2                                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
REP90    BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                MUST BE HERE                                 
         CLI   NUOVNUM,1           HOMES?                                       
         BNE   REP90                                                            
         ICM   R1,15,NUOVVAL                                                    
         CVD   R1,DUB                                                           
         MVC   GVNHIMP4,DUB+4      SET PACKED NUMB                              
                                                                                
*                            *STORE RETURNED HHIMP IN WGTHIMP8                  
         LA    R2,NDDEMOS          LIST OF DEMOS                                
         LA    R3,ACCUMLST         LIST OF DEMO VALUES                          
         ZIC   R1,NDNDEMOS         # OF DEMOS IN LIST                           
         CLI   2(R2),1             HOMES?                                       
         BE    REP95                                                            
         LA    R2,3(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R1,*-12                                                          
         DC    H'0'                MUST BE HERE                                 
REP95    ICM   R1,15,0(R3)                                                      
         CVD   R1,WGTHIMP8         WEIGHTED HOME IMP                            
*                                                                               
**       GO THROUGH ACCUM LIST                                                  
**       1) ACCUM DEMO IMP/WGTHIMP8 -> PAK16                                    
**       2) PAK16 * GVNHIMP4 -> DEMO IMPRESSION                                 
**       5) SET DEMO IMPRESSION TO DEMWORK                                      
**       6) AT END ADD X'DD' ELEMS TO UNIT                                      
*                                                                               
         LA    R2,NDDEMOS          LIST OF DEMOS                                
         LA    R3,ACCUMLST         LIST OF DEMO VALUES                          
         ZIC   R1,NDNDEMOS         # OF DEMOS IN LIST                           
REP100   CLI   2(R2),1             IF HOMES                                     
         BE    REP120              SKIP                                         
         CLC   0(8,R3),=PL8'0'     IF NO DEMO VALUE                             
         BE    REP120              SKIP                                         
         ZAP   PAK16,0(R3)         ACCUM DEMO VALUE -> PAK16                    
         DP    PAK16,WGTHIMP8      DIVIDE BY WEIGHTED HOMES                     
         MVC   PAK16+8(8),PAK16                                                 
         XC    PAK16(8),PAK16      CLEAR UPPER BYTES                            
         MP    PAK16,GVNHIMP4      MULTIPLY BY GIVEN HOMES                      
         DS    0H                  DECIMAL???                                   
REP120   LA    R2,3(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R1,REP100                                                        
         EJECT                                                                  
***********************************************************                     
* NO MORE UNITS                                                                 
*                                                                               
REP200   DS    0H                                                               
         B     XIT                                                              
************************************************************                    
* GET DEMOS, WEIGHT, STORE                                                      
************************************************************                    
DEMHOOK  NTR1                                                                   
         LA    R3,NDDEMOS          DEMOS LIST                                   
         ZIC   R4,NDNDEMOS         # OF DEMOS                                   
         LA    R5,ACCUMLST                                                      
         XC    DEMWORK,DEMWORK                                                  
         GOTO1 NBDEMOUT,DMCB,(C'L',(R3)),DBLOCK,DEMWORK                         
         LA    R1,DEMWORK                                                       
DHK20    ICM   RF,15,0(R1)         PICK UP VALUE JUST FOUND                     
         CVD   RF,DUB              DUB HAS DEMO VALUE                           
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,DBFACTOR                                                    
         CVD   RE,PAK8             PAK8 GETS WEIGHT                             
         MP    DUB,PAK8(2)         DUB(6) GETS WEIGHTED VALUE                   
*                                                                               
         AP    0(8,R5),DUB(6)      ADD TO ACCUMULATED VALUE                     
         LA    R1,4(R1)                                                         
         LA    R5,8(R5)                                                         
         BCT   R4,DHK20                                                         
DHKX     XIT1                                                                   
*                                                                               
         EJECT                                                                  
****************************************                                        
* INPUT:  DIVIDEND IN DUB, DIVISOR IN DUB+4                                     
*         OUTAREA IN RF                                                         
* OUTPUT: ROUNDED EDITED NUMBER IN OUTAREA, BINARY VALUE IN RF                  
*                                                                               
EDCPM    NTR1                                                                   
         LR    R2,RF                                                            
         L     RE,DUB                                                           
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         M     RE,=F'1000'                                                      
         D     RE,DUB+4                                                         
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         LTR   RF,RF                                                            
         BZ    EDCX                                                             
         EDIT  (RF),(5,0(R2)),2                                                 
         CLI   0(R2),X'40'                                                      
         BH    *+8                                                              
         MVI   0(R2),C'$'                                                       
EDCX     XIT1  REGS=(RF)                                                        
*                                                                               
****************************************                                        
* INPUT:  DIVIDEND IN DUB, DIVISOR IN DUB+4                                     
*         OUTAREA IN RF                                                         
* OUTPUT: +/- GRP INDEX IN OUTAREA, BINARY VALUE IN RF                          
*                                                                               
EDIND    NTR1                                                                   
         OC    DUB+4(4),DUB+4                                                   
         BZ    EDIX                                                             
         LR    R2,RF                                                            
         L     RE,DUB                                                           
         SRDA  RE,32(0)                                                         
         M     RE,=F'100'                                                       
         D     RE,DUB+4                                                         
         S     RF,=F'100'                                                       
         EDIT  (RF),(4,0(R2)),FLOAT=-                                           
         LTR   RF,RF                                                            
         BM    EDIX                                                             
         BNZ   EDI5                                                             
         MVC   2(2,R2),=C'+0'                                                   
         B     EDIX                                                             
EDI5     LA    R3,3(R2)                                                         
         LA    R4,4                                                             
PLUS     CLI   0(R3),C' '                                                       
         BE    EDI9                                                             
         BCTR  R3,0                                                             
         BCT   R4,PLUS                                                          
         B     *+8                                                              
EDI9     MVI   0(R3),C'+'                                                       
EDIX     XIT1  REGS=(RF)                                                        
         EJECT                                                                  
*******************************************************                         
* INITIALIZE DBLOCK                                   *                         
* THESE VALUES ARE CONSTANT FOR A UNIT                *                         
* YR/WK CHANGE FROM YRWKLST ARE SET AT DEMAND CALL    *                         
*******************************************************                         
INITDB   NTR1                                                                   
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NTI'                                                   
         MVC   DBFUNCT,DBGETNTI                                                 
         LA    R1,DEMOWRK                                                       
         ST    R1,DBAREC                                                        
         MVC   DBCOMFCS,NBACOM                                                  
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBSELMED,C'N'       INITIALIZE FOR NETWORK                       
*                                                                               
         CLI   NBPOSTYP,C'H'                                                    
         BNE   IDB10                                                            
         CLI   NBUSER1+3,C'H'      ONLY NHTI WKLY                               
         BE    *+12                                                             
         CLI   NBUSER1+3,C'B'      NHTI AND NAD WKLY                            
         BNE   *+8                                                              
         MVI   DBSELMED,C'W'                                                    
*                                                                               
IDB10    MVC   DBSELSTA(4),NBACTNET                                             
         CLC   NBAUTH,=XL2'4040'                                                
         BNH   *+10                                                             
         MVC   DBAUTH(2),NBAUTH    MOVE AUTH CODE FOR DEMO SECURITY             
         CLI   NBNTISTA,X'40'                                                   
         BNH   *+10                                                             
         MVC   DBSELSTA(4),NBNTISTA  IF SYNDICATOR USE NTI STATION              
         MVI   DBSELSTA+4,C'T'                                                  
         CLI   NBPOSTYP,X'40'                                                   
         BNH   *+18                                                             
         CLI   NBPOSTYP,C'N'                                                    
         BE    *+10                                                             
         MVC   DBSELSTA+4(1),NBPOSTYP                                           
         MVC   DBSELAGY,NBSELAGY                                                
*                                                                               
         CLI   NBPOSTDT,C'C'       TEST FOR CONFORMED                           
         BNE   *+8                                                              
         MVI   DBBTYPE,C'C'                                                     
         CLI   NBPOSTDT,C'I'       TEST FOR INTEGRATED                          
         BNE   *+8                                                              
         MVI   DBBTYPE,C'I'                                                     
         CLI   NBPOSTDT,C'A'       TEST FOR ASCRIBED                            
         BNE   *+8                                                              
         MVI   DBBTYPE,C'A'                                                     
         CLI   NBPOSTDT,C'Z'       TEST FOR X-RATED                             
         BNE   *+8                                                              
         MVI   DBBTYPE,C'Z'                                                     
         CLI   NBPOSTYP,C'H'                                                    
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
*                                  FIX AGENCY SCREW-UPS                         
         LA    RE,AGSTAEQU                                                      
IDB20    CLC   DBSELSTA(4),0(RE)                                                
         BL    IDB30           <---WON'T BE THERE                               
         BH    *+14            <---TRY NEXT                                     
         MVC   DBSELSTA(4),4(RE)   MOVE NEW STATION TO DBLOCK                   
         B     IDB30                                                            
         LA    RE,L'AGSTAEQU(RE)   BUMP TO NEXT STA EQU                         
         B     IDB20                                                            
IDB30    MVC   DBSELDAY,NBDAY      DAY                                          
         MVC   DBSELTIM,NBTIME     TIME                                         
IDBX     XIT1                                                                   
                                                                                
                                                                                
*          DATA SET NENTVLDEMO AT LEVEL 209 AS OF 05/10/00                      
*                           CALL LETTER EQUATES FOR AGENCY STATIONS             
AGSTAEQU DS    0CL8          TABLE MUST BE KEPT IN SEQUENCE                     
         DC    C'CAB ',C'CAX '                                                  
         DC    C'FBC ',C'FOX '                                                  
         DC    C'IND ',C'INX '                                                  
         DC    C'NIK ',C'NICK'                                                  
         DC    C'PAY ',C'PAX '                                                  
         DC    C'PBS ',C'PBX '                                                  
         DC    C'TBS ',C'WTBS'                                                  
         DC    C'TCF ',C'FOX '                                                  
         DC    C'SUP ',C'SUX '                                                  
         DC    X'FF'                                                            
         DS    0F                                                               
                                                                                
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
         MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+17(20),SPLCLIN                                                
         MVC   H4(8),=C'ESTIMATE'                                               
         MVC   H4+10(6),SPLEST                                                  
         MVC   H4+17(20),SPLESTN                                                
         DROP  R5                                                               
         SPACE                                                                  
*                                                                               
HDX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,50,C'DEMOS SEED REPORT'                                       
         SSPEC H2,50,C'-----------------'                                       
         SSPEC H3,50,PERIOD                                                     
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H3,98,REPORT                                                     
         SSPEC H4,98,RUN                                                        
         SSPEC H5,125,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
*                                                                               
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
         GETEL (R2),DATADISP,ELCODE                                             
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DEMOWRK  DS    CL4000                                                           
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
RELO     DS    A                                                                
ACLIST   DS    A                                                                
         DS    A                   SPARE                                        
SVDEMOS  DS    CL78                FOR 25 DEMOS + 3 EOF                         
NTISTART DS    CL6                 YYMMDD NTI REQ START                         
NTIEND   DS    CL6                 YYMMDD NTI REQ END                           
NWEEKS   EQU   13                  MAX NUMBER OF NTI WEEKS                      
****ABOVE MUST MATCH WITH NEWRI22****                                           
*                                                                               
         DS    0F                                                               
ACCUMLST DS    25PL8               WORK AREA FOR 25 DEMOS                       
         DS    0D                                                               
WGTHIMP8 DS    PL8                                                              
PAK16    DS    PL16                                                             
PAK8     DS    PL8                                                              
GVNHIMP4 DS    PL4                                                              
DEMWORK  DS    CL200               DEMO WORK AREA                               
SVSTA4   DS    CL1                                                              
*                                                                               
YRWKLST  DS    CL(2*NWEEKS)        YR/WK BOOKS FOR DEMOUT                       
*                                                                               
MYDLENE  EQU   *-MYD                                                            
*                                                                               
         EJECT                                                                  
         EJECT                                                                  
       ++INCLUDE NEGENINCLS                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE2D                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'104NEWRI23Z  08/30/00'                                      
         END                                                                    
