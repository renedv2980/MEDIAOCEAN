*          DATA SET NEWRI23    AT LEVEL 019 AS OF 03/14/18                      
*          DATA SET NEWRI23    AT LEVEL 189 AS OF 11/02/00                      
*PHASE T32023A                                                                  
*INCLUDE NETWEEK                                                                
*INCLUDE RECUP                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'T32023 - DEMOSEED REPORT '                                      
T32023   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE23**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD        * ANETWS1+2 WORK AREAS                         
         USING SPOOLD,R8                                                        
         L     R9,ASYSD          * ANETWS3 = CLIST                              
         USING NETSYSD,R9                                                       
         L     R6,ANETWS4        * ANETWS4 = NDDEMBLK                           
         USING NDDEMBLK,R6                                                      
         ST    R6,NBADEM                                                        
         L     R7,ANETWS1                                                       
         USING MYD,R7                                                           
         L     R1,=A(NTISTATS)                                                  
         ST    R1,NBCNVNTI                                                      
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXWIDTH,=F'165'                                                 
         L     R1,BOXAWIDE                                                      
         ST    R1,ADRWIDE                                                       
         DROP  R1                                                               
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         BAS   RE,REPMOD                                                        
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
         BAS   RE,CLRACCUM         CLEAR ACCUM LIST                             
         BAS   RE,CLRACCUW         CLEAR ACCUM WORK                             
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
         LA    R1,NETHOOK                                                       
         ST    R1,NBHOOK                                                        
REP20    DS    0H                                                               
         NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    REP200                                                           
         B     REP20                                                            
                                                                                
*                                                                               
NETHOOK  NTR1                                                                   
         MVI   NBUPUNIT,C'N'       DON'T WRITE UNIT BACK                        
         MVI   NBNOWRIT,C'N'                                                    
         CLI   TESTRUN,C'N'                                                     
         BNE   REP25                                                            
         MVI   NBUPUNIT,C'Y'       WRITE UNIT BACK                              
         MVI   NBNOWRIT,C'Y'                                                    
REP25    CLI   NBMODE,NBPROCUN                                                  
         BE    REP30                                                            
         CLI   NBMODE,NBPROCPK     PACKAGE?                                     
         BNE   *+8                                                              
         BAS   RE,CHKPACK                                                       
REP30    CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,NBAIO            MAKE SURE IMP BASED PACKAGE                  
         USING NUKEY,R2                                                         
         LA    R3,PAKTBL                                                        
REP32    CLC   NUPACK,0(R3)        IMP BASED PAK?                               
         BE    REP32X              YES                                          
         LA    R3,1(R3)            BUMP PAKTBL                                  
         CLI   0(R3),X'FF'         EOF?                                         
         BNE   REP32               NO                                           
         BE    XIT                 SKIP UNIT-PAK NOT IN TBL                     
REP32X   EQU   *                                                                
*                                                                               
         BAS   RE,INITDB           INITIALIZE DBLOCK                            
         B     REP35                                                            
         EJECT                                                                  
**********************************************************                      
* REMOVE X'DD' ELEMS ADDED BY PREVIOUS DEMOSEED RUNS     *                      
**********************************************************                      
REP35    DS    0H                                                               
         MVI   ELCODE,X'DD'        GET OVERRIDE CATEGORIES                      
         L     R2,NBAIO                                                         
         USING NUOVEL,R2                                                        
         BAS   RE,GETEL                                                         
         BE    REP37                                                            
         MVI   BYTE,C'*'                                                        
         B     REP97               NO OVERRIDES/SKIP                            
                                                                                
** REMOVE ELEMS ADDED BY DEMOSEED                                               
REP37    TM    NUOVFLG,NUOVDMSD    ADDED BY DEMOSEED?                           
         BNO   REP40                                                            
         L     R4,NBAIO                                                         
         GOTO1 =V(RECUP),DMCB,(C'U',0(R4)),0(R2),0                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                BAD ERROR!                                   
         L     R2,NBAIO            RESET POINTER                                
         BAS   RE,GETEL                                                         
         BE    REP37                                                            
         DC    H'0'                SHOULD NEVER GET HERE!                       
*                                                                               
REP40    BAS   RE,NEXTEL                                                        
         BE    REP37                                                            
*                                                                               
REP41    EQU   *                   END OF X'DD' ELEMS                           
         L     R2,NBAIO            RESTORE POINTER                              
         BAS   RE,GETEL                                                         
                                                                                
*** MOVE DEMOS ON UNIT TO DEMLIST                                               
         LA    R3,DEMLIST                                                       
         XC    DEMLIST,DEMLIST                                                  
         SR    R1,R1                                                            
REP52    MVC   0(3,R3),NUOVCAT     MOVE DEMO TO DEMWORK                         
         LA    R1,1(R1)                                                         
         CLI   SVMOD,0             MODIFIER ALREADY SET?                        
         BNE   REP53                                                            
         MVC   SVMOD,NUOVMOD       SAVE MODIFIER                                
         MVC   SVPREC,NUOVPRE      SAVE PRECISION                               
REP53    LA    R3,3(R3)            BUMP LIST                                    
         BAS   RE,NEXTEL           ANY MORE OVERRIDE DEMOS?                     
         BE    REP52                                                            
         DROP  R2                                                               
         CHI   R1,21               ALL DEMOS ON UNIT HAVE OVERRIDES?            
         BE    XIT                 YES/GET NEXT UNIT                            
                                                                                
* PREPARE TO REMOVE DUPLICATE DEMOS FROM NDDEMOS                                
* SET ALL DEMOS IN NDDEMOS TO IMPS                                              
         LA    R1,NDDEMOS          SET DEMOS LIST TO IMPS                       
         SR    R2,R2               CLEAR COUNTER                                
         LA    R3,NDMAXDEM         MAX NUMBER OF DEMOS IN NDDEMOS               
REP54    OC    0(3,R1),0(R1)                                                    
         BZ    REP55                                                            
         LA    R2,1(R2)                                                         
         MVI   1(R1),C'I'                                                       
         LA    R1,3(R1)                                                         
         CR    R2,R3                                                            
         BL    REP54                                                            
REP55    STC   R2,NDNDEMOS         MAX DEMOS                                    
                                                                                
* NOW REMOVE ANY DUPLICATES FROM NDDEMOS                                        
         ZIC   RF,NDNDEMOS         RF->OLD DEMO NUMBER                          
         XC    DEMWORK,DEMWORK                                                  
         LA    R1,NDDEMOS                                                       
         MVC   DEMWORK(3),0(R1)    SET FIRST DEMO                               
         BCTR  RF,0                DECREASE DEMO NUMBER                         
         LA    R2,1                R2->NON-DUP DEMO COUNT                       
         LA    R1,3(R1)            BUMP NDDEMOS                                 
         LA    RE,DEMWORK          RE-> DEMOWORK AREA                           
*                                                                               
REP60    CHI   RF,0                NO MORE DEMOS?                               
         BE    REP65                                                            
         CLC   0(3,RE),0(R1)       DO WE HAVE THIS DEMO ALREADY?                
         BNE   REP62                                                            
REP61    LA    R1,3(R1)            YES/BUMP NDDEMOS                             
         BCTR  RF,0                DECREASE DEMO NUMBER                         
         LA    RE,DEMWORK          REPOINT TO START OF DEMWORK                  
         B     REP60                                                            
REP62    LA    RE,3(RE)            BUMP DEMWORK                                 
         OC    0(3,RE),0(RE)       ANY MORE IN WORKAREA?                        
         BNZ   REP60               YES                                          
         MVC   0(3,RE),0(R1)       NO/SET DEMO TO DEMWORK                       
         LA    R2,1(R2)            BUMP NEW DEMO COUNTER                        
         B     REP61                  AND BUMP TO NEXT NDDEMOS DEMO             
                                                                                
* DEMWORK HAS NON-DUP LIST OF DEMOS FROM NDDEMOS                                
* DEMLIST HAS UNIT DEMO OVERRIDES                                               
* MOVE DEMOS FROM DEMWORK THAT ARE NOT IN DEMLIST TO NDDEMOS                    
REP65    XC    NDDEMOS,NDDEMOS                                                  
         LA    R1,DEMWORK                                                       
         LA    RF,NDDEMOS                                                       
REP67    CLI   2(R1),1             IF HOMES                                     
         BE    REP70               PASS IT                                      
         BAS   RE,CHKDUP           IS IT OVERRIDEN?                             
         BNE   REP70                                                            
         LA    R1,3(R1)            YES/BUMP DEMWORK                             
         BCTR  R2,0                 AND DECREASE NDDEMO COUNT                   
         LTR   R2,R2                                                            
         BNZ   REP72                                                            
         DC    H'0'                SOMETHING WRONG                              
         B     REP70                                                            
                                                                                
                                                                                
REP70    MVC   0(3,RF),0(R1)       SET DEMOS TO NDDEMOS                         
         LA    RF,3(RF)            BUMP NDDEMOS                                 
         LA    R1,3(R1)            BUMP DEMWORK                                 
REP72    OC    0(3,R1),0(R1)       ANY MORE DEMOS                               
         BNZ   REP67                                                            
         B     REP75               NO                                           
                                                                                
******************************************************************              
* R1-> POINT TO DEMO IN DEMWORK                                                 
* DEMLIST HAS UNIT DEMO OVERRIDES                                               
*                                                                               
CHKDUP   NTR1                                                                   
         LA    R2,DEMLIST                                                       
CHKD10   CLC   0(1,R2),0(R1)                                                    
         BNE   *+14                                                             
         CLC   2(1,R2),2(R1)                                                    
         BE    CHKDUPX                                                          
         LA    R2,3(R2)            BUMP UNIT DEMOOVERRIDE LIST                  
         OC    0(3,R2),0(R2)       NO MORE OVERRIDES?                           
         BNZ   CHKD10                                                           
         LTR   RE,RE               NO SET CC                                    
CHKDUPX  XIT1                                                                   
*******************************************************************             
REP75    STC   R2,NDNDEMOS         AND NEW DEMOS NUMBER                         
**->     BAS   RE,SETDMPRE         SET DEMO PRECISSION                          
**->                               FOR 2 DECIMAL RTG                            
**->                               NOT NEEDED FOR IMPS?                         
**->                               (IN NENTVLDEMO)                              
         EJECT                                                                  
****************************************************                            
* DEMOS IN NDDEMOS ARE ALL IMPS AND NO DUPLICATES                               
* GENERAL DBLOCK FOR UNIT SET                                                   
* YRWKLST HAS YR/WK LIST OF BOOK WEEKS                                          
* NOW STEP THROUGH YRWKLST AND PASS TO DEMAND                                   
****************************************************                            
         LA    R1,NDDEMOS          DEMOS LIST                                   
         LA    R3,DEMLIST                                                       
         XC    DEMLIST,DEMLIST                                                  
         MVC   DEMLIST,NDDEMOS                                                  
* NOW SET X'FF' AT END OF LIST                                                  
         ZIC   RE,NDNDEMOS                                                      
         MHI   RE,3                                                             
         AR    R3,RE                                                            
         MVI   0(R3),X'FF'         SET EOF                                      
*                                                                               
         LA    R3,YRWKLST                                                       
         LA    R4,NWEEKS                                                        
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
*                                                                               
**************                                                                  
         OC    DBDIVSOR,DBDIVSOR   DIVIDE DEMO VALUES BY DIVSOR                 
         BZ    REP86D                (AND ROUND UP)                             
         SR    RE,RE                                                            
         ICM   RE,3,DBDIVSOR                                                    
         CVD   RE,PAK8             PAK8 GETS DIVSOR                             
         ZIC   R1,NDNDEMOS         R1->NUMBER OF DEMOS                          
         LA    RF,ACCUMWRK         RF->ACCUMWRK DEMO VALUES                     
REP86C   MVC   DUB,0(RF)           MOVE DEMO VALUE TO DUB                       
         MP    DUB,=P'100'         SET TP ROUND UP                              
         DP    DUB,PAK8+6(2)       DIVIDE BY DBDIVSOR                           
         ZAP   WORK(8),DUB(6)      SET TO WORK(8)                               
         AP    WORK(8),=P'50'                                                   
         DP    WORK(8),=P'100'                                                  
         ZAP   0(8,RF),WORK(6)     PUT QUOTIENT BACK TO ACCUMWRK                
         LA    RF,8(RF)            BUMP TO NEXT DEMO VALUE                      
         BCT   R1,REP86C                                                        
                                                                                
REP86D   DS    0H                                                               
         ZIC   R1,NDNDEMOS         ADD ACCUMWRK TO ACCUMLST                     
         LA    RE,ACCUMWRK                                                      
         LA    RF,ACCUMLST                                                      
REP86E   AP    0(8,RF),0(8,RE)                                                  
         LA    RF,8(RF)                                                         
         LA    RE,8(RE)                                                         
         BCT   R1,REP86E                                                        
         BAS   RE,CLRACCUW         CLEAR ACCUMWRK                               
*                                                                               
SKIPALL  EQU   *                                                                
**************************************************                              
                                                                                
         LA    R3,2(R3)            BUMP YR/WEEK LIST                            
         OC    0(2,R3),0(R3)       ANY MORE DATES?                              
         BZ    REP87               NO                                           
         BCT   R4,REP80                                                         
*                           *STORE GIVEN HOME IMPS IN GVNHIMP4                  
REP87    L     R2,NBAIO                                                         
         MVI   ELCODE,X'DD'        FIND IT                                      
         USING NUOVEL,R2                                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
REP90    BAS   RE,NEXTEL                                                        
         BE    REP91                                                            
         MVI   BYTE,C'*'           IF NO HOMES/SKIP UNIT                        
         B     REP97                                                            
REP91    CLI   NUOVNUM,1           HOMES?                                       
         BNE   REP90                                                            
         ICM   R1,15,NUOVVAL                                                    
         LTR   R1,R1                                                            
         BNZ   REP92                                                            
         MVI   BYTE,C'*'           NO GIVEN HOMES IMP                           
         B     REP97                                                            
REP92    CVD   R1,DUB                                                           
         MVC   GVNHIMP4,DUB+4      SET PACKED NUMB                              
         DROP  R2                                                               
                                                                                
*                            *STORE RETURNED HHIMP IN WGTHIMP8                  
         LA    R2,NDDEMOS          LIST OF DEMOS                                
         LA    R3,ACCUMLST         LIST OF DEMO VALUES                          
         ZIC   R1,NDNDEMOS         # OF DEMOS IN LIST                           
REP93    CLI   2(R2),1             HOMES?                                       
         BE    REP95                                                            
         LA    R2,3(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R1,REP93                                                         
         DC    H'0'                MUST BE HERE                                 
REP95    DS    0H                                                               
         MVC   WGTHIMP8,0(R3)      WEIGHTED HOME IMP                            
*                                                                               
**       GO THROUGH ACCUM LIST                                                  
**       1) ACCUM DEMO IMP/WGTHIMP8 -> PAK16                                    
**       2) PAK16 * GVNHIMP4 -> DEMO IMPRESSION                                 
**       5) SET DEMO IMPRESSION TO DEMWORK                                      
**       6) AT END ADD X'DD' ELEMS TO UNIT                                      
*                                                                               
REP97    L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
         MVC   XP(6),NBACTPRG                                                   
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(4,XP+7)                                
         MVI   XP+12,C'-'                                                       
         EDIT  (B1,NBACTSUB),(3,XP+13),ALIGN=LEFT                               
         CLI   BYTE,C'*'           NO IMP?                                      
         BNE   REP98                                                            
         MVC   XP+17(40),=C'*** NO HOMES IMPS / UNIT NOT UPDATED ***'           
         BAS   RE,PRINTIT                                                       
         MVI   BYTE,0                                                           
         BAS   RE,CLRACCUM         CLEAR ANY ACCUM                              
*                                                                               
REP97C   B     XIT                 GET NEXT UNIT                                
                                                                                
REP98    BAS   RE,PRINTIT                                                       
REP98X   EQU   *                                                                
*                                                                               
         LA    R5,XP                                                            
         LR    RE,R5                                                            
         LA    RE,140(RE)          BUMP P LINE                                  
         ST    RE,AENDPLIN         P-LINE LIMI                                  
         DROP  R5                                                               
*                                                                               
         LA    R2,NDDEMOS          LIST OF DEMOS                                
         LA    R3,ACCUMLST         LIST OF DEMO VALUES                          
         ZIC   R1,NDNDEMOS         # OF DEMOS IN LIST                           
REP100   CLI   2(R2),1             IF HOMES                                     
         BE    REP120              SKIP                                         
         CLC   0(8,R3),=PL8'0'     IF NO DEMO VALUE                             
         BE    REP115              SKIP                                         
         ZAP   PAK16,0(8,R3)       ACCUM DEMO VALUE -> PAK16                    
         MP    PAK16,=P'100000'                                                 
         DP    PAK16,WGTHIMP8      DIVIDE BY WEIGHTED HOMES                     
***      GOTO1 =V(PRNTBL),DMCB,=C'WGTHI',WGTHIMP8,C'DUMP',10,=C'1D'             
         MVC   PAK16+8(8),PAK16                                                 
         XC    PAK16(8),PAK16      CLEAR UPPER BYTES                            
         MP    PAK16,GVNHIMP4      MULTIPLY BY GIVEN HOMES                      
***      GOTO1 =V(PRNTBL),DMCB,=C'GVNUM',GVNHIMP4,C'DUMP',10,=C'1D'             
***      DC    H'0'                                                             
         AP    PAK16,=P'50000'     ROUND UP                                     
         DP    PAK16,=P'100000'                                                 
*                                                                               
         CLI   TESTRUN,C'Y'        TESTRUN?                                     
         BE    *+8                 YES                                          
         BAS   RE,ADDOVELM         NO-ADD X'DD' OVERRIDE ELEM                   
         BAS   RE,PUTOLINE                                                      
*                                                                               
REP115   L     RE,AENDPLIN         P LINE LIMIT                                 
         CR    R5,RE               ENOUGH SPACE LEFT?                           
         BNH   REP120              YES                                          
         BAS   RE,PRINTIT                                                       
         L     R5,ADRWIDE          RESET R5 TO START                            
         USING WIDED,R5                                                         
         LA    R5,XP                                                            
         DROP  R5                                                               
*                                                                               
REP120   LA    R2,3(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R1,REP100                                                        
         BAS   RE,PRINTIT                                                       
         BAS   RE,CLRACCUM                                                      
*                                                                               
REP125   B     XIT                 GET NEXT UNIT                                
*                                                                               
PUTOLINE NTR1                                                                   
         ZIC   R3,NDNDEMOS         TOTAL NUMBER OF DEMOS                        
         SR    R3,R1               R1=CURRENT DEMO (STARTING FROM 20)           
         NETGO NVDEMCON,DMCB,((R3),NDDEMOS),(C'C',DBLOCK),(13,WORK)             
         MVC   0(6,R5),WORK                                                     
         LA    R4,6                                                             
         CLI   WORK+7,X'40'        NAME GREATER THAN 6?                         
         BNH   PUT10                                                            
         MVC   0(10,R5),WORK                                                    
         LA    R4,4(R4)                                                         
PUT10    AR    R5,R4                                                            
         MVI   0(R5),C'='                                                       
         LA    R5,1(R5)                                                         
         EDIT  (P4,PAK16+8),(7,0(R5)),1,ALIGN=LEFT   PUT TO PLINE               
         LA    R5,8(R5)                                                         
         XIT1  REGS=(R5)                                                        
*                                                                               
         EJECT                                                                  
***********************************************************                     
* NO MORE UNITS                                                                 
*                                                                               
REP200   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
************************************************************                    
* GET DEMOS, WEIGHT, STORE                                                      
************************************************************                    
DEMHOOK  NTR1                                                                   
         LA    R3,DEMLIST                                                       
         ZIC   R4,NDNDEMOS         # OF DEMOS                                   
***      LA    R5,ACCUMLST                                                      
         LA    R5,ACCUMWRK                                                      
         XC    DEMWORK,DEMWORK                                                  
***      GOTO1 =V(PRNTBL),DMCB,=C'DEMLST',DEMLIST,C'DUMP',60,=C'1D'             
         GOTO1 NBDEMOUT,DMCB,(C'L',(R3)),DBLOCK,DEMWORK                         
         LA    R1,DEMWORK                                                       
DHK20    ICM   RF,15,0(R1)         PICK UP VALUE JUST FOUND                     
         CVD   RF,DUB              DUB HAS DEMO VALUE                           
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,DBFACTOR                                                    
         CVD   RE,PAK8             PAK8 GETS WEIGHT                             
         MP    DUB,PAK8+6(2)       DUB(6) GETS WEIGHTED VALUE                   
*                                                                               
         AP    0(8,R5),DUB      ADD TO ACCUMULATED VALUE                        
         LA    R1,4(R1)                                                         
         LA    R5,8(R5)                                                         
         BCT   R4,DHK20                                                         
***      GOTO1 =V(PRNTBL),DMCB,=C'DEMWRK',DEMWORK,C'DUMP',60,=C'1D'             
***      GOTO1 =V(PRNTBL),DMCB,=C'ACCUM',ACCUMLST,C'DUMP',80,=C'1D'             
DHKX     XIT1                                                                   
*                                                                               
*********************************************************                       
* ADD DEMO OVERRIDE ELEM X'DD'                                                  
*********************************************************                       
ADDOVELM NTR1                                                                   
         XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
         USING NUOVEL,RE                                                        
         MVC   NUOVEL(2),=X'DD0C'         ID/LENGTH                             
         MVC   NUOVCAT(3),0(R2)                                                 
         MVC   NUOVPRE,SVPREC      SET STORED PRECISION                         
         MVC   NUOVMOD,SVMOD       SET SAVED MODIFIER                           
         OI    NUOVFLG,NUOVDMSD       SET BY DEMOSEED                           
         MVC   DUB,PAK16+4                                                      
         CVB   R1,DUB                                                           
         STCM  R1,15,NUOVVAL                                                    
         DROP  RE                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFILE '),NBAIO,ELEM,0                      
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
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
         MVI   DBFUNCT,DBGETDEM                                                 
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
IDB30    DS    0H                                                               
******   MVC   DBSELDAY,NBDAY      DAY                                          
         MVC   DBSELDAY,NBSDROT    USE ROTATION                                 
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
         L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
         L     R4,ATWA                                                          
         USING T320FFD,R4                                                       
         MVC   XHEAD1(12),=C'NETWORK T.V.'                                      
         MVC   XHEAD3(6),=C'CLIENT'                                             
         MVC   XHEAD3+10(3),SPLCLI                                              
         MVC   XHEAD3+17(20),SPLCLIN                                            
         MVC   XHEAD4(8),=C'ESTIMATE'                                           
         MVC   XHEAD4+10(6),SPLEST                                              
         MVC   XHEAD4+17(20),SPLESTN                                            
         B     HDX                                                              
****************************************************************                
         LA    R1,XHEAD6          R5->H6                                        
         DROP  R4,R5                                                            
         LR    R5,R1                                                            
         SR    R2,R2               R2->DEMO POSITION IN NDDEMOS                 
         LA    R4,NDDEMOS          R4->NDDEMOS                                  
         LA    R3,20               MAX 20 DEMOS TO PRINT                        
HDRTN10  CLI   2(R4),1             HOMES ?                                      
         BE    HDRTN20             YES-SKIP                                     
         XC    WORK(20),WORK                                                    
         NETGO NVDEMCON,DMCB,((R2),NDDEMOS),(C'C',DBLOCK),(13,WORK)             
         CLI   WORK+7,X'40'        NAME GREATER THAN 7?                         
         BNH   HDRTN18                                                          
         LR    RE,R5               YES- PUT ON TWO LINES                        
         LA    RE,2(RE)            INDENT OUTPUT                                
         LA    RF,WORK                                                          
         LA    R1,7                A SAFETY NET                                 
HDRTN17  MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         CLI   0(RF),C'.'                                                       
         BE    HDRTN17B                                                         
         BCT   R1,HDRTN17                                                       
         DC    H'0'                 WHAT ARE WE DEALING WITH?                   
HDRTN17B MVC   198(7,R5),1(RF)                                                  
         B     *+10                                                             
HDRTN18  MVC   0(7,R5),WORK                                                     
         LA    R5,8(R5)            BUMP P LINE                                  
HDRTN20  LA    R2,1(R2)            BUMP DEMO #                                  
         LA    R4,3(R4)            BUMP DEMO LIST                               
         OC    0(3,R4),0(R4)       EOF ?                                        
         BZ    HDX                                                              
         BCT   R3,HDRTN10                                                       
****************************************************************                
*                                                                               
HDX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  WSPEC H2,1,REQUESTOR                                                   
         WSPEC H1,50,C'DEMOS SEED REPORT'                                       
         WSPEC H2,50,C'-----------------'                                       
         WSPEC H3,50,PERIOD                                                     
         WSPEC H1,98,AGYNAME                                                    
         WSPEC H2,98,AGYADD                                                     
         WSPEC H3,98,REPORT                                                     
         WSPEC H4,98,RUN                                                        
         WSPEC H5,125,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
*                                                                               
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
         GETEL (R2),DATADISP,ELCODE                                             
         EJECT                                                                  
******************************************************                          
* GET LIST OF IMPRESSION BASED PKGS                                             
******************************************************                          
CHKPACK  NTR1                                                                   
         L     R2,NBAIO                                                         
         USING NPKEY,R2                                                         
         TM    NPAKCNTL,X'40'      IMP BASED?                                   
         BNO   CHKPAKX                                                          
         LA    R3,PAKTBL                                                        
CKP5     CLI   0(R3),0                                                          
         BE    CKP20                                                            
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'         EOF?                                         
         BNE   CKP5                                                             
         DC    H'0'                INCREASE TABLE                               
CKP20    MVC   0(1,R3),NPKPACK     SET PAK NUMBER                               
*                                                                               
CHKPAKX  B     XIT                                                              
         DROP  R2                                                               
*                                                                               
CLRACCUM NTR1                                                                   
         LA    R1,ACCUMLST         PREPARE PACKED ACCUMLST                      
         LA    R2,25               MAX # OF DEMOS IN LIST                       
CLR10    ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R2,CLR10                                                         
         B     XIT                                                              
CLRACCUW NTR1                                                                   
         LA    R1,ACCUMWRK         PREPARE PACKED ACCUMWRK                      
         LA    R2,25               MAX # OF DEMOS IN LIST                       
CLR20    ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R2,CLR20                                                         
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DEMOWRK  DS    CL4000                                                           
NTISTATS DS    CL4000                                                           
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
TESTRUN  DS    CL1                                                              
PAKTBL   DS    CL100               100 PACKAGE NUMBERS                          
         DC    X'FF'                                                            
****ABOVE MUST MATCH WITH NEWRI22****                                           
*                                                                               
         DS    0F                                                               
ADRWIDE  DS    A                                                                
AENDPLIN DS    A                                                                
ACCUMLST DS    25PL8               ACCUM AREA FOR 25 DEMOS                      
ACCUMWRK DS    25PL8               WORK AREA FOR 25 DEMOS                       
         DS    0D                                                               
WGTHIMP8 DS    PL8                                                              
PAK16    DS    PL16                                                             
PAK8     DS    PL8                                                              
GVNHIMP4 DS    PL4                                                              
DEMWORK  DS    CL200               DEMO WORK AREA                               
DEMLIST  DS    CL76                25X3+X'FF'                                   
SVSTA4   DS    CL1                                                              
SVMOD    DS    CL1                                                              
SVPREC   DS    CL1                                                              
*                                                                               
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
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019NEWRI23   03/14/18'                                      
         END                                                                    
