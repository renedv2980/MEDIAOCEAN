*          DATA SET DEPN0110   AT LEVEL 097 AS OF 03/21/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEPN09IA                                                                 
*INCLUDE DEMTIME                                                                
*INCLUDE UNTIME                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE LOADER                                                                 
         TITLE 'DEMO CONVERSION'                                                
DEPN09I  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DEPN09I                                                        
         LR    R3,RB                                                            
         LA    R3,2048(RB)                                                      
         LA    R3,2048(R3)                                                      
         USING DEPN09I+4096,R3                                                  
         USING DEMCOND,R8                                                       
         L     RC,ARREC                                                         
         LA    RC,4(RC)                                                         
         USING QHDSECT,RC                                                       
         L     R2,AIREC                                                         
         USING INTERD,R2                                                        
         B     *+4(R1)                                                          
         SPACE 2                                                                
         B     READ                GET INPUT (ARREC - INT)                      
         B     CNVWR               CONVERT TO OUTPUT (INT - WORK)               
         B     MORET               CLOSE INPUT                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*===================== GET INPUT (RREC --> IREC) =====================*         
*                                                                               
READ     CLI   INTAPESW,1                                                       
         BE    OPENOK                                                           
         OPEN  (IN1,(INPUT))                                                    
*                                                                               
*                                                                               
LOADTAB  MVC   DUB,DEMTABS                                                      
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   CDEMTABS,4(R1)                                                   
*                                                                               
OPENOK   L     R4,ARREC                                                         
         XC    0(4,R4),0(R4)                                                    
         MVC   0(2,R4),=Y(RRECL+4)                                              
         LA    R4,4(R4)                                                         
         GET   IN1,(R4)                                                         
*                                                                               
         L     RE,ANIINPUT         SET FOR RECORD COUNTS                        
         L     R0,0(RE)                                                         
         AH    R0,=H'1'                                                         
         ST    R0,0(RE)                                                         
         CLC   QHMNO,=C'000124'                                                 
         BNE   *+10                                                             
         MVC   QHMNO,=C'000168'                                                 
*                                                                               
         CLC   QHMNO,=C'000295'                                                 
         BNE   *+10                                                             
         MVC   QHMNO,=C'000480'                                                 
*                                                                               
         SPACE 1                                                                
*                                                                               
         XC    PREVKEY,PREVKEY     CLEAR LAST KEY FOR MKT CHANGE                
         CLC   QHRCDE,=C'002'                                                   
         BE    M2REC                                                            
         CLC   QHRCDE,=C'001'                                                   
         BE    M1REC                                                            
         CLC   QHRCDE,=C'400'                                                   
         BNE   OPENOK                                                           
*                                                                               
*-------------------------- MARKET NUMBER ----------------------------*         
*                                                                               
         XC    DUB,DUB   CONVER MARKET NUMBER                                   
         PACK  DUB,QHMNO                                                        
         CVB   RE,DUB                                                           
         STCM  RE,3,INTMRKT                                                     
*                                                                               
*---------------------------- BOOK TYPE ------------------------------*         
*                                                                               
* SINCE HART/NEW HAVEN METROS HAVE SAME STATIONS WE MUST LOAD ONE               
* AS A 'T' AND ONE AS AN 'M'                                                    
*                                                                               
GETBTYP  DS    0H                  GET SPECIAL BOOK TYPES                       
         GOTO1 VLDCREC,DMCB,0,0    FIND MARKET TABLES                           
         L     RE,DMCB                                                          
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                DIE IF NONE THERE                            
GETBTYP2 CLC   0(2,RE),=C'NT'      FIND THE NSI TV ONE                          
         BE    GETBTYP4                                                         
         ICM   RE,7,2(RE)          NF - TRY NEXT ONE                            
         OC    0(2,RE),0(RE)       EOT - DIE SOMETHING WRONG                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     GETBTYP2                                                         
*                                                                               
GETBTYP4 LA    RE,5(RE)            BYPASS HEADER                                
GETBTYP6 OC    0(2,RE),0(RE)       EOT                                          
         BNZ   *+6                 DIE -- PREVENT OVERWRITING                   
         DC    H'0'                 OF DATA ALREADY LOADED                      
*                                                                               
         CLC   0(2,RE),INTMRKT     HAVE IT                                      
         BE    *+12                                                             
         A     RE,DMCB+4           L'ENTRY                                      
         B     GETBTYP6            NEXT ONE                                     
*                                                                               
         CLI   2(RE),C' '          DON'T CARE                                   
         BE    *+14                                                             
         MVC   INTBTYP,2(RE)       SET BOOK TYPE                                
         B     GETBTYP7                                                         
         SPACE 1                                                                
         CLI   BOOKTYPE,0          IF BTYPE IN TABLE IS SPACE,                  
         BE    GETBTYP7                                                         
         CLI   BOOKTYPE,C'P'       LPM DATA?                                    
         BNE   *+8                                                              
         CLI   SAMPLEBT,C'H'       HISPANIC LPM?                                
         BNE   *+12                                                             
         MVI   INTBTYP,C'I'        HIPANIC LPM = I                              
         B     GETBTYPX                                                         
         MVC   INTBTYP,BOOKTYPE     THEN USE OVERRIDE IF GIVEN                  
GETBTYP7 DS    0H                                                               
*                                                                               
         CLI   LIVEPLSD,1                                                       
         BNE   *+20                                                             
         MVI   INTBTYP,BOOKTYPE_LS X'36' FOR LIVE+SD                            
         CLI   SAMPLEBT,C'H'                                                    
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_HS FOR HISPANIC LIVE+SD                         
*                                                                               
         CLI   LIVEPLU3,1                                                       
         BNE   *+8                                                              
         MVI   INTBTYP,BOOKTYPE_L3 FOR LIVE+3                                   
*                                                                               
         CLI   LIVEONLY,1                                                       
         BNE   *+20                                                             
         MVI   INTBTYP,C'L'                                                     
         CLI   SAMPLEBT,C'H'                                                    
         BNE   *+8                                                              
         MVI   INTBTYP,C'J'                                                     
*                                                                               
         CLI   ZEROCELL,1                                                       
         BNE   GETBTYP8                                                         
         MVI   INTBTYP,C'1'                                                     
         CLI   SAMPLEBT,C'H'                                                    
         BNE   *+8                                                              
         MVI   INTBTYP,C'2'                                                     
*                                                                               
GETBTYP8 CLI   INTBTYP,0                                                        
         BNE   GETBTYPX                                                         
         MVC   INTBTYP,BOOKTYPE                                                 
         CLI   SAMPLEBT,0          BOOK TYPE ON TAPE                            
         BE    *+10                                                             
         MVC   INTBTYP,SAMPLEBT    YES - USE THAT ONE                           
*                                                                               
GETBTYPX DS    0H                                                               
         BAS   RE,MSGREC                                                        
         BAS   RE,FLTMKT                                                        
         CLI   PASSFLT,C'Y'                                                     
         BE    *+12                                                             
         MVI   MSGFLAG,0                                                        
         B     OPENOK                                                           
*                                                                               
*---------------------- STATION CALL LETTERS ------------------------*          
*                                                                               
BTOK1    MVC   INTSTA(4),QHSCALL                                                
         MVI   INTSTA+4,C'T'                                                    
         MVC   INTBOOK,NSIBKYM     BOOK (YEAR & MONTH)                          
*                                                                               
         CLC   INTSTA,SVSTA        CHANGE OF STATION                            
         BNE   PCK1                                                             
         CLC   SVQHCDE,QHQHCDE     CHANGE OF QH                                 
         BNE   PCK1                                                             
         CLC   SVSTYP,QHSTYP       CHANGE OF STATION TYPE                       
         BE    PCK2                                                             
PCK1     XC    SVPNAMD,SVPNAMD     CLEAR PROGRAM BUFFER                         
         MVC   SVSTA,INTSTA                                                     
         MVC   SVQHCDE,QHQHCDE                                                  
         MVC   SVSTYP,QHSTYP                                                    
*                                                                               
* CHECK FOR RECODING OF DAYS NEEDED                                             
PCK2     CLC   QHDALF(2),=C'AV'                                                 
         BNE   NORECODE                                                         
         XC    APNAMD,APNAMD       CLEAR OUT POINTER                            
         LA    RF,SVPNAMD                                                       
RECODE1  CLI   0(RF),0                                                          
         BE    GETDAY0                                                          
         CLC   QHPN14,0(RF)                                                     
         BE    *+12                                                             
         LA    RF,15(RF)                                                        
         B     RECODE1                                                          
         CLC   QHDALF,=C'AV5'      AV5 MAY BE M-F                               
         BNE   RECODE2                                                          
         CLI   14(RF),X'7C'                                                     
         BNE   *+16                                                             
         MVC   QHDALF,=C'M-F'                                                   
         MVC   QHDCDE,=C'00'                                                    
RECODE2  DS    0C                                                               
         CLC   QHDCDE,=C'09'                                                    
         BNE   GETDAY0                                                          
         CLC   QHDALF,=C'AV7'                                                   
         BE    GETDAY0                                                          
         MVC   QHDCDE,=C'06'                                                    
         B     GETDAY0                                                          
NORECODE DS    0H                                                               
* SAVE PROGRAM NAME AND DAYS FOR RECODING                                       
         LA    RF,SVPNAMD                                                       
CHKPND   CLI   0(RF),0             END - JUST INSERT IT                         
         BNE   *+14                                                             
         MVC   0(14,RF),QHPN14                                                  
         B     CHKPNDX                                                          
         CLC   0(14,RF),QHPN14     EQUAL - SET FOR DAY CODE MERGE               
         BE    CHKPNDX                                                          
         LA    RF,15(RF)                                                        
         B     CHKPND                                                           
CHKPNDX  ST    RF,APNAMD                                                        
*                                                                               
*---------------------------- GET DAYS ------------------------------*          
*                                                                               
                                                                                
         DS    0H                                                               
GETDAY0  LA    RF,DAYTABL                                                       
*                                                                               
GETDAY   CLI   0(RF),0                                                          
         BNE   GETDAY2                                                          
         DC    H'0'                                                             
GETDAY2  CLC   QHDCDE,0(RF)                                                     
         BE    *+12                                                             
         AHI   RF,L'DAYTABL                                                     
         B     GETDAY                                                           
         MVC   INTDAYWK,2(RF)                                                   
         MVC   INTNDAYS,3(RF)                                                   
                                                                                
         NI    INTNDAYS,X'0F'                                                   
                                                                                
         ICM   RE,15,APNAMD        ANY PNAMD TABLE ENTRY                        
         BZ    *+10                                                             
         OC    14(1,RE),4(RF)      YES - KEEP TRACK OF DAYS                     
                                                                                
         CLI   INTDAYWK,9                                                       
         BNE   GETDAYX                                                          
         MVC   INTNDAYS,PRDYINAV                                                
         NI    INTNDAYS,X'0F'                                                   
         CLC   QHDALF(2),=C'AV'                                                 
         BE    *+8                                                              
         MVI   INTDAYWK,0                                                       
GETDAYX  DS    0H                                                               
*                                                                               
*--------------------------- ACTIVE WEEKS ---------------------------*          
*                                                                               
         MVI   INTWEEKS,0         SET UP ACTIVE WEEKS                           
         SR    R9,R9              MOVED THIS PART OF THE CODES UP               
         CLC   QHDAYT(2),=C'00'       BECAUSE QTR HOUR ROUTINE REQUIRED         
         BNH   *+12               # OF WEEKS.                                   
         OI    INTWEEKS,X'08'                                                   
         LA    R9,1(R9)                                                         
         CLC   QHDAYT+2(2),=C'00'                                               
         BNH   *+12                                                             
         OI    INTWEEKS,X'04'                                                   
         LA    R9,1(R9)                                                         
         CLC   QHDAYT+4(2),=C'00'                                               
         BNH   *+12                                                             
         OI    INTWEEKS,X'02'                                                   
         LA    R9,1(R9)                                                         
         CLC   QHDAYT+6(2),=C'00'                                               
         BNH   *+12                                                             
         OI    INTWEEKS,X'01'                                                   
         LA    R9,1(R9)            R9 = WEEKS                                   
*                                                                               
*-------------------------- QUARTER HOURS ---------------------------*          
*                                                                               
**********************************************************************          
* THE FOLLOWING CODE WILL TRY TO DECIDE WHICH DURATION IS THE MAJORITY          
* OF THE 4 WEEKS, IN A CASE OF A TIE, THE LONGEST ONE WILL BE USED.             
**********************************************************************          
*                                                                               
         CLC   =C'KCAL',QHSCALL                                                 
         BNE   CN10                                                             
         CLC   =C'0800P',QHQHCDE                                                
         BNE   CN10                                                             
         LA    R1,0(R1)                                                         
*                                                                               
CN10     DS    0H                                                               
         XC    DURTAB,DURTAB                                                    
*        MVI   DURTABX,X'FF'                                                    
         LA    R7,QHDAYT                                                        
         LA    R6,PRQHSN           CONVERT THE FIRST 4 DURATIONS                
         USING PRQHSND,R6                                                       
         LHI   R5,4                                                             
         LA    RE,DURTAB           AND FILL DURATION TABLE                      
         MVI   DAYS,1              SET THE NUMBER OF DAYS TCAST                 
         CLI   PRDYINAV,C'0'                                                    
         BE    *+14                                                             
         MVC   DAYS,PRDYINAV                                                    
         NI    DAYS,X'0F'                                                       
         CLC   QHDALF,=C'M-F'                                                   
         BNE   *+8                                                              
         MVI   DAYS,5                                                           
                                                                                
CNVDURP  DS    0C                                                               
         OC    0(2,R7),=C'00'      ELIMINATE SPACES                             
         CLC   0(2,R7),=C'00'      ONLY SAVE ACTIVES                            
         BE    CNVDURP5                                                         
         OC    0(3,RE),0(RE)                                                    
         BZ    CNVDURP2                                                         
         CLC   PRQHNUM,0(RE)       NUMBER OF QH                                 
         BE    CNVDURP4                                                         
*        BNE   *+14                                                             
*        CLC   3(2,RE),0(R7)       NUMBER OF DAYS                               
*        BE    CNVDURP4                                                         
         LA    RE,6(RE)            CHECK FO RNEXT                               
         B     CNVDURP                                                          
CNVDURP2 MVC   0(3,RE),PRQHNUM                                                  
         MVC   3(2,RE),0(R7)                                                    
CNVDURP4 ZIC   RF,5(RE)            BUMP THE COUNT                               
         AHI   RF,1                                                             
         STC   RF,5(RE)                                                         
         LA    RE,DURTAB           BUMP TABLE POINTERS                          
CNVDURP5 LA    R7,2(R7)                                                         
         LA    R6,8(R6)                                                         
         BCT   R5,CNVDURP                                                       
         SPACE 2                                                                
* NOW FIND THE ONE WHICH OCCURS THE MOST                                        
         LA    RE,DURTAB+6                                                      
         LA    R5,3                                                             
CNVDURP8 CLC   DURTAB+5(1),5(RE)   CURRENT HIGHEST GTE NEXT ITEM                
         BNL   *+10                                                             
         MVC   DURTAB(6),0(RE)      NO - USE NEXT ITEM                          
         LA    RE,6(RE)                                                         
         BCT   R5,CNVDURP8                                                      
*                                                                               
         PACK  DUB,DURTAB(3)       WEEKLY QH                                    
         CVB   R1,DUB                                                           
         ZIC   RF,DAYS                                                          
         SLL   R1,1                                                             
         SR    R0,R0                                                            
         DR    R0,RF               GET WEEKLY AVG QH                            
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LR    R7,R1                                                            
*                                                                               
         CH    R7,=H'2'           NO DURATION SHOULD BE LESS                    
         BH    *+8                THAN TWO                                      
         LA    R7,2               R7 = AVERAGE DURATION                         
*                                                                               
         SR    R6,R6                                                            
         PACK  DUB,PRQHTOT        SET ACTUAL NUMBER OF QH                       
         CVB   RF,DUB                                                           
         STC   RF,INTADUR         INTADUR GETS TOTAL MONTHLY DURATION           
*                                                                               
*        GOTO1 =V(HRTOQH),DMCB,(1,PRQHSN),INTSQH                                
         GOTO1 =V(HRTOQH),DMCB,(1,QHQHCDE),INTSQH                               
         ZIC   RF,INTSQH                                                        
*                                                                               
         AR    R7,RF                                                            
         STC   R7,INTEQH          END HOUR                                      
*                                                                               
*-------------------------- STATION TYPE ----------------------------*          
*                                                                               
*                                                                               
         CLC   QHSTYP,=C'01'                                                    
         BNE   *+8                                                              
         MVI   INTSTYP,PARENTE                                                  
         CLC   QHSTYP,=C'02'                                                    
         BNE   *+8                                                              
         MVI   INTSTYP,PARENTE                                                  
         CLC   QHSTYP,=C'00'                                                    
         BNE   *+8                                                              
         MVI   INTSTYP,PARENTE                                                  
         CLC   QHSTYP,=C'05'                                                    
         BNE   *+8                                                              
         MVI   INTSTYP,PS1E                                                     
         CLC   QHSTYP,=C'07'                                                    
         BNE   *+8                                                              
         MVI   INTSTYP,S1EQU                                                    
         CLC   QHSTYP,=C'09'                                                    
         BNE   *+8                                                              
         MVI   INTSTYP,OUTMARE                                                  
         SPACE 1                                                                
* FORCE KTNC P+S1 TO SPILL FOR SACRAMENTO HISPANIC                              
         CLC   INTMRKT,=H'462'     SACRAMENTO                                   
         BNE   STYPOK                                                           
         CLC   INTSTA(4),=C'KTNC'  NHSI MISCODING - HOME IS SANFRAN             
         BNE   STYPOK                                                           
         MVI   INTSTYP,OUTMARE     FORCE IT TO OUTSIDE                          
STYPOK   CLC   INTMRKT,=H'481'   CANADIAN VIEWERS                               
         BE    CHKCAN                                                           
         CLC   INTMRKT,=H'114'   CANADIAN VIEWERS                               
         BE    CHKCAN                                                           
         B     CHKMTROB                                                         
*                                                                               
CHKCAN   CLI   QHSGI,C'+'                                                       
         BNE   *+8                                                              
         MVI   INTSTYP,PS1E                                                     
*                                                                               
*--------------------------- MARKET TYPE ----------------------------*          
*                                                                               
CHKMTROB CLC   INTMRKT,=H'178'   METRO B MARKETS                                
         BNE   *+8                                                              
         MVI   INTMTYP,METROBE                                                  
         CLC   INTMRKT,=H'253'                                                  
         BNE   *+8                                                              
         MVI   INTMTYP,METROBE                                                  
         CLC   INTMRKT,=H'254'                                                  
         BNE   *+8                                                              
         MVI   INTMTYP,METROBE                                                  
*                                                                               
*--------------------------- PROGRAM STUFF ---------------------------*         
*                                                                               
         MVC   INTPNAME,QHPN14     PROGRAM NAME                                 
*        MVC   INTPNAM6,QHPN6      PROGRAM NAME (6-CHAR REPRESENTATION)         
         MVC   INTPTYP,QHDPTYP      PROGRAM TYPE                                
         MVC   INTPRSRC,QHPRSRC    PROGRAM SOURCE                               
*        MVC   INTAFFL,NTWKAFFL    AFFILIATION                                  
         PACK  DUB,QHPDOM                                                       
         CVB   RE,DUB                                                           
         STCM  RE,15,INTPNUM       PROGRAM NUMBER                               
         MVI   INTDTYP,1          SET UP REG1PREM IND                           
         CLI   PAVNF,C'1'                                                       
         BNE   *+14                                                             
         MVC   INTPNAME+9(5),=C'(NOR)'                                          
         MVI   INTDTYP,0                                                        
*                                                                               
*--------------------------- REVISION ID'S ---------------------------*         
*                                                                               
         MVI   INTREVID,X'FF'      REVISION ID FLAG                             
         MVI   INTREV,1            THIS REVISION IS DEFINED AS 1                
*                                                                               
* FILE CONVERSION ROUTINES GO HERE                                              
*                                                                               
*        L     R7,ARREC                                                         
*        LA    R7,4(R7)                                                         
*        L     R6,AIREC                                                         
*        GOTO1 =V(ADPROC)                                                       
*        SPACE 1                                                                
*        L     R7,=V(M2RECA)       R7-->REC WITH UNIVERSE FIGURES               
*        L     R6,AIREC                                                         
*        GOTO1 =V(BDPROC)                                                       
*          DATA SET DETN0110   AT LEVEL 077 AS OF 08/23/01                      
         LA    R4,CDMA2INT         TABLE CONTROL DMA                            
         LA    R7,QHA1EST          DMAIN                                        
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
*                                                                               
         LA    R4,CTSA2INT         TABLE CONTROL TSA                            
         LA    R7,QHA2EST          TSA IN                                       
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
                                                                                
         LA    R4,CUNV2INT         TABLE CONTROL UNIVERSE                       
         LA    R6,INTACCS                                                       
         L     R7,=V(M2RECA)           UNIVERSE IN                              
         LA    R7,(M2DK25-M2RCODE)(R7)                                          
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
         SPACE 1                                                                
                                                                                
         LA    R4,CUNH2INT         TABLE CONTROL UNIVERSE                       
         LA    R6,INTACCS                                                       
         L     R7,=V(M2RECA)           UNIVERSE IN                              
         LA    R7,(M2MA-M2RCODE)(R7)                                            
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
                                                                                
* NOW DO THE HOMES                                                              
         MVC   CRHOMES(HOMESEND-CRHOMES),ZEROS                                  
         MVC   CRHOMES+9-L'QHDRHH(L'QHDRHH),QHDRHH                              
         MVC   CRHH1+9-L'QHDRHH(L'QHDRHH),QHDWRTG                               
         MVC   CRHH2+9-L'QHDRHH(L'QHDRHH),QHDWRTG+L'QHDRHH                      
         MVC   CRHH3+9-L'QHDRHH(L'QHDRHH),QHDWRTG+L'QHDRHH*2                    
         MVC   CRHH4+9-L'QHDRHH(L'QHDRHH),QHDWRTG+L'QHDRHH*3                    
         MVC   CSHOMES+9-L'QHDRHS(L'QHDRHS),QHDRHS                              
         MVC   CPMETROA+9-L'QHMAHUT(L'QHMAHUT),QHMAHUT                          
         MVC   CSMETROA+9-L'QHMATHS(L'QHMATHS),QHMATHS                          
         MVC   CRMETROA+9-L'QHMARTG(L'QHMARTG),QHMARTG                          
         MVC   CPMETROB+9-L'QHMBHUT(L'QHMBHUT),QHMBHUT                          
         MVC   CSMETROB+9-L'QHMBTHS(L'QHMBTHS),QHMBTHS                          
         MVC   CRMETROB+9-L'QHMBRTG(L'QHMBRTG),QHMBRTG                          
         MVC   CTHOMES+9-L'QHA2HG(L'QHA2HG),QHA2HG                              
         MVC   CDHOMES+9-L'QHDTPH(L'QHDTPH),QHDTPH                              
         MVC   CDMETROA+9-L'QHMATPH(L'QHMATPH),QHMATPH                          
         MVC   CDMETROB+9-L'QHMBTPH(L'QHMBTPH),QHMBTPH                          
                                                                                
*                                                                               
         LA    R4,CHOM2INT         TABLE CONTROL TSA                            
         LA    R7,CRHOMES          TSA IN                                       
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
                                                                                
         BAS   RE,SETKEY                                                        
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*---------------------------- SORT RECORD ----------------------------*         
*                                                                               
SETKEY   NTR1                 BUILD PAV SORT KEY                                
         L     R6,AIREC                                                         
         USING PRKEY,R6                                                         
         LA    R6,4(R6)                                                         
         MVI   INTRTYP,C'P'                                                     
         MVI   PRCODE,PRCODEQU                                                  
         MVI   PRMEDIA,C'T'                                                     
         MVI   PRSRC,C'N'                                                       
         MVC   PRSTAT,INTSTA                                                    
         MVC   PRSTYP,INTSTYP      FORCE SATELLITES TO SORT TOG                 
         TM    INTSTYP,X'20'       SET UP SPILL STATION                         
         BZ    *+14                                                             
         MVI   INTSPILL,C'Y'                                                    
         MVC   PRKMKT,INTMRKT                                                   
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRBTYP,INTBTYP                                                   
         MVC   PRSTIM,INTSQH                                                    
         MVC   PRDW,INTDAYWK       SET UP DAY AND WEEK                          
         ZIC   RE,INTDAYWK                                                      
         SLL   RE,4                                                             
         STC   RE,PRDW                                                          
         TM    INTWEEKS,X'0F'                                                   
         BO    CNVDWX                                                           
         CLI   INTWEEKS,0                                                       
         BE    CNVDWX                                                           
         ZIC   RF,INTWEEKS         CALCULATE PROPER WEEKS                       
         SLL   RF,28                                                            
         SR    RE,RE                                                            
         LA    R1,1                                                             
CNVDW1   SLDL  RE,1                COUNT UP TO START WEEK                       
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         B     CNVDW2                                                           
         LA    R1,1(R1)                                                         
         B     CNVDW1                                                           
CNVDW2   ZIC   RE,PRDW             GET SHIFTED DAY                              
         SLL   R1,1                SHIFT START WEEK                             
         OR    RE,R1               INSERT INTO KEY                              
         STC   RE,PRDW                                                          
CNVDWX   MVC   INTDAYWK,PRDW       SET INTERIM DAY AND WEEK                     
         MVC   PRDW+1(1),INTDTYP   FORCE (NOR) TO SORT FIRST                    
         DROP  R6                                                               
         XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====================== MARKET RECORD ROUTINES =======================*         
*                                                                               
*----------------------------- M1 RECORD -----------------------------*         
*                                                                               
M1REC    L     R5,ARREC                                                         
         LA    R5,4(R5)                                                         
         USING M1DSECT,R5                                                       
         MVC   NPRES,M1ROUND                                                    
*        MVC   NTWKAFFL,M1NTWK     AFFILIATIONS                                 
         PACK  DUB,M1YR                                                         
         CVB   RE,DUB                                                           
         CH    RE,=H'1999'                                                      
         BH    *+12                                                             
         SH    RE,=H'1900'                                                      
         B     *+8                                                              
         SH    RE,=H'2000'                                                      
         CHI   RE,27                CHECK FOR Y2K COMPATIBILITY                 
         BH    *+8                                                              
         AHI   RE,100                                                           
         STC   RE,NSIBOOKY         YEAR                                         
         PACK  DUB,M1MO                                                         
         CVB   RE,DUB                                                           
         STC   RE,NSIBOOKM         MONTH                                        
         SPACE 1                                                                
         MVC   NSIBKYM,NSIBOOKY                                                 
         MVC   NSIBKYM+1(1),NSIBOOKM                                            
         SPACE 1                                                                
         CLI   FORCE,C'Y'                                                       
         BNE   *+10                                                             
         MVC   NSIBKYM,FILTBOOK+1                                               
*                                                                               
         CLC   M1RPTTYP(2),=C'ZC'                                               
         BNE   *+8                                                              
         MVI   ZEROCELL,1                                                       
*                                                                               
         CLC   M1RPTTYP(2),=C'LO'                                               
         BNE   *+8                                                              
         MVI   LIVEONLY,1                                                       
*                                                                               
         CLC   M1RPTTYP(2),=C'L3'                                               
         BNE   *+8                                                              
         MVI   LIVEPLU3,1                                                       
*                                                                               
         CLC   M1RPTTYP(2),=C'LS'                                               
         BNE   *+8                                                              
         MVI   LIVEPLSD,1                                                       
*                                                                               
         CLI   M1SAMTY1,C'H'                                                    
         BNE   OPENOK                                                           
         MVI   SAMPLEBT,C'H'                                                    
*                                                                               
         B     OPENOK                                                           
         DROP  R5                                                               
         SPACE 3                                                                
*----------------------------- M2 RECORD -----------------------------*         
*                                                                               
M2REC    LA    R1,RRECL                                                         
         L     R5,=V(M2RECA)                                                    
         MOVE  ((R5),(R1)),(R4)                                                 
         USING M2DSECT,R4                                                       
*                                                                               
         XC    BTYPTAB,BTYPTAB                                                  
         MVI   BTYPTAB,X'FF'                                                    
         MVC   TEMPMNO,M2MNO       MARKET NUMBER SAVED FOR AMREC                
         MVC   TEMPMKT,M2MN26      MARKET NAME SAVED FOR AMREC                  
*                                                                               
         B     OPENOK                                                           
         EJECT                                                                  
**********************************************************************          
*------------USE TABLE TO CONVERT FROM RREC TO IREC                             
* R4 = A(CONVERSION TABLE)                                                      
* R7 = A(START OF RATING SERVICE DEMO DATA)                                     
* R6 = A(START OF INTERIM RECORD DEMO DATA)                                     
CNVRTOI  NTR1                                                                   
         SHI   R6,4                                                             
         SHI   R7,9                                                             
CNVRTOI1 CLI   0(R4),X'FF'                                                      
         BE    CNVRTOIX                                                         
         ZIC   R5,0(R4)            GET RS FIELD NUMBER                          
         MH    R5,=H'9'            ADJUST FOR FLD LEN                           
         AR    R5,R7                                                            
         PACK  DUB,0(9,R5)         PACK IT                                      
         CVB   RF,DUB              AND CONVERT                                  
         ZIC   R5,1(R4)            GET INT FIELD NUMBER                         
         MH    R5,=H'4'            ADJUST FOR FLD LEN                           
         AR    R5,R6                                                            
         ST    RF,0(R5)            SAVE IT                                      
         LA    R4,2(R4)            NEXT FIELD                                   
         B     CNVRTOI1                                                         
*                                                                               
CNVRTOIX XIT1                                                                   
***********************************************************************         
FLTMKT   NTR1                                                                   
         SR    RF,RF                                                            
         MVI   PASSFLT,C'Y'                                                     
         ICM   RF,1,FILTMRKT                                                    
         BZ    FLTMKTX                                                          
         OC    INTMRKT,INTMRKT                                                  
         BZ    FLTMKTX                                                          
         LA    R1,FILTMRKT+1                                                    
*                                                                               
         TM    FLAGS1,NEGATIVE_FILTMRKT                                         
         BZ    FLTMKT10                                                         
FLTMKT05 CLC   INTMRKT,0(R1)                                                    
         BNE   *+12                                                             
         MVI   PASSFLT,C'N'                                                     
         B     FLTMKTX                                                          
         LA    R1,L'INTMRKT(R1)                                                 
         BCT   RF,FLTMKT05                                                      
         B     FLTMKTX                                                          
*                                                                               
FLTMKT10 CLC   INTMRKT,0(R1)                                                    
         BE    FLTMKTX                                                          
         LA    R1,L'INTMRKT(R1)                                                 
         BCT   RF,FLTMKT10                                                      
         MVI   PASSFLT,C'N'                                                     
FLTMKTX  J     EXIT                                                             
***********************************************************************         
MSGREC   NTR1                                                                   
         LA    RE,BTYPTAB          STORE UNIQUE BOOKTYPES IN TABLE              
MSGR05   CLI   0(RE),X'FF'                                                      
         BNE   MSGR10                                                           
         CLC   =X'FFFF',0(RE)      END OF TABLE?                                
         BE    MSGRX                                                            
         B     MSGR20                                                           
MSGR10   CLC   INTBTYP,0(RE)        NOT UNIQUE                                  
         BE    MSGRX                                                            
         LA    RE,1(RE)                                                         
         B     MSGR05                                                           
MSGR20   MVC   0(1,RE),INTBTYP                                                  
         MVI   1(RE),X'FF'                                                      
************************************************************                    
MSGR25   L     R6,AMREC                                                         
         LA    R6,4(R6)                                                         
         MVC   0(3,R6),TEMPMNO+3   MARKET NUMBER                                
         LA    R6,3(R6)                                                         
         MVI   0(R6),C','          ,                                            
         LA    R6,1(R6)                                                         
         LA    R1,TEMPMKT+25       MARKET NAME - SPACES                         
MSGR30   CLI   0(R1),X'40'                                                      
         BNE   MSGR40                                                           
         SHI   R1,1                                                             
         B     MSGR30                                                           
*                                                                               
MSGR40   LA    RF,TEMPMKT          CALCULATE LENGTH                             
         SR    R1,RF                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),TEMPMKT                                                  
         AHI   R1,1                                                             
         AR    R6,R1                                                            
         MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
*                                                                               
         MVC   0(1,R6),INTBTYP     BOOKTYPE                                     
         CLI   INTBTYP,0                                                        
         BE    MSGR70                                                           
*                                                                               
         GOTO1 CDEMTABS,DMCB,SPBOOKTB                                           
         ICM   RE,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)           LENGTH OF TABLE ENTRIES                       
         USING SPBKTYPD,RE                                                      
*                                                                               
MSGR50   CLI   0(RE),X'FF'        MUST FIND MARKET IN TABLE                     
         BE    MSGR71                                                           
         CLC   INTBTYP,SPBKTYPN                                                 
         BE    *+10                                                             
         AR    RE,RF                                                            
         B     MSGR50                                                           
*                                                                               
         MVC   0(L'SPBKTYPA,R6),SPBKTYPA                                        
         AHI   R6,L'SPBKTYPA                                                    
         B     MSGR80                                                           
*                                                                               
MSGR70   MVC   0(1,R6),=C' '                                                    
MSGR71   AHI   R6,1                                                             
*                                                                               
MSGR80   MVI   0(R6),C','                                                       
*                                                                               
         MVC   TEMPBK(2),NSIBKYM                                                
         MVI   TEMPBK+2,X'1'                                                    
         GOTO1 VDATCON,DMCB,(3,TEMPBK),(6,1(R6))                                
         AHI   R6,7                                                             
*                                                                               
         L     RF,AMREC                                                         
         SR    R6,RF                                                            
         STC   R6,1(RF)                                                         
         MVI   MSGFLAG,1                                                        
MSGRX    J     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
*              SORT RECORD PROCESSING FOR KEY SEQUENCING                        
*                                                                               
CNVWR    DS    0H                                                               
         L     R6,ASREC            POINT AT SORT RECORD                         
         LR    R2,R6               ADDRESSABILITY FOR INTERIM VALUES            
         LA    R6,4(R6)            POINT AT RECORD START                        
         USING PRKEY,R6                                                         
         CLC   PREVKEY,PRKEY       TEST FOR KEYS IN SEQUENCE                    
         BL    CNVWR2              OK                                           
         LA    RE,PREVKEY          PICK UP DAY/WEEK FROM PREV SORT KEY          
         ZIC   R1,PRDW-PRKEY(RE)                                                
         LA    R1,1(R1)            INCREMENT VALUE TO INSURE                    
         STC   R1,PRDW             SEQUENTIALITY                                
         STC   R1,INTDAYWK         FORCE OUTPUT PHASE TO USE BUMPED KEY         
         SPACE 1                                                                
CNVWR2   MVC   PREVKEY,PRKEY       UPDATE PREVIOUS KEY                          
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* READ ERROR - SET ERROR FLAG AND CLOSE FILE                                    
*OERROR  DS    0H                                                               
*        GOTO1 VLOGIO,DUB,1,C'***READ ERROR ON INPUT***'                        
*        CLOSER IN1                                                             
*        MVI   INTAPESW,X'82'                                                   
*        B     EXIT                                                             
*        SPACE 2                                                                
* WRONG LENGTH RECORD ON INPUT                                                  
*LR      GOTO1 VLOGIO,DUB,1,=C'WRONG LENGTH RECORD ON INPUT'                    
*        CLOSER IN1                                                             
*        MVI   INTAPESW,X'82'                                                   
*        B     EXIT                                                             
         SPACE 2                                                                
MORET    DS    0H'0'                                                            
*NDJOB   CLOSER IN1                                                             
ENDJOB   CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
PS1E     EQU   1                                                                
PARENTE  EQU   2                                                                
PS2E     EQU   4                                                                
S1EQU    EQU   8                                                                
S2EQU    EQU   16                                                               
CANOUT   EQU   32                                                               
OUTMARE  EQU   32                                                               
CANMARE  EQU   64                                                               
METROAE  EQU   1                                                                
METROBE  EQU   2                                                                
GLOBIND  EQU   4                                                                
         EJECT                                                                  
*          DATA SET DETN0110   AT LEVEL 077 AS OF 08/23/01                      
                                                                                
* TABLE TO CONVERT UNIVERSES FROM NSI TO DDS INTREC (M2 RECORD)                 
CUNV2INT DS    0C                                                               
         DC    AL1(RRV25,OUV25)                                                 
         DC    AL1(RRV611,OUV611)                                               
         DC    AL1(RRM1217,OUM1217)                                             
         DC    AL1(RRW1217,OUW1217)                                             
         DC    AL1(RRM1820,OUM1820)                                             
         DC    AL1(RRM2124,OUM2124)                                             
         DC    AL1(RRM2534,OUM2534)                                             
         DC    AL1(RRM3549,OUM3549)                                             
         DC    AL1(RRM5054,OUM5054)                                             
         DC    AL1(RRM5564,OUM5564)                                             
         DC    AL1(RRM65O,OUM65O)                                               
         DC    AL1(RRW1820,OUW1820)                                             
         DC    AL1(RRW2124,OUW2124)                                             
         DC    AL1(RRW2534,OUW2534)                                             
         DC    AL1(RRW3549,OUW3549)                                             
         DC    AL1(RRW5054,OUW5054)                                             
         DC    AL1(RRW5564,OUW5564)                                             
         DC    AL1(RRW65O,OUW65O)                                               
         DC    AL1(RRWWRK,OUWWRK)                                               
         DC    X'FF'                                                            
CDMA2INT DS    0C                                                               
         DC    AL1(RRV25,ODV25)                                                 
         DC    AL1(RRV611,ODV611)                                               
         DC    AL1(RRM1217,ODM1217)                                             
         DC    AL1(RRW1217,ODW1217)                                             
         DC    AL1(RRM1820,ODM1820)                                             
         DC    AL1(RRM2124,ODM2124)                                             
         DC    AL1(RRM2534,ODM2534)                                             
         DC    AL1(RRM3549,ODM3549)                                             
         DC    AL1(RRM5054,ODM5054)                                             
         DC    AL1(RRM5564,ODM5564)                                             
         DC    AL1(RRM65O,ODM65O)                                               
         DC    AL1(RRW1820,ODW1820)                                             
         DC    AL1(RRW2124,ODW2124)                                             
         DC    AL1(RRW2534,ODW2534)                                             
         DC    AL1(RRW3549,ODW3549)                                             
         DC    AL1(RRW5054,ODW5054)                                             
         DC    AL1(RRW5564,ODW5564)                                             
         DC    AL1(RRW65O,ODW65O)                                               
         DC    AL1(RRWWRK,ODWWRK)                                               
         DC    X'FF'                                                            
CTSA2INT DS    0C                                                               
         DC    AL1(RRV25,OTV25)                                                 
         DC    AL1(RRV611,OTV611)                                               
         DC    AL1(RRM1217,OTM1217)                                             
         DC    AL1(RRW1217,OTW1217)                                             
         DC    AL1(RRM1820,OTM1820)                                             
         DC    AL1(RRM2124,OTM2124)                                             
         DC    AL1(RRM2534,OTM2534)                                             
         DC    AL1(RRM3549,OTM3549)                                             
         DC    AL1(RRM5054,OTM5054)                                             
         DC    AL1(RRM5564,OTM5564)                                             
         DC    AL1(RRM65O,OTM65O)                                               
         DC    AL1(RRW1820,OTW1820)                                             
         DC    AL1(RRW2124,OTW2124)                                             
         DC    AL1(RRW2534,OTW2534)                                             
         DC    AL1(RRW3549,OTW3549)                                             
         DC    AL1(RRW5054,OTW5054)                                             
         DC    AL1(RRW5564,OTW5564)                                             
         DC    AL1(RRW65O,OTW65O)                                               
         DC    AL1(RRWWRK,OTWWRK)                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
CHOM2INT DS    0C                      CONVERT HOMES TO INTERIM                 
         DC    AL1(WRHOMES,ORHOMES)                                             
         DC    AL1(WRHH1,ORHH1)                                                 
         DC    AL1(WRHH2,ORHH2)                                                 
         DC    AL1(WRHH3,ORHH3)                                                 
         DC    AL1(WRHH4,ORHH4)                                                 
         DC    AL1(WSHOMES,OSHOMES)                                             
         DC    AL1(WPMETROA,OPMETROA)                                           
         DC    AL1(WSMETROA,OSMETROA)                                           
         DC    AL1(WRMETROA,ORMETROA)                                           
         DC    AL1(WPMETROB,OPMETROB)                                           
         DC    AL1(WSMETROB,OSMETROB)                                           
         DC    AL1(WRMETROB,ORMETROB)                                           
         DC    AL1(WTHOMES,OTHOMES)                                             
         DC    AL1(WDHOMES,ODHOMES)                                             
         DC    AL1(WDMETROA,ODMETROA)                                           
         DC    AL1(WDMETROB,ODMETROB)                                           
         DC    X'FF'                                                            
                                                                                
CUNH2INT DS    0C                      CONVERT HOMES TO INTERIM                 
         DC    AL1(UUMETROA,OUMETROA)                                           
         DC    AL1(UUMETROB,OUMETROB)                                           
         DC    AL1(UUHOMES,OUHOMES)                                             
         DC    AL1(UUA1HH,OUA1HH)                                               
         DC    AL1(UUA2HH,OUA2HH)                                               
         DC    AL1(UUA3HH,OUA3HH)                                               
         DC    X'FF'                                                            
UUMETROA EQU   1                                                                
UUMETROB EQU   2                                                                
UUHOMES  EQU   4                                                                
UUA1HH   EQU   6                                                                
UUA2HH   EQU   7                                                                
UUA3HH   EQU   8                                                                
*          DATA SET DETN0110   AT LEVEL 077 AS OF 08/23/01                      
         EJECT                                                                  
* INTERIM RECORD DISPLACEMENTS                                                  
ORHOMES  EQU   1                                                                
ORHH1    EQU   2                                                                
ORHH2    EQU   3                                                                
ORHH3    EQU   4                                                                
ORHH4    EQU   5                                                                
OSHOMES  EQU   6                                                                
OPMETROA EQU   7                                                                
OSMETROA EQU   8                                                                
ORMETROA EQU   9                                                                
OPMETROB EQU   10                                                               
OSMETROB EQU   11                                                               
ORMETROB EQU   12                                                               
OTHOMES  EQU   13                                                               
ODHOMES  EQU   14                                                               
OTWWRK   EQU   15                                                               
ODWWRK   EQU   16                                                               
OTW65O   EQU   17                                                               
ODW65O   EQU   18                                                               
OTM65O   EQU   19                                                               
ODM65O   EQU   20                                                               
ODMETROA EQU   21                                                               
ODMETROB EQU   22                                                               
OTM1217  EQU   23                                                               
OTW1217  EQU   24                                                               
OTM1820  EQU   25                                                               
OTW1820  EQU   26                                                               
OTM2124  EQU   27                                                               
OTW2124  EQU   28                                                               
OTM2534  EQU   29                                                               
OTW2534  EQU   30                                                               
OTM3549  EQU   31                                                               
OTW3549  EQU   32                                                               
OTM5564  EQU   33                                                               
OTW5564  EQU   34                                                               
OTM5054  EQU   35                                                               
OTW5054  EQU   36                                                               
OTV25    EQU   37                                                               
ODV25    EQU   38                                                               
OTV611   EQU   39                                                               
ODV611   EQU   40                                                               
OUHOMES  EQU   41                                                               
OUM65O   EQU   42                                                               
OUW65O   EQU   43                                                               
OUWWRK   EQU   44                                                               
OUA1HH   EQU   45                                                               
OUA2HH   EQU   46                                                               
OUA3HH   EQU   47                                                               
OUMETROA EQU   48                                                               
OUMETROB EQU   49                                                               
OUM1217  EQU   50                                                               
OUW1217  EQU   51                                                               
OUM1820  EQU   52                                                               
OUW1820  EQU   53                                                               
OUM2124  EQU   54                                                               
OUW2124  EQU   55                                                               
OUM2534  EQU   56                                                               
OUW2534  EQU   57                                                               
OUM3549  EQU   58                                                               
OUW3549  EQU   59                                                               
OUM5564  EQU   60                                                               
OUW5564  EQU   61                                                               
OUM5054  EQU   62                                                               
OUW5054  EQU   63                                                               
OUV25    EQU   64                                                               
OUV611   EQU   65                                                               
ODM1217  EQU   66                                                               
ODW1217  EQU   67                                                               
ODM1820  EQU   68                                                               
ODW1820  EQU   69                                                               
ODM2124  EQU   70                                                               
ODW2124  EQU   71                                                               
ODM2534  EQU   72                                                               
ODW2534  EQU   73                                                               
ODM3549  EQU   74                                                               
ODW3549  EQU   75                                                               
ODM5564  EQU   76                                                               
ODW5564  EQU   77                                                               
ODM5054  EQU   78                                                               
ODW5054  EQU   79                                                               
OMHOMES  EQU   80                                                               
OMWWRK   EQU   81                                                               
OMW65O   EQU   82                                                               
OMM65O   EQU   83                                                               
OMMETROA EQU   84                                                               
OMMETROB EQU   85                                                               
OMM1217  EQU   86                                                               
OMW1217  EQU   87                                                               
OMM1820  EQU   88                                                               
OMW1820  EQU   89                                                               
OMM2124  EQU   90                                                               
OMW2124  EQU   91                                                               
OMM2534  EQU   92                                                               
OMW2534  EQU   93                                                               
OMM3549  EQU   94                                                               
OMW3549  EQU   95                                                               
OMM5564  EQU   96                                                               
OMW5564  EQU   97                                                               
OMM5054  EQU   98                                                               
OMW5054  EQU   99                                                               
OMV25    EQU   100                                                              
OMV611   EQU   101                                                              
OQHOMES  EQU   102                                                              
OQWWRK   EQU   103                                                              
OQW65O   EQU   104                                                              
OQM65O   EQU   105                                                              
OQM1217  EQU   106                                                              
OQW1217  EQU   107                                                              
OQM1820  EQU   108                                                              
OQW1820  EQU   109                                                              
OQM2124  EQU   110                                                              
OQW2124  EQU   111                                                              
OQM2534  EQU   112                                                              
OQW2534  EQU   113                                                              
OQM3549  EQU   114                                                              
OQW3549  EQU   115                                                              
OQM5564  EQU   116                                                              
OQW5564  EQU   117                                                              
OQM5054  EQU   118                                                              
OQW5054  EQU   119                                                              
OQV25    EQU   120                                                              
OQV211   EQU   121                                                              
ORV2O    EQU   122                                                              
ORV18O   EQU   123                                                              
ORV1234  EQU   124                                                              
ORV1224  EQU   125                                                              
ORV1217  EQU   126                                                              
ORV611   EQU   127                                                              
ORV211   EQU   128                                                              
ORW18O   EQU   129                                                              
ORW1834  EQU   130                                                              
ORW1849  EQU   131                                                              
ORW2549  EQU   132                                                              
ORW2554  EQU   133                                                              
ORW1224  EQU   134                                                              
ORW2564  EQU   135                                                              
ORW1234  EQU   136                                                              
ORM18O   EQU   137                                                              
ORM1834  EQU   138                                                              
ORM1849  EQU   139                                                              
ORM2554  EQU   140                                                              
ORM2564  EQU   141                                                              
ORWWRK   EQU   142                                                              
ORM2549  EQU   143                                                              
ORA1849  EQU   144                                                              
ORA1834  EQU   145                                                              
ORA2554  EQU   146                                                              
OPV2O    EQU   147                                                              
OPV18O   EQU   148                                                              
OPV1234  EQU   149                                                              
OPV1224  EQU   150                                                              
OPV1217  EQU   151                                                              
OPV611   EQU   152                                                              
OPV211   EQU   153                                                              
OPW18O   EQU   154                                                              
OPW1834  EQU   155                                                              
OPW1849  EQU   156                                                              
OPW2549  EQU   157                                                              
OPW2554  EQU   158                                                              
OPW1224  EQU   159                                                              
OPW2564  EQU   160                                                              
OPW1234  EQU   161                                                              
OPM18O   EQU   162                                                              
OPM1834  EQU   163                                                              
OPM1849  EQU   164                                                              
OPM2554  EQU   165                                                              
OPM2564  EQU   166                                                              
OPWWRK   EQU   167                                                              
OPM2549  EQU   168                                                              
OPA1849  EQU   169                                                              
OPA1834  EQU   170                                                              
OPA2554  EQU   171                                                              
         EJECT                                                                  
* FIELD DISPLACEMENTS FOR RATING SERVICE RECORDS                                
RRV25    EQU   1                                                                
RRV611   EQU   2                                                                
RRM1217  EQU   3                                                                
RRW1217  EQU   5                                                                
RRM1820  EQU   7                                                                
RRM2124  EQU   8                                                                
RRM2534  EQU   9                                                                
RRM3549  EQU   10                                                               
RRM5054  EQU   12                                                               
RRM5564  EQU   13                                                               
RRM65O   EQU   14                                                               
RRW1820  EQU   15                                                               
RRW2124  EQU   16                                                               
RRW2534  EQU   17                                                               
RRW3549  EQU   18                                                               
RRW5054  EQU   20                                                               
RRW5564  EQU   21                                                               
RRW65O   EQU   22                                                               
RRWWRK   EQU   23                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
SQHTAB   DS    0XL12                                                            
SQH1     DC    3X'0'                                                            
SQH2     DC    3X'0'                                                            
SQH3     DC    3X'0'                                                            
SQH4     DC    3X'0'                                                            
SQHTABX  EQU   *                                                                
*                                                                               
DAYTABL  DS    0CL5                                                             
         DC    C'00',X'00057C'      M-F DAYTIME                                 
         DC    C'01',X'010140'      MON                                         
         DC    C'02',X'020120'      TUE                                         
         DC    C'03',X'030110'      WED                                         
         DC    C'04',X'040108'      THU                                         
         DC    C'05',X'050104'      FRI                                         
         DC    C'06',X'09017C'      M-F OR MULTI-DAY AVERAGE                    
         DC    C'07',X'060102'      SAT                                         
         DC    C'08',X'070101'      SUN                                         
         DC    C'09',X'08077F'      M-S AVERAGE                                 
         DC    X'000000'           END OF LIST                                  
         DC    X'00'                                                            
                                                                                
CRHOMES  DS    CL9                                                              
CRHH1    DS    CL9                                                              
CRHH2    DS    CL9                                                              
CRHH3    DS    CL9                                                              
CRHH4    DS    CL9                                                              
CSHOMES  DS    CL9                                                              
CPMETROA DS    CL9                                                              
CSMETROA DS    CL9                                                              
CRMETROA DS    CL9                                                              
CPMETROB DS    CL9                                                              
CSMETROB DS    CL9                                                              
CRMETROB DS    CL9                                                              
CTHOMES  DS    CL9                                                              
CDHOMES  DS    CL9                                                              
CDMETROA DS    CL9                                                              
CDMETROB DS    CL9                                                              
HOMESEND DS    0C                                                               
*                                                                               
WRHOMES  EQU   1                                                                
WRHH1    EQU   2                                                                
WRHH2    EQU   3                                                                
WRHH3    EQU   4                                                                
WRHH4    EQU   5                                                                
WSHOMES  EQU   6                                                                
WPMETROA EQU   7                                                                
WSMETROA EQU   8                                                                
WRMETROA EQU   9                                                                
WPMETROB EQU   10                                                               
WSMETROB EQU   11                                                               
WRMETROB EQU   12                                                               
WTHOMES  EQU   13                                                               
WDHOMES  EQU   14                                                               
WDMETROA EQU   15                                                               
WDMETROB EQU   16                                                               
         LTORG                                                                  
         EJECT                                                                  
ZEROS    DC    100C'0'                                                          
NFRST    DC    X'01'                                                            
RELOFRST DC    X'01'                                                            
NPRES    DC    X'00'                                                            
ZEROCELL DS    XL1                                                              
LIVEONLY DS    XL1                                                              
LIVEPLU3 DS    XL1                                                              
LIVEPLSD DS    XL1                                                              
SAMPLEBT DS    C                   BOOK TYPE FROM SAMPLE RECORD                 
PRESDIV  DS    F                                                                
PRESMULT DS    H                                                                
NSIBOOKY DS    CL1                                                              
NSIBOOKM DS    CL1                                                              
NSIBKYM  DS    CL2                                                              
NTWKAFFL DS    CL5                 NETWORK AFFILIATIONS                         
PREVKEY  DC    XL20'00'                                                         
DCNT     DC    F'0'                                                             
PASSFLT  DC    X'00'                                                            
*                                                                               
DAYS     DS    X                                                                
DURTAB   DS    XL30                                                             
DURTABX  DS    CL1                                                              
TEMPQHR  DS    XL2                                                              
TEMPCNT  DS    XL1                                                              
SVSTA    DS    CL5                                                              
SVQHCDE  DS    CL5                                                              
SVSTYP   DS    CL2                                                              
SVPNAMD  DS    CL255               SAVE PROGRAM NAMES AND DAYS                  
APNAMD   DS    F                                                                
*                                                                               
TEMPMNO  DS    CL6                                                              
TEMPMKT  DS    CL26                                                             
TEMPBK   DS    XL3                                                              
DEMTABS  DC    CL8'T00AD1'                                                      
CDEMTABS DS    V                                                                
BTYPTAB  DS    0XL10                                                            
         DC    X'FF'                                                            
         DS    XL9                                                              
         DC    X'FF'                                                            
*                                                                               
IREC     DS    (RRECL)C                                                         
         EJECT                                                                  
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=RRECL,                                            X        
               MACRF=GM,                                               X        
               EODAD=MORET                                                      
IN1A     DS    (10*RRECL)C                                                      
RRECL    EQU   1214                                                             
M2RECA   CSECT                                                                  
         DS    (RRECL)C                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DEDEMFILE                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DEDEMCNVD                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DEDEMTABD                                                      
***********************************************************************         
         EJECT                                                                  
*============================= IREC DSECT ============================*         
*          DATA SET DEINTD     AT LEVEL 005 AS OF 11/17/87                      
* DSECT TO COVER INTERIM CONVERSION RECORDS                                     
*                                                                               
INTERD   DSECT                                                                  
INTRECLN DS    F                   RECORD LENGTH                                
INTKEY   DS    CL30                SORT KEY                                     
INTVALS  DS    0CL20               FILTERABLE VALUES                            
INTRTYP  DS    CL1                 RECORD TYPE                                  
INTMRKT  DS    XL2                 MARKET NUMBER                                
INTSTA   DS    CL5                 STATION CALL LETTERS                         
INTBOOK  DS    XL2                 BOOK (BINARY YYMM)                           
INTSTYP  DS    XL1                 STATION TYPE                                 
INTDAYWK DS    XL1                 DAY & WEEK                                   
INTSQH   DS    XL1                 START QTR HR                                 
INTEQH   DS    XL1                 END QTR HR                                   
INTSPILL DS    C                   Y=SPILL MARKET                               
INTADUR  DS    CL1                 ACTUAL DURATION FOR PRGM RECS.               
INTPNO   DS    CL2                 PROG NUMBER(SYND AND NETW PROGS)             
INTBTYP  DS    CL1                 INTERNALLY GENERATED BOOK TYPE               
         DS    CL1                                                              
INTDATA  DS    0C                  ALPHA DATA & BINARY VALUES                   
*          DATA SET DEINTPAV3D AT LEVEL 003 AS OF 05/10/00                      
         SPACE 2                                                                
*                                  PAV FILE CONVERSION FIELDS                   
INTMTYP  DS    X                   MARKET TYPE                                  
INTREVID DS    XL1                 REVISION ID = X'FF'                          
INTREV   DS    XL1                 REVISION NUMBER                              
INTDTYP  DS    X                   DATA TYPE                                    
INTWEEKS DS    X                   ACTIVE WEEKS                                 
INTPNAME DS    CL14                PROGRAM NAME                                 
INTPNAM6 DS    CL6                 6 CHAR PROGRAM NAME                          
INTPNUM  DS    XL4                 PROGRAM NUMBER                               
INTPTYP  DS    CL2                 PROGRAM TYPE                                 
INTAFFL  DS    CL5                 AFFILIATIONS                                 
INTPRSRC DS    CL2                 PROGRAM SOURCE                               
INTNDAYS DS    XL1                 NUMBER OF DAYS                               
         DS    XL7                 (SPARE)                                      
INTACCS  DS    0X                  ACCUMULATOR VALUES                           
***********************************************************************         
*          DATA SET DEPN0110   AT LEVEL 093 AS OF 06/25/08                      
***********************************************************************         
         EJECT                                                                  
         SPACE 2                                                                
INTABD   DSECT                     DSECT FOR NET-IN-TAB-COUNTS                  
ITMETA   DS    CL3       P         METRO A                                      
ITMETB   DS    CL3       P         METRO B                                      
ITDMA    DS    CL3       P         DMA                                          
ITNTA    DS    CL3       P         NSI AREA                                     
         SPACE 2                                                                
QHTD     DSECT                     DSECT FOR QUARTER-HOUR TRENDS                
QHTMO    DS    CL2       C         REPORT PERIOD                                
QHTYR    DS    CL2       C         REPORT YEAR                                  
         SPACE 2                                                                
M1STATD  DSECT                     DSECT FOR REPORTABLE STATIONS                
M1SCDE   DS    CL2       P         STATION CODE                                 
M1STATUS DS    CL1       C         REPORTABILITY STATUS                         
M1AFFL   DS    CL7       C         NETWORK AFFILIATION                          
M1STYP   DS    CL1       C         STATION TYPE                                 
         EJECT                                                                  
*          DATA SET DETN0110   AT LEVEL 062 AS OF 08/17/01                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*==================== MARKET INFORMATION RECORD 1 ====================*         
*                                                                               
M1DSECT  DSECT                                                                  
M1RCDE   DS    CL3                 RECORD ID =001                               
M1MNO    DS    CL6                 MARKET NUMBER                                
M1MRI    DS    CL1                 MULTIPLE RECORD INDICATOR 1-3                
         DS    CL2                 (FOR FUTURE USE) ZERO                        
         DS    CL3                 (FOR FUTURE USE) BLANK                       
         DS    CL5                 (FOR FUTURE USE) ZERO                        
         DS    CL4                 (FOR FUTURE USE) ZERO                        
         DS    CL6                 (FOR FUTURE USE) JIC                         
M1YR     DS    CL4       C         REPORT YEAR                                  
M1MO     DS    CL2       C         REPORT MONTH  1-12,99=DOES NOT APPLY         
         DS    CL4                 JIC                                          
M1START  DS    CL8                 SURVEY START MMDDYYYY                        
M1END    DS    CL8                 SURVEY END MMDDYYYY                          
         DS    CL1                 JIC                                          
M1WKS    DS    CL2                 NUMBER OF WEEKS MEASURED                     
*                                   MAY BE LESS THAT END-START                  
         DS    C                   JIC                                          
M1SDW1   DS    CL8                 WEEK1 MMDDYYYY                               
M1NDW1   DS    C                   NUMBER OF DAYS                               
M1SDW2   DS    CL8                 WEEK2 MMDDYYYY                               
M1NDW2   DS    C                   NUMBER OF DAYS                               
M1SDW3   DS    CL8                 WEEK3 MMDDYYYY                               
M1NDW3   DS    C                   NUMBER OF DAYS                               
M1SDW4   DS    CL8                 WEEK4 MMDDYYYY                               
M1NDW4   DS    C                   NUMBER OF DAYS                               
M1SDW5   DS    CL8                 WEEK5 MMDDYYYY                               
M1NDW5   DS    C                   NUMBER OF DAYS                               
M1SDW6   DS    CL8                 WEEK6 MMDDYYYY                               
M1NDW6   DS    C                   NUMBER OF DAYS                               
M1SDW7   DS    CL8                 WEEK7 MMDDYYYY                               
M1NDW7   DS    C                   NUMBER OF DAYS                               
M1SDW8   DS    CL8                 WEEK8 MMDDYYYY                               
M1NDW8   DS    C                   NUMBER OF DAYS                               
M1SDW9   DS    CL8                 WEEK9 MMDDYYYY                               
M1NDW9   DS    C                   NUMBER OF DAYS                               
M1SDW10  DS    CL8                 WEEK10 MMDDYYYY                              
M1NDW10  DS    C                   NUMBER OF DAYS                               
M1SDW11  DS    CL8                 WEEK11 MMDDYYYY                              
M1NDW11  DS    C                   NUMBER OF DAYS                               
M1SDW12  DS    CL8                 WEEK12 MMDDYYYY                              
M1NDW12  DS    C                   NUMBER OF DAYS                               
         DS    CL9                 JIC                                          
M1TZ     DS    CL2       C         TIME ZONE                                    
*                        01=EASTERN 02=CENTRAL 03=MOUNTAIN 04=PACIFIC           
*                        05=ALASKA  06=HAWAIIAN                                 
M1MTYP   DS    CL2       C         MARKET TYPE                                  
*                                01=SINGLE METRO/METERED 02=MULTI-METER         
*                                05=NON METRO/DIARY      07=MULTI-DIARY         
*                                09=SINGLE METRO/CENTRAL-DIARY                  
*                                99=OTHER                                       
M1SERV   DS    CL5                 NIELSEN SERVICE                              
*                                NSI = NIELSEN STATION INDEX                    
*                                NHI = NIELSEN HOMEVIDEO INDEX                  
*                                NHSI= NIELSEN HISPANIC STA INDEX               
M1RPTTYP DS    CL3                 REPORT TYPE CODE                             
*                                SB=NSI BLACK ETHNIC                            
*                                SC=NSI CABLE(CABLE NETS ONLY)                  
*                                SH=NSI HISPANIC ETHNIC                         
*                                SO=NSI OUTSIDE "SPILL" RPT                     
*                                SR=NSI SRA                                     
*                                SS=NSI STATE SUMMARY                           
*                                ST=NSI TOTAL MARKET(STAND VIP)                 
*                                SX=NSI EXCLUSION                               
*                                SZ=NSI OTHER                                   
*                                CC=NHI CABLE(CABLE HH ONLY)                    
*                                CT=NHI TOT MARKET(ALL HH)                      
*                                CZ=NHI OTHER                                   
*                                HH=NHSI HISPANIC                               
*                                HZ=NHSI OTHER                                  
         DS    C                   JIC                                          
M1SAMTY1 DS    C                   SAMPLE TYPE(GRP1 AREA)                       
*                                B=AFRICAN-AMERICAN                             
*                                C=CABLE                                        
*                                H=HISPANIC                                     
*                                T=TOTAL SAMPLE                                 
*                                Z=OTHER(CUSTOM SAMPLE)                         
M1SAMTY2 DS    C                   SAMPLE TYPE(GRP2 AREA)                       
*                                B=AFRICAN-AMERICAN                             
*                                C=CABLE                                        
*                                H=HISPANIC                                     
*                                T=TOTAL SAMPLE                                 
*                                Z=OTHER(CUSTOM SAMPLE)                         
M1SRCCDE DS    CL2                 BROADCAST/CABLE SOURCE CODE                  
*                                B=BROADCAST ONLY                               
*                                C=CABLE ONLY                                   
*                                O=OUTSIDE  "SPILL"                             
*                                R=BROAD/CAB(STANDARD CASE)                     
*                                Z=CUSTOM                                       
M1MDIND  DS    C                   METER/DIARY INDICATOR                        
*                                  D=DIARY                                      
*                                  M=METER AND/OR METER ADJ                     
*                                  P=PEOPLE METER                               
M1METIND DS    CL1                 METRO INDICATOR                              
*                                  0=NO MET/CENTRAL REPORTED                    
*                                  1=1 MET/CENTRAL REPORTED                     
*                                  2=2 MET/CENTRAL REPORTED                     
*                                  3=3 MET/CENTRAL REPORTED                     
M1ROUND  DS    CL1       C         ROUNDING CONTROL                             
*                                  1=THOUSANDS                                  
*                                  2=HUNDREDS                                   
M1SPACT  DS    CL1                 SPECIAL ACTIVITY IND.(Y=YES)                 
         DS    CL2       C         JIC                                          
M1INTAB  DS    CL96      P         NET-IN-TAB COUNTS (4)                        
M1QHTRND DS    CL24      C         QUARTER HOUR TRENDS (4)                      
M1ADJDMA DS    CL18      P         ADJACENT DMAS (3)                            
         DS    684C                FILLER                                       
         EJECT                                                                  
*==================== MARKET INFORMATION RECORD 2 ====================*         
*                         UNIVERSES                                             
M2DSECT  DSECT                                                                  
M2RCODE  DS    CL3       C         RECORD CODE = 002                            
M2MNO    DS    CL6       C         MARKET NUMBER                                
M2MRI    DS    CL1                 MULTIPLE RECORD INDICATOR 1-3                
         DS    CL2                 ZERO FILLED                                  
         DS    CL3                 BLANK                                        
         DS    CL5                 ZERO                                         
         DS    CL4                 ZERO                                         
M2MN26   DS    CL26                NSI MARKET NAME SHORT                        
M2MN55   DS    CL55                NSI MARKET NAME LONG                         
         DS    CL44                JIC                                          
*                                                                               
* TV HOUSEHOLDS UNIVERSE ESTIMATES (0)                                          
M2MA     DS    CL9       C         METRO A                                      
M2MB     DS    CL9       C         METRO B                                      
M2MC     DS    CL9       C         METRO C                                      
M2DMA    DS    CL9       C         DMA/SRA                                      
         DS    CL9       C         JIC                                          
M2ADJ1   DS    CL9       C         ADJ DMA 1                                    
M2ADJ2   DS    CL9       C         ADJ DMA 2                                    
M2ADJ3   DS    CL9       C         ADJ DMA 3                                    
         DS    CL158               JIC                                          
M2NSD    DS    C                   NONSTANDARD DEMO INDICATOR                   
*                                C=CUSTOM (DEFINITIONS IN REC 05)               
*                                                                               
* DMA UNIVERSE ESTIMATES (00) OR (000)                                          
* NSI AREA OR GROUP 2 UNIVERSE ESTIMATES (00) OR (000)                          
*                                                                               
M2DK25   DS    CL9       C         KIDS                                         
M2DK611  DS    CL9       C                                                      
*                                  TEENS                                        
M2DM1214 DS    CL9       C                                                      
M2DM1517 DS    CL9       C                                                      
M2DF1214 DS    CL9       C                                                      
M2DF1517 DS    CL9       C                                                      
*                                                                               
M2DM1820 DS    CL9       C         MEN                                          
M2DM2124 DS    CL9       C                                                      
M2DM2534 DS    CL9       C                                                      
M2DM3544 DS    CL9       C                                                      
M2DM4549 DS    CL9       C                                                      
M2DM5054 DS    CL9       C                                                      
M2DM5564 DS    CL9       C                                                      
M2DM65O  DS    CL9       C                                                      
*                                                                               
M2DW1820 DS    CL9       C         WOMEN                                        
M2DW2124 DS    CL9       C                                                      
M2DW2534 DS    CL9       C                                                      
M2DW3544 DS    CL9       C                                                      
M2DW4549 DS    CL9       C                                                      
M2DW5054 DS    CL9       C                                                      
M2DW5564 DS    CL9       C                                                      
M2DW65O  DS    CL9       C                                                      
M2DWWRK  DS    CL9       C                                                      
         DS    CL9       C SPARE                                                
         DS    CL9       C SPARE                                                
         DS    CL9       C SPARE                                                
         DS    CL9       C SPARE                                                
M2NK25   DS    CL9       C         KIDS                                         
M2NK611  DS    CL9       C                                                      
*                                  TEENS                                        
M2NM1214 DS    CL9       C                                                      
M2NM1517 DS    CL9       C                                                      
M2NF1214 DS    CL9       C                                                      
M2NF1517 DS    CL9       C                                                      
*                                                                               
M2NM1820 DS    CL9       C         MEN                                          
M2NM2124 DS    CL9       C                                                      
M2NM2534 DS    CL9       C                                                      
M2NM3544 DS    CL9       C                                                      
M2NM4549 DS    CL9       C                                                      
M2NM5054 DS    CL9       C                                                      
M2NM5564 DS    CL9       C                                                      
M2NM65O  DS    CL9       C                                                      
*                                                                               
M2NW1820 DS    CL9       C         WOMEN                                        
M2NW2124 DS    CL9       C                                                      
M2NW2534 DS    CL9       C                                                      
M2NW3544 DS    CL9       C                                                      
M2NW4549 DS    CL9       C                                                      
M2NW5054 DS    CL9       C                                                      
M2NW5564 DS    CL9       C                                                      
M2NW65O  DS    CL9       C                                                      
M2NWWRK  DS    CL9       C                                                      
         DS    CL9       C SPARE                                                
         DS    CL9       C SPARE                                                
         DS    CL9       C SPARE                                                
         DS    CL9       C SPARE                                                
         DS    531C                                                             
*                                                                               
         EJECT                                                                  
*==================== MARKET INFORMATION RECORD 3 ====================*         
*                     MINIMUM SAMPLE SIZES                                      
M3DSECT  DSECT                                                                  
M3RCODE  DS    CL3       C         RECORD CODE = 003                            
M3MNO    DS    CL6       C         MARKET NUMBER                                
M3MRI    DS    CL1                 MULTIPLE RECORD INDICATOR 1-3                
         DS    CL2                 ZERO FILLED                                  
         DS    CL3                 BLANK                                        
         DS    CL5                 ZERO                                         
         DS    CL4                 ZERO                                         
         DS    CL5                 ZERO                                         
M3MCLASS DS    CL3                 MARKET CLASS                                 
* GROUP 1 AREA DEMOGRAPHICS                                                     
M31K211  DS    CL6       C         KIDS                                         
M31K611  DS    CL6       C                                                      
*                                  TEENS                                        
M31V1217 DS    CL6       C                                                      
M31F1217 DS    CL6       C                                                      
*                                                                               
M31MO18  DS    CL6       C                                                      
M31M1834 DS    CL6       C                                                      
M31M1849 DS    CL6       C         MEN                                          
M31M2149 DS    CL6       C                                                      
M31M2549 DS    CL6       C                                                      
M31M2554 DS    CL6       C                                                      
M31MO50  DS    CL6       C                                                      
*                                                                               
M31WO18  DS    CL6       C                                                      
M31W1224 DS    CL6       C                                                      
M31W1834 DS    CL6       C                                                      
M31W1849 DS    CL6       C         WOMEN                                        
M31W2549 DS    CL6       C                                                      
M31W2554 DS    CL6       C                                                      
M31W2564 DS    CL6       C                                                      
M31WO50  DS    CL6       C                                                      
M31WWORK DS    CL6       C                                                      
*                                  VIEWERS                                      
M31VO2   DS    CL6       C                                                      
M31VO18  DS    CL6       C                                                      
M31V1224 DS    CL6       C                                                      
M31V1234 DS    CL6       C                                                      
M31V1834 DS    CL6       C                                                      
M31V1849 DS    CL6       C                                                      
M31V2149 DS    CL6       C                                                      
M31V2554 DS    CL6       C                                                      
M31VO35  DS    CL6       C                                                      
M31V3564 DS    CL6       C                                                      
M31VO50  DS    CL6       C                                                      
         DS    CL102     C JIC                                                  
         DS    CL12      C JIC                                                  
* GROUP 1 AREA DEMOGRAPHICS                                                     
M32K211  DS    CL6       C         KIDS                                         
M32K611  DS    CL6       C                                                      
*                                  TEENS                                        
M32V1217 DS    CL6       C                                                      
M32F1217 DS    CL6       C                                                      
*                                                                               
M32MO18  DS    CL6       C                                                      
M32M1834 DS    CL6       C                                                      
M32M1849 DS    CL6       C         MEN                                          
M32M2149 DS    CL6       C                                                      
M32M2549 DS    CL6       C                                                      
M32M2554 DS    CL6       C                                                      
M32MO50  DS    CL6       C                                                      
*                                                                               
M32WO18  DS    CL6       C                                                      
M32W1224 DS    CL6       C                                                      
M32W1834 DS    CL6       C                                                      
M32W1849 DS    CL6       C         WOMEN                                        
M32W2549 DS    CL6       C                                                      
M32W2554 DS    CL6       C                                                      
M32W2564 DS    CL6       C                                                      
M32WO50  DS    CL6       C                                                      
M32WWORK DS    CL6       C                                                      
*                                  VIEWERS                                      
M32VO2   DS    CL6       C                                                      
M32VO18  DS    CL6       C                                                      
M32V1224 DS    CL6       C                                                      
M32V1234 DS    CL6       C                                                      
M32V1834 DS    CL6       C                                                      
M32V1849 DS    CL6       C                                                      
M32V2149 DS    CL6       C                                                      
M32V2554 DS    CL6       C                                                      
M32VO35  DS    CL6       C                                                      
M32V3564 DS    CL6       C                                                      
M32VO50  DS    CL6       C                                                      
         DS    CL114     C ANY DEMO                                             
         DS    CL12      C JIC                                                  
* MINIMUM IN TABS FOR DAYPART                                                   
M3MITDM  DS    CL3                 METRO                                        
M3MITDD  DS    CL3                 DMA (GROUP 1 AREA)                           
M3MITDN  DS    CL3                 NSI (GROUP 2 AREA)                           
* MINIMUM IN TABS FOR M-F (ALL MULTI-WEEK COMBINATIONS)                         
M3MITMWM DS    CL3                 METRO                                        
M3MITMWD DS    CL3                 DMA (GROUP 1 AREA)                           
M3MITMWN DS    CL3                 NSI (GROUP 2 AREA)                           
* MINIMUM IN TABS FOR M-F (1-WEEK)                                              
M3MIT1WM DS    CL3                 METRO                                        
M3MIT1WD DS    CL3                 DMA (GROUP 1 AREA)                           
M3MIT1WN DS    CL3                 NSI (GROUP 2 AREA)                           
* MINIMUM IN TABS FOR INDIVIDUAL DAY (1WEEK AND ALL WEEK COMBINATIONS)          
M3MIT1DM DS    CL3                 METRO                                        
M3MIT1DD DS    CL3                 DMA (GROUP 1 AREA)                           
M3MIT1DN DS    CL3                 NSI (GROUP 2 AREA)                           
         DS    CL702     C SPARE                                                
* MARKET INFORMATION *4                                                         
M4DSECT  DSECT                                                                  
M4RCODE  DS    CL3       C         RECORD CODE = 004                            
M4MNO    DS    CL6       C         MARKET NUMBER                                
M4ADDREC DS    CL1                 ADDITIONAL RECORD INDICATOR  1-9             
         DS    CL2                 ZERO FILLED                                  
         DS    CL3                 BLANK                                        
         DS    CL5                 ZERO                                         
M4SEQSR  DS    CL4                 STATION REPORT SEQUENCE                      
         DS    CL1                 JIC                                          
M4SCOLD  DS    CL4                 STATION CODE (OLD) 1-9095                    
M4SIDCDE DS    CL10                STATION ID CODE                              
M4SCALL  DS    CL12                STATION CALL LETTERS/GROUP NAME              
M4SGRPI  DS    CL1                 STATION GROUP INDICATOR(+=GRP INCL)          
M4ORIGD  DS    CL6                 DMA OF ORIGIN                                
M4CHAN   DS    CL7                 BROADCAST CHANNEL NUMBER                     
M4STYP   DS    CL2                 STATION TYPE CODE                            
*                                  01=REGULAR,02=PARENT                         
*                                  03=PARENT+SAT/CHILD STN GRP                  
*                                  07=SAT/CHILD STATION IN MKT                  
*                                  09 OUTSIDE STATION                           
M4AFF1   DS    CL7                 PRIMARY AFFILIATION                          
M4AFF2   DS    CL7                 SECONDARY AFFILIATION                        
M4AFF3   DS    CL7                 TERTIARY AFFILIATION                         
M4PSCO   DS    CL4                 PARENT STA CODE(OLD)                         
M4PSID   DS    CL10                PARENT STATION ID CODE                       
M4PCALL  DS    CL12                PARENT STATION CALL/GROUP                    
M4S1PSCO DS    CL4                 SAT 1 STA CODE(OLD)                          
M4S1PSID DS    CL10                SAT 1 STATION ID CODE                        
M4S1PCAL DS    CL12                SAT 1 STATION CALL/GROUP                     
M4S2PSCO DS    CL4                 SAT 2 STA CODE(OLD)                          
M4S2PSID DS    CL10                SAT 2 STATION ID CODE                        
M4S2PCAL DS    CL12                SAT 2 STATION CALL/GROUP                     
M4S3PSCO DS    CL4                 SAT 3 STA CODE(OLD)                          
M4S3PSID DS    CL10                SAT 3 STATION ID CODE                        
M4S3PCAL DS    CL12                SAT 3 STATION CALL/GROUP                     
M4SATTAB DS    (26*17)C            REPEAT SAT FIELDS 17 TIMES                   
M4STNAM  DS    CL40                EXPANDED STATION NAME                        
M4DRIND  DS    C                   DATA RECORD IND (Y OR N)                     
         DS    727C                SPARE                                        
                                                                                
* MARKET INFORMATION *5                                                         
M5DSECT  DSECT                                                                  
M5RCODE  DS    CL3       C         RECORD CODE = 002                            
M5MNO    DS    CL6       C         MARKET NUMBER                                
M5ADDREC DS    CL1                 ADDITIONAL RECORD INDICATOR  1-9             
         DS    CL2                 ZERO FILLED                                  
         DS    CL3                 BLANK                                        
         DS    CL5                 ZERO                                         
         DS    CL41                ZERO                                         
M5A1DEF  DS    CL14                GROUP1 AREA DEFINITION                       
M5A2DEF  DS    CL14                GROUP2 AREA DEFINITION                       
         DS    290C                JIC                                          
M5CUSTOM DS    C                   C=NON-STANDARD DEMO                          
*                                  USE DEMO DEFS BELOW                          
M5A1DNAM DS    CL(9*27)            AREA1 CUSTOM DEMO NAMES                      
M5A2DNAM DS    CL(9*27)            AREA2 CUSTOM DEMO NAMES                      
         DS    CL375               SPARE                                        
* *************REST OF DEFINITION REMAINS TO BE DONE************                
         EJECT                                                                  
*======================== TIME PERIOD RECORD =========================*         
*                                                                               
QHDSECT  DSECT                                                                  
QHRCDE   DS    CL3       C         RECORD CODE 300=FULL SURVEY                  
*                                   3XX XX=FIRST WEEK OF PROGRAM                
QHMNO    DS    CL6       C         MARKET NUMBER                                
QHMRI    DS    CL1       C         MULTIPLE RECORD INDICATOR                    
QHDCDE   DS    CL2       C         DAY SEQUENCE 01=M-F 02-08=DAY                
QHDALF   DS    CL3       C         DAY CODE ALPHA                               
QHQHCDE  DS    CL5       C         HH:MM:A/P MID=1200A                          
QHSSEQ   DS    CL4       C         REPORT SEQ                                   
PAVNF    DS    CL1                 PAV NORMAL/FULL CYCLE INDICATOR              
QHSCDE   DS    CL4       C         STATION CODE(OLD)                            
QHSCDEN  DS    CL10      C         STATION CODE(NEW)                            
QHSCALL  DS    CL12      C         STATION CALL LETTERS/GROUP NAME              
         DS    CL6                 SPARE                                        
QHSGI    DS    CL1       C         STATION GROUP INDICATOR                      
*                                   +=STATION GROUP DEMOS                       
QHCHAN   DS    CL7                 CHANNEL XXX-YYY                              
*                                   XXX=PRIMARY,YYY=SECONDARY                   
QHSTYP   DS    CL2       C         STATION TYPE                                 
*                                   00=HUT/PUT 01=LOCAL                         
*                                   02=PARENT ONLY                              
*                                   05=PARENT+SAT/CHILD                         
*                                   07=SAT/CHILD                                
*                                   09=OUTSIDE                                  
         DS    CL4                                                              
QHSGINAM DS    CL12                PARENT GROUP NAME FOR SATS                   
         DS    CL5       C                                                      
QHPN14   DS    CL14      C         PROGRAM NAME (14)                            
         DS    CL2       C                                                      
QHNOW    DS    CL2       C         NUMBER OF WEEKS IN SURVEY                    
QHDAYT   DS    CL24      C         NUMBER OF TELECAST DAYS BY WEEK              
PRQHTOT  DS    CL4       C         TOTAL QH TELECAST                            
QHPDOM   DS    CL10      C         PROGRAM CODE DOMINANT                        
QHDMATND DS    CL20      C         TREND DMA SHARES OR HUT                      
QHDPTYP  DS    CL8       C         PROGRAM TYPE                                 
         DS    CL2       C                                                      
QHPNI1   DS    CL1       C         1=PROG NAMES DIFFER PAR/SAT/CHILD            
QHPNI2   DS    CL1       C         ALWAYS 0 FOR PAV                             
QHPNI3   DS    CL1       C         ALWAYS 0 FOR PAV                             
QHPNI4   DS    CL1       C         ALWAYS 0 FOR PAV                             
         DS    CL5                                                              
QHPRSRC  DS    CL7       C         PROGRAM SOURCE                               
         DS    CL28                (FOR FUTURE USE)                             
PRDYINAV DS    C         C         NUMBER OF DAYS IN AVERAGE                    
         SPACE 1                                                                
QHMARTG  DS    CL5       P         METRO A HH RTG                               
QHMAHUT  DS    CL5       P         METRO A HUT                                  
QHMATHS  DS    CL5       P         METRO A HOUSEHOLD SHARE                      
QHMATPH  DS    CL9       P         METRO A TOTAL PROJECTED HH (0)               
QHMBRTG  DS    CL5       P         METRO B HH RTG                               
QHMBHUT  DS    CL5       P         METRO B HUT                                  
QHMBTHS  DS    CL5       P         METRO B HOUSEHOLD SHARE                      
QHMBTPH  DS    CL9       P         METRO B TOTAL PROJECTED HH (0)               
QHMCRTG  DS    CL5       C         METRO C HH RTG                               
QHMCHUT  DS    CL5       C         METRO C HUT                                  
QHMCTHS  DS    CL5       C         METRO C HOUSEHOLD SHARE                      
QHMCTPH  DS    CL9       C         METRO C TOTAL PROJECTED HH (0)               
QHDRHH   DS    CL5       C         DMA HH RTG                                   
QHDHUT   DS    CL5       C         DMA HUT                                      
QHDRHS   DS    CL5       C         DMA HH SHARE                                 
QHDTPH   DS    CL9       C         DMA TOTAL PROJECTED HH (0)                   
QHDWRTG  DS    CL(12*5)  C         WEEKLY RATINGS                               
         DS    CL5                                                              
QHA1EST  DS    CL(27*9)  C         AREA1 DEMOS IN IMPRESSIONS(DMA)              
QHA2HG   DS    CL9                 TSA HOMES                                    
QHA2EST  DS    CL(27*9)  C         AREA2 DEMOS IN IMPRESSIONS(TSA)              
PRQHSN   DS    CL(8*12)  C         START QH/NUM QH BY WEEK                      
QHAPPUT  DS    CL(9*27)            N/A RESERVED FOR FUTURE PROGRAM PUTS         
********PAV ARRAY DSECTS                                                        
PRQHSND  DSECT                                                                  
PRQHST   DS    CL5                 START QH BY WEEK                             
PRQHNUM  DS    CL3                 NUMBER OF QH AVERAGED BY WEEK                
***********************************************************************         
         EJECT                                                                  
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'097DEPN0110  03/21/14'                                      
         END                                                                    
