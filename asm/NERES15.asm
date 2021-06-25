*          DATA SET NERES15    AT LEVEL 157 AS OF 05/01/02                      
*PHASE T32115A,*                                                                
*INCLUDE DLFLD                                                                  
T32115   TITLE '-   OVERNIGHT RESEARCH PRINTING MODULE'                         
T32115   CSECT                                                                  
MAXFLEN  EQU   25                  MAXIMUM FIELD LENGTH                         
MAXFLN1  EQU   MAXFLEN-1           MAXIMUM FIELD LENGTH -1                      
         PRINT NOGEN                                                            
         NMOD1 0,**REPR**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R6,BUFF                                                          
         USING RESD,R6                                                          
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         SPACE 1                                                                
         LA    R1,RELOC                                                         
         S     R1,RELOC                                                         
         ST    R1,RELO                                                          
         SPACE 1                                                                
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         SPACE 1                                                                
****     MVC   RESTITLE,=CL40'NETWORK RESEARCH REPORT'                          
         L     R2,=A(NETTITLE)                                                  
         MVC   RESTITLE,0(R2)                                                   
         CLC   DBFILE,=C'NTI'      SPECIAL NAD/NTI OPTIONS                      
         BE    *+10                                                             
         CLC   DBFILE,=C'NAD'                                                   
         BNE   REL                                                              
         MVI   DBSELDUR,0                                                       
         CLI   FFTNOPT,C'Y'        SET DURATION OPTIONS                         
         BNE   *+8                                                              
         MVI   DBSELDUR,X'FF'      INCLUDE '15'                                 
         CLI   FFTNOPT,C'O'        ONLY '15'                                    
         BNE   *+8                                                              
         MVI   DBSELDUR,X'FE'                                                   
         CLC   DBFILE,=C'NTI'                                                   
         BNE   REL                                                              
         CLI   TPOPT,C'Y'          OPTION TO USE TP FROM NTI                    
         BNE   *+8                                                              
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBDAYOPT,DAYOPT     P (POCKETPIECE) OR I (INDIVIDUAL)            
         SPACE 1                                                                
REL      L     R2,=A(VTYPES)                                                    
         LA    R3,AHOOK                                                         
         SPACE 1                                                                
REL2     CLI   0(R2),X'FF'                                                      
         BE    DOPT2                                                            
         L     R1,0(R2)                                                         
         A     R1,RELO                                                          
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         B     REL2                                                             
         SPACE 1                                                                
DOPT2    MVC   SAVDBLOK,DBLOCK     CHECK FOR SPECIAL OPTION                     
         MVI   SHARCALC,C'N'       TO CALCULATE SHARES                          
         MVI   DBFUNCT,DBGETCTL                                                 
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         TM    DBCSHR,DBOTOSHR     TEST COMPUTE SHARES                          
         BZ    *+8                                                              
         MVI   SHARCALC,C'Y'       INDICATE SHARE COMP REQUIRED                 
         MVC   DBLOCK,SAVDBLOK    RESTORE DEMO LOOK-UP FUNCTION                 
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 2                                                                
IN2      XC    DMCB(12),DMCB       PICK UP A(SPDEMUP)                           
*        MVC   DMCB+4(4),=X'D9000A22'                                           
*        GOTO1 CALLOV,DMCB                                                      
*        MVC   SPDEMUP,DMCB                                                     
*        XC    DMCB(12),DMCB       AND A(SIDIO)                                 
*        MVC   DMCB+4(4),=X'D9000A34'                                           
*        GOTO1 CALLOV,DMCB                                                      
*        MVC   SIDIO,DMCB                                                       
IN3      ZIC   R1,WDETS                                                         
         ZIC   R0,NUMDEMS          FIGURE OUT WIDTH                             
         LR    RF,R0                                                            
         SLL   R0,3                                                             
         AR    R0,RF               MULTIPLY BY 9 POSITION COLUMN                
         AR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         LA    R2,132                                                           
         SR    R2,R1                                                            
         BNM   IN4                                                              
         CLI   DOWNOPT,C'D'        TEST DOWNLOAD - ALLOW OVER 132               
         BE    IN5                                                              
         ZIC   R0,NUMDEMS          WONT FIT INTO 132 - TRIM DEMOS               
         BCTR  R0,0                                                             
         STC   R0,NUMDEMS                                                       
         B     IN3                                                              
         SPACE 1                                                                
IN4      BP    IN5                                                              
         LA    R2,2                                                             
IN5      SRL   R2,1                                                             
         ST    R2,DISP             RECORD DISPLACEMENTS                         
         ZIC   R3,WDETS                                                         
         AR    R2,R3                                                            
         ST    R2,DEMDISP                                                       
         SPACE 1                                                                
         ZIC   R1,NDETS            FIGURE SORT KEY LENGTH                       
*        SLL   R1,4                                                             
         LA    RF,MAXFLEN                                                       
         MR    R0,RF                                                            
         ST    R1,LSORTKEY                                                      
         LA    R2,SORTCARD+15                                                   
         EDIT  (R1),(3,(R2)),FILL=0                                             
         ZIC   R3,NUMDEMS          NUMBER OF COLUMNS                            
         SLL   R3,1                2 COLUMNS FOR EACH DEMO                      
         LA    R3,8(R3)            N'DEMS +8                                    
         ST    R3,NUMCOLS                                                       
         SLL   R3,2                TIMES 4                                      
         AR    R1,R3               PLUS KEY = LENGTH                            
         ST    R1,LSORT                                                         
***      LA    R2,RECCARD+21                                                    
         LA    R2,SORTCARD                                                      
         LA    R2,101(R2)                                                       
         EDIT  (R1),(3,(R2)),FILL=0                                             
         LA    R1,4(R1)            SOUT IS VARIABLE                             
         SLL   R1,16               SO GENERATE A HEADER                         
         ST    R1,SOUTHEAD                                                      
         LA    R1,SORTREC+MAXFLEN*10      SAVE START OF RECORD                  
         S     R1,LSORTKEY                                                      
         ST    R1,RECSTART                                                      
         SPACE 1                                                                
         CLI   DOWNOPT,C'D'        TEST DOWNLOAD                                
         BE    IN12                                                             
         MVC   HEADHOOK,AHOOK                                                   
         SPACE 1                                                                
IN12     CLI   RESSRCE,C'P'        UNIVERSES FOR PROGRAMS                       
         BNE   IN20                                                             
         LA    R2,BLOCK                                                         
         USING GUVD,R2             FILL BLOCK FOR GETNUN                        
         XC    GUVBLOCK,GUVBLOCK                                                
         MVC   GUVDATE,PSTART                                                   
         XC    PUEL,PUEL                                                        
         MVI   PUEL,X'31'                                                       
         MVI   PUEL+1,179                                                       
         MVI   PUEL+2,X'44'                                                     
         LA    R1,PUEL+3                                                        
         ST    R1,GUVAOUT                                                       
         MVI   GUVTYPE,2           (HUNDREDS)                                   
         MVC   GUVAREC,AIO                                                      
         MVC   GUVCMFCS,ACOMFACS                                                
         GOTO1 GETNUN,DMCB,GUVBLOCK                                             
         CLI   GUVERROR,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         OC    UNIVOPT,UNIVOPT                                                  
         BZ    IN16                OPTION TO USE SPECIAL UNIVERSE               
         MVC   GUVAGY,AGENCY                                                    
         L     R1,UNIVOPT          NEED CODE IN PWOS                            
         CVD   R1,DUB                                                           
         L     R1,DUB+4                                                         
         SRL   R1,4                                                             
         STH   R1,DUB                                                           
         MVC   GUVCODE,DUB                                                      
         XC    GUVDATE,GUVDATE                                                  
         GOTO1 GETNUN,DMCB,GUVBLOCK                                             
         CLI   GUVERROR,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
IN16     OC    HOMEOPT,HOMEOPT     OPTION TO MODIFY UNIVERSES                   
         BZ    IN20                BASED ON SUPPLIED HOMES FIGURE               
         MVC   DUB(4),PUEL+87      PICK OUT HOMES FIGURE                        
         L     R2,DUB              INTO R2                                      
         L     R3,HOMEOPT                                                       
         MH    R3,=H'10'           GET R3 TO HUNDREDS AS WELL                   
         LA    R4,PUEL+3                                                        
****     LA    R5,24               (WAS 24 - EXPANDED TO 41)                    
         ZIC   R5,PUEL+1           R5 - DEVELOP NUMBER OF ELEMENTS              
         SRL   R5,2                     BY DIVIDING BY 4                        
         SPACE 1                                                                
IN18     L     R1,0(R4)                                                         
         MR    R0,R3                                                            
         DR    R0,R2                                                            
         ST    R1,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,IN18                                                          
         SPACE 1                                                                
*IN20     GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                  
IN20     LA    R2,SORTCARD                                                      
         LA    R2,80(R2)                                                        
         GOTO1 SORTER,DMCB,SORTCARD,(R2),0                                      
         SPACE 1                                                                
         L     RE,APRGBUFF         CLEAR PROGRAM BUFFER                         
         L     RF,=F'75000'                                                     
         XCEF                                                                   
         SPACE 1                                                                
         MVC   ACTNDEM,NUMDEMS     SAVE ACTUAL DEMOS                            
         MVC   AVDEMOS,DEMOS                                                    
         ZIC   R1,NUMDEMS          SET FF AT END OF DEM LIST                    
         MH    R1,=H'3'                                                         
         LA    R1,AVDEMOS(R1)                                                   
         MVI   0(R1),X'FF'                                                      
         EJECT                                                                  
*              GENERATE FULL RANGE OF DEMOS                                     
         SPACE 3                                                                
         ZIC   R0,NUMDEMS                                                       
         SLL   R0,1                                                             
         STC   R0,NEXDEMS          (NUMBER OF EXTRACT DEMOS)                    
         SRL   R0,1                                                             
         LA    R2,AVDEMOS                                                       
         LA    R3,EXDEMOS                                                       
         SPACE 1                                                                
EX2      MVC   0(3,R3),0(R2)       ASSUME NORMAL (NO SECOND DEMO)               
         MVC   3(3,R3),=X'00C9FF'                                               
         CLI   1(R2),C'V'          CHECK FOR SPECIALS                           
         BE    EX4                                                              
         CLI   1(R2),C'S'                                                       
         BE    EX6                                                              
         B     EXEND                                                            
         SPACE 1                                                                
EX4      CLC   RESSRCE(3),=C'NAD'  UNLESS THIS IS NAD                           
         BE    EXEND                                                            
         MVI   1(R3),C'T'          VPH BECOMES DEMO IMPS                        
         MVC   3(3,R3),=X'00E301'  / HOMES IMPRESSIONS                          
         B     EXEND                                                            
         SPACE 1                                                                
EX6      CLI   SHARCALC,C'Y'       IF OPTION IS SET ON                          
         BNE   EXEND                                                            
         MVI   1(R3),C'R'          SHARE BECOMES RATINGS                        
         MVI   4(R3),C'P'          / PUTS                                       
         MVC   5(1,R3),2(R3)                                                    
         SPACE 1                                                                
EXEND    LA    R2,3(R2)                                                         
         LA    R3,6(R3)                                                         
         BCT   R0,EX2                                                           
         MVI   0(R3),X'FF'                                                      
         EJECT                                                                  
*              CONTROL THE INPUT ROUTINES                                       
         SPACE 3                                                                
         LA    R2,NETSAVE                                                       
         ZIC   R3,NUMNETS                                                       
         SPACE 1                                                                
MULTNET  MVC   DBSELSTA,0(R2)      SUPPORT MULTIPLE NETWORKS                    
*                                                                               
         LA    RE,NETSAVE                                                       
         ZIC   RF,NUMNETS                                                       
MULTNET1 CLC   0(5,RE),=C'ZZZ C'   CHECK FOR ALL CABLE                          
         BE    *+10                 OR                                          
         CLC   0(5,RE),=C'ZZZ M'     ALL NAD SYND                               
         BE    *+10                 OR                                          
         CLC   0(5,RE),=C'ZZZ S'     ALL SYND                                   
         BE    *+16                                                             
         LA    RE,5(RE)                                                         
         BCT   RF,MULTNET1                                                      
         B     MULTNET2                                                         
         SPACE 2                                                                
*                                  MERGE THEM IN WITH REGS                      
         L     RF,=V(GETALL)                                                    
         CLI   4(RE),C'M'          NAD SYNDICATION                              
         BNE   *+8                                                              
         L     RF,=V(GETALN)                                                    
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(RC)                                                   
         L     R3,DMCB             AND POINT TO DIFFERENT LIST                  
         L     R2,DMCB+4                                                        
         MVC   DBSELSTA,0(R2)                                                   
*                                                                               
MULTNET2 GOTO1 VADJSEL             ADJUST FOR PEOPLE METERS                     
         GOTO1 VDISPNET            GET DISPLAYABLE VERSION IN WORK              
         MVC   ACTNET(5),0(R2)                                                  
         CLI   RESSRCE,C'P'        PROGRAM NEEDS 7 BYTES                        
         BNE   *+10                                                             
         MVC   ACTNET(7),0(R2)                                                  
         MVC   SORTNET(4),WORK                                                  
         CLI   SURVOPT,0           IF REQUESTED                                 
         BE    *+10                                                             
         MVC   DBBTYPE,SURVOPT     SPECIAL SURVEYS                              
         BAS   RE,ONENET                                                        
         LA    R2,5(R2)                                                         
         CLI   RESSRCE,C'P'                                                     
         BNE   *+12                REG NETS A DONE ELSEWHERE                    
         LA    R2,2(R2)            (2 MORE BYTES FOR PROGRAMS)                  
         BCT   R3,MULTNET                                                       
*                                                                               
         CLI   DOWNOPT,C'D'        TEST DOWNLOAD                                
         BNE   MULTNET8                                                         
         GOTO1 =A(DOWN),DMCB,0     0=START OF REPORT                            
         CLI   ANYSORT,C'N'                                                     
         BNE   MULTNET6                                                         
         GOTO1 =A(DOWN),DMCB,4     4='NO DATA' MESSAGE                          
         B     MULTNET8                                                         
MULTNET6 MVI   FORCEHED,C'Y'                                                    
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         L     RF,AHOOK            ONE-TIME-ONLY USE OF HEAD HOOK               
         BASR  RE,RF                                                            
         XC    AVETOTAD,AVETOTAD   SET AVE/TOT ADDRESS TO ZERO                  
*                                                                               
MULTNET8 BAS   RE,GET              THEN DO THE REPORTS                          
*                                                                               
         CLI   DOWNOPT,C'D'        TEST DOWNLOAD                                
         BNE   XIT                                                              
         GOTO1 =A(DOWN),DMCB,5     5=END OF REPORT                              
         B     XIT                                                              
         SPACE 2                                                                
ONENET   NTR1                                                                   
         MVI   ROWSW,C'N'                                                       
         CLI   RESSRCE,C'P'        PROGRAMS                                     
         BNE   ONENET2                                                          
         BAS   RE,PROGS                                                         
         B     XIT                                                              
         SPACE 1                                                                
ONENET2  BAS   RE,MULTBOOK         OR OTHERS                                    
         B     XIT                                                              
         EJECT                                                                  
*              SUPPORT MULTIPLE BOOKS                                           
         SPACE 3                                                                
MULTBOOK NTR1                                                                   
         STM   R2,R3,NETPTR                                                     
         STM   R2,R3,SVNETPTR                                                   
         MVC   WORKBOOK,BOOK+1                                                  
         LA    R2,BOOKS                                                         
         LA    R3,1                REQUEST BOOK SEQUENCE                        
         LA    R0,12                                                            
         SPACE 1                                                                
MB6      MVC   SORTBOOK,WORKBOOK                                                
         MVC   DBSELBK,WORKBOOK                                                 
         STC   R3,SORTWKSQ                                                      
         STC   R3,SORTRQBK                                                      
         MVC   SORTRQBK+1(1),0(R2) CARRY TYPE OF BOOK                           
         MVC   THISBOOK,0(R2)                                                   
         CLC   RESSRCE(3),=C'NTI'                                               
         BNE   MB6B                                                             
         CLI   SEPOPT,C'N'         OPTION TO SUPPRESS                           
         BNE   MB6A1                                                            
         CLI   WORKBOOK+1,35       WEEKS 35                                     
         BE    MB7                                                              
         CLI   WORKBOOK+1,36       AND 36                                       
         BE    MB7                                                              
         SPACE 1                                                                
MB6A1    CLI   NOVOPT,C'N'         OPTION TO SUPPRESS                           
         BNE   MB6A2                                                            
         CLI   WORKBOOK+1,41       WEEKS 41                                     
         BE    MB7                                                              
         CLI   WORKBOOK+1,42       AND 42                                       
         BE    MB7                                                              
         SPACE 1                                                                
MB6A2    MVI   ROWSW,C'N'                                                       
         GOTO1 NETBROWN,DMCB,(WORKBOOK+1,DUB),(WORKBOOK,0)                      
         OC    DUB(4),DUB          CHECK IF THIS IS A ROW                       
         BZ    MB6B                                                             
         CLI   ROWOPT,C'N'         OPTION TO EXCLUDE THESE                      
         BE    MB7                                                              
         CLI   DBBTYPE,C'A'        OR ASCRIBED                                  
         BE    MB6B                                                             
         CLI   DBBTYPE,C'I'        OR INTEGRATED                                
         BE    MB6B                                                             
         CLI   DBBTYPE,C'C'        NO ROW FOR CONFORMED                         
         BE    MB6B                                                             
         CLI   DBBTYPE,C'Z'        NO ROW FOR Z-BOOK (TEST)                     
         BE    MB6B                                                             
         MVI   ROWSW,C'Y'                                                       
         SPACE 1                                                                
MB6B     BAS   RE,MULTDT           HANDLE ONE WEEK                              
         LM    RE,RF,NETPTR        FOR ALL STATIONS                             
         BCT   RF,*+12                                                          
         LM    RE,RF,SVNETPTR                                                   
         B     *+8                                                              
         LA    RE,5(RE)                                                         
         STM   RE,RF,NETPTR                                                     
         MVC   DBSELSTA,0(RE)                                                   
         MVC   DROPJUNK(5),0(RE)                                                
         GOTO1 VADJSEL                                                          
         GOTO1 VDISPNET                                                         
         MVC   SORTNET(4),WORK                                                  
         MVC   ACTNET(5),DROPJUNK                                               
         CLC   NETPTR(8),SVNETPTR                                               
         BNE   MB6                                                              
         SPACE 1                                                                
MB7      TM    0(R2),X'01'         OPTION TO HANDLE A RANGE                     
         BNO   MB10                                                             
         CLC   WORKBOOK,4(R2)                                                   
         BNE   MB8                                                              
         LA    R2,3(R2)                                                         
         BCTR  R0,0                                                             
         B     MB10                                                             
         SPACE 1                                                                
MB8      BAS   RE,BUMPWEEK         BUMP TO NEXT WEEK                            
         LA    R3,1(R3)                                                         
         B     MB6                                                              
         SPACE 1                                                                
MB10     LA    R2,3(R2)                                                         
         OC    0(3,R2),0(R2)                                                    
         BZ    XIT                                                              
         MVC   WORKBOOK,1(R2)                                                   
         LA    R3,1(R3)                                                         
         BCT   R0,MB6                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FIGURE OUT NEXT NTI WEEK NO.                          
         SPACE 3                                                                
BUMPWEEK NTR1                                                                   
         L     R1,=A(BUMPLIST)                                                  
         SPACE 1                                                                
BUMPW2   CLC   0(1,R1),WORKBOOK+1  LOOK UP WEEK IN LIST                         
         BE    BUMPW4                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   BUMPW2                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
BUMPW4   MVC   WORKBOOK+1(1),1(R1)                                              
         CLI   WORKBOOK+1,1        HAVE WE STARTED A NEW YEAR                   
         BNE   BUMPW5                                                           
         ZIC   RF,WORKBOOK         THEN BUMP THE YEAR                           
         LA    RF,1(RF)                                                         
         STC   RF,WORKBOOK                                                      
         B     BUMPW6                                                           
         SPACE 1                                                                
BUMPW5   CLI   WORKBOOK+1,53       WEEK 53 APPLIES TO 1984 ONLY                 
         BNE   BUMPW6                                                           
         CLI   WORKBOOK,84         1984                                         
         BE    BUMPW6                                                           
         CLI   WORKBOOK,89         OR 1989                                      
         BE    BUMPW6                                                           
         LA    R1,1(R1)                                                         
         MVC   WORKBOOK+1(1),1(R1) BYPASS WEEK 53 FOR ALL OTHER YEARS           
         SPACE 1                                                                
BUMPW6   CLI   ALTOPT,C'Y'         ALTERNATING OPTION                           
         BNE   XIT                                                              
         CLC   WORKBOOK,4(R2)      MAKE SURE WE DON'T GO PAST END               
         BE    XIT                                                              
         MVC   WORKBOOK+1(1),2(R1)                                              
         CLI   WORKBOOK+1,1        HAVE WE STARTED A NEW YEAR                   
         BNE   XIT                                                              
         ZIC   RF,WORKBOOK         THEN BUMP THE YEAR                           
         LA    RF,1(RF)                                                         
         STC   RF,WORKBOOK                                                      
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL MULTIPLE DAY/TIMES AND DAYPART LISTS                     
         SPACE 3                                                                
MULTDT   NTR1                                                                   
         LA    R2,DAYTMLST         SUPPORT MULTIPLE DAY/TIME                    
         ZIC   R3,DAYTIMES                                                      
         SPACE 1                                                                
MDT14    MVC   DBSELDAY(5),0(R2)                                                
         MVI   DBBEST,0                                                         
         MVC   SORTDP,DBSELDAY     SAVE START-END TIME                          
         CLI   DBSELDAY,X'FF'      UNLESS ALL SELECTED                          
         BE    MDT14B                                                           
         ZIC   R1,DBSELDAY         ADJUST TO SPOT-TYPE DAY                      
         LA    R1,SPOTDAYS(R1)                                                  
         MVC   DBSELDAY(1),0(R1)                                                
         CLI   DBSELDAY,X'7C'      ASK FOR EXACT MATCH FOR M-F                  
         BE    *+8                                                              
         CLI   DBSELDAY,X'7F'      AND FOR M-S                                  
         BNE   *+8                                                              
         MVI   DBBEST,C'L'                                                      
         B     MDT15                                                            
         SPACE 1                                                                
MDT14B   CLC   DBFILE,=C'PAV'      FOR PAV FF=ALL                               
         BE    MDT15                                                            
         MVI   DBSELDAY,X'7F'      OTHERS LIKE 7F (M-S)                         
         SPACE 1                                                                
MDT15    CLI   DBSELTIM,X'FF'                                                   
         BE    MDT15A                                                           
         OC    DBSELTIM(2),DBSELTIM                                             
         BNZ   MDT15B                                                           
MDT15A   MVC   DBSELTIM(2),=H'600' SET ALL TIME TO 6-545                        
         MVC   DBSELTIM+2(2),=H'545'                                            
         CLC   RESSRCE(3),=C'NTI'  FOR NETWORK                                  
         BE    MDT15B                                                           
         CLC   RESSRCE(3),=C'NAD'                                               
         BE    MDT15B                                                           
         MVC   DBSELTIM+2(2),=H'200'   6-2AM FOR OTHERS                         
         SPACE 1                                                                
MDT15B   BAS   RE,ONEDT                                                         
         LA    R2,5(R2)                                                         
         BCT   R3,MDT14                                                         
         B     XIT                                                              
         EJECT                                                                  
*              NOW HANDLE READING FOR SPECIFIC NETWORK/BOOK/DAY/TIME            
         SPACE 3                                                                
ONEDT    NTR1                                                                   
*        MVI   DBFULTRK,C'Y'                                                    
         L     RE,=V(TRKBUF)                                                    
         ST    RE,DBAOPT                                                        
         MVC   DBLOPT,=F'53000'                                                 
         GOTO1 DEMAND,DMCB,DBLOCK,ONEHOOK                                       
         B     XIT                                                              
         SPACE 1                                                                
ONEHOOK  NTR1                      HOOK FOR DEMAND                              
         CLI   ANYUP,C'Y'          DON'T DO UPGRADES HERE                       
         BE    XIT                                                              
         CLC   DBFILE,=C'EVN'                                                   
         BE    ONEHOOK4                                                         
         GOTO1 DEFINE,DMCB,=C'PROGRAM',DBLOCK,SORTPROG                          
         GOTO1 (RF),(R1),=C'EPISODE',DBLOCK,SORTEPIS                            
         GOTO1 DEFINE,DMCB,=C'PROG25',DBLOCK,SORTLPRO                           
         GOTO1 (RF),(R1),=C'EPIS25',DBLOCK,SORTLEPI                             
         GOTO1 (RF),(R1),=C'DAY',DBLOCK,BLOCK                                   
         MVC   SORTDAY,BLOCK+1                                                  
         GOTO1 (RF),(R1),=C'TIME'                                               
         MVC   SORTTIME,BLOCK+2                                                 
         BAS   RE,FIGMINS                                                       
         CLI   DPOPT,C'Y'          OPTION TO USE DAYPART DAY/TIME               
         BNE   *+16                                                             
         MVC   SORTDAY,SORTDP                                                   
         MVC   SORTTIME,SORTDP+1                                                
         GOTO1 (RF),(R1),=C'PURE'                                               
         MVC   SORTCODE,BLOCK+3                                                 
         GOTO1 (RF),(R1),=C'TYPE'                                               
         MVC   SORTFILT,BLOCK                                                   
         GOTO1 (RF),(R1),=C'WEEK'                                               
         XC    SORTWEEK,SORTWEEK                                                
         MVC   SORTWEEK(4),BLOCK+1                                              
         CLC   DBFILE,=C'NTI'                                                   
         BE    ONEHOOK2                                                         
         CLC   DBFILE,=C'NAD'                                                   
         BNE   ONEHOOK4                                                         
         SPACE 1                                                                
ONEHOOK2 GOTO1 (RF),(R1),=C'NTI '                                               
         MVC   SORTNTI,BLOCK                                                    
         SPACE 1                                                                
ONEHOOK4 XC    DUB,DUB             CHECK FOR ANY FILTERS                        
         MVC   DUB(4),SORTFILT                                                  
         GOTO1 VCHEFILT,DMCB,DUB                                                
         BNE   XIT                                                              
         BAS   RE,DEMPUT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              HANDLE PROGRAM RECORDS FOR NETWORK                               
         SPACE 3                                                                
PROGS    NTR1                                                                   
         MVI   LASTHDAY,X'FF'                                                   
         LA    R4,KEY              BUILD KEY                                    
         USING NPGRECD,R4                                                       
         MVC   DATADISP,=H'24'                                                  
         XC    NPGKEY,NPGKEY                                                    
         MVC   NPGKTYP,=X'0DA0'                                                 
         MVC   NPGKAM,BINAGYMD                                                  
         MVC   NPGKNET,ACTNET+5                                                 
         GOTO1 HIGH                                                             
         B     PRG6                                                             
         SPACE 1                                                                
PRG4     GOTO1 SEQ                                                              
         SPACE 1                                                                
PRG6     CLC   KEY(5),KEYSAVE      CHECK NETWORK C/B                            
         BNE   XIT                                                              
         CLC   NPGKEND,PSTART      DATE FILTERS                                 
         BL    PRG4                                                             
         CLC   NPGKEND,PEND                                                     
         BH    PRG4                                                             
         GOTO1 DATCON,DMCB,(2,NPGKEND),(0,SORTDATE)                             
         GOTO1 (RF),(R1),(2,NPGKEND),(3,WORK)                                   
         MVC   SORTMNTH,WORK                                                    
         SPACE 1                                                                
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         CLI   DPTOPT,0            DAYPART OPTION                               
         BE    PRG7                                                             
         LA    R5,IO                                                            
         MVI   ELCODE,X'93'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRG4                                                             
         USING NPG2ELEM,R5                                                      
         CLC   DPTOPT,NPG2DYP                                                   
         BNE   PRG4                                                             
         DROP  R5                                                               
PRG7     LA    R5,IO                                                            
         LA    R4,IO                                                            
         MVC   SORTCODE,NPGKPROG                                                
         LA    R4,KEY                                                           
         MVI   ELCODE,X'5D'        NEED A BOOK ELEMENT                          
         BAS   RE,GETEL                                                         
         MVC   PBEL,0(R5)                                                       
         LA    R5,IO                                                            
         MVI   ELCODE,X'92'                                                     
         BAS   RE,GETEL                                                         
         USING NPGELEM,R5                                                       
         MVC   DAYNUM,NPGRDAY                                                   
         SPACE 1                                                                
         OC    DAYTMLST,DAYTMLST   DAY/TIME FILTERS                             
         BZ    PRG10                                                            
         LA    R2,DAYTMLST         MUST MATCH ON ONE IN LIST                    
         ZIC   R3,DAYTIMES                                                      
         SPACE 1                                                                
PRG8     CLI   0(R2),X'FF'         FOR DAY - ALL                                
         BE    PRG10                                                            
         CLC   0(1,R2),NPGRDAY               OR MATCH                           
         BNE   PRG12                                                            
         SPACE 1                                                                
PRG10    CLI   1(R2),X'FF'         AND TIME - ALL                               
         BE    PRG14                                                            
         MVC   WORK(4),NPGTIME                OR FIT                            
         MVC   WORK+4(4),1(R2)                                                  
         LA    R1,WORK                                                          
         LA    R0,4                                                             
         SPACE 1                                                                
PRG11    LH    RE,0(R1)            CONVERT 0-6AM TO 2400-3000                   
         BZ    PRG11B                                                           
         CH    RE,=H'600'                                                       
         BNL   PRG11B                                                           
         LA    RE,2400(RE)                                                      
         STH   RE,0(R1)                                                         
         SPACE 1                                                                
PRG11B   LA    R1,2(R1)                                                         
         BCT   R0,PRG11                                                         
         CLC   WORK(2),WORK+4      CHECK START NOT BEFORE REQ START             
         BL    PRG12                                                            
         OC    WORK+2(2),WORK+2    WAS END TIME SPECIFIED                       
         BNZ   *+10                                                             
         MVC   WORK+2(2),WORK      END=START                                    
         CLC   WORK+2(2),WORK+6    TEST END NOT PAST REQUEST END                
         BNH   PRG14                                                            
         SPACE 1                                                                
PRG12    LA    R2,5(R2)                                                         
         BCT   R3,PRG8                                                          
         B     PRG4                                                             
         SPACE 1                                                                
PRG14    XC    DUB,DUB             CHECK FILTERS                                
         MVC   DUB(4),NPGFILT                                                   
         GOTO1 VCHEFILT,DMCB,DUB                                                
         BNE   PRG4                                                             
         EJECT                                                                  
*              BUILD PHONEY EVN RECORD AND EXTRACT DATA                         
         SPACE 3                                                                
         MVC   SORTDAY,NPGRDAY                                                  
         MVC   SORTTIME,NPGTIME                                                 
         MVC   SORTPROG,NPGNAME                                                 
         OC    SORTPROG,SPACES                                                  
         MVC   SORTBOOK,BOOKS+1                                                 
         MVC   SORTFILT,NPGFILT                                                 
         MVI   SORTFILT+3,C' '     (ONLY 3 FILTERS FOR PROGS)                   
         EDIT  (2,NPGPPNO),(5,SORTNTI),FILL=0                                   
         SPACE 1                                                                
         MVC   HUTTIME,NPGKTIME                                                 
         MVC   HUTSCHEM,SCHEMOPT   PASS OPTIONS                                 
         MVC   HUT52,HUT52OPT                                                   
*        MVC   HUTTYPE,SORTNET+3   HUT - USE NETWORK'S BOOK TYPE                
*        CLI   TYPEOPT,0                 OR USE TYPE = OVERRIDE                 
*        BE    *+10                                                             
         CLI   HUTTYPE,0                                                        
***      BNE   *+8                                                              
         B     *+8                 ***TEMP***                                   
         MVI   HUTTYPE,C'A'        FORCE ASCRIBED AS DEFAULT                    
         MVC   HUTTYPE,TYPEOPT                                                  
         GOTO1 VPROGHUT                                                         
         OC    HUTOVER,HUTOVER     ANY HUT OVERRIDES                            
         BZ    EVN1                                                             
         LH    R0,HUTOVER          YES - SO APPLY PERCENT                       
         LH    R1,HUT                                                           
         MR    R0,R0                                                            
         D     R0,=F'5000'                                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STH   R1,HUT                                                           
         SPACE 1                                                                
EVN1     XC    PVEL,PVEL                                                        
         MVI   PVEL+0,X'33'                                                     
         MVI   PVEL+1,119                                                       
         MVI   PVEL+2,1            1-BYTE PER VPH                               
         MVC   PVEL+3(34),NPGVPHS                                               
         ZIC   R1,1(R5)                                                         
         AR    R1,R5                                                            
         CLI   0(R1),X'93'         TEST IF NEW PROGRAM ELEMENT FOUND            
         BNE   EVN5                                                             
         USING NPG2ELEM,R1                                                      
         MVI   PVEL+2,X'42'        PRECISION AND 2-BYTES PER VPH                
         MVC   PVEL+3(116),NPG2VPHS                                             
         DROP  R1                                                               
*                                                                               
EVN5     MVI   PIOEOR,0                                                         
         XC    PREL,PREL                                                        
         MVC   PREL(3),=X'350902'  GENERATE RATINGS ELEMENT                     
         MVC   PREL+3(2),NPGSHARE                                               
         TM    NPGSTAT,X'80'                                                    
         BO    EVN9                                                             
         LH    R1,NPGSHARE         COMPUTE RTG=HUT X SHR                        
         MH    R1,HUT                                                           
         SR    R0,R0                                                            
         D     R0,=F'500'                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STH   R1,DUB                                                           
         MVC   PREL+3(2),DUB                                                    
         MVC   PREL+5(2),HUT                                                    
         MVC   PREL+7(2),NPGSHARE                                               
         SPACE 2                                                                
EVN9     BAS   RE,DEMPUT                                                        
         B     PRG4                                                             
         EJECT                                                                  
*              ROUTINE TO EXTRACT DEMOS                                         
         SPACE 3                                                                
DEMPUT   NTR1                                                                   
         XC    SORTDEMS,SORTDEMS                                                
         XC    LASTDEMS,LASTDEMS                                                
         BAS   RE,FIGLEN           WORK OUT LENGTH IN 1/4 HOURS                 
         MVC   SORTCNT,=F'1'                                                    
         MVC   SORTWT,SORTQHRS     USE THIS AS WEIGHT                           
         L     RE,SORTWT           ADJUST FOR MULTI-DAYS                        
         CLI   SORTDAY,0           M-F                                          
         BNE   *+8                                                              
         MH    RE,=H'5'                                                         
         CLI   SORTDAY,8           M-S                                          
         BNE   *+8                                                              
         MH    RE,=H'7'                                                         
         ST    RE,SORTWT                                                        
         MVC   SORTDCNT(8),SORTCNT                                              
         CLI   ROWSW,C'N'          IF ROW, DON'T CARRY COUNT & WEIGHT           
         BE    DEMPUT2                                                          
         XC    SORTDCNT(8),SORTDCNT                                             
         SPACE 1                                                                
DEMPUT2  GOTO1 DEMOUT,DMCB,(C'L',EXDEMOS),DBLOCK,SORTDEMS                       
         ZIC   R1,NEXDEMS          REMOVE X'FF'                                 
         SLL   R1,2                                                             
         LA    R1,SORTDEMS(R1)                                                  
         MVI   0(R1),X'00'                                                      
         LA    R2,EXDEMOS                                                       
         LA    R3,SORTDEMS                                                      
         ZIC   R4,NEXDEMS                                                       
         SPACE 1                                                                
DEMPUT4  L     R1,0(R3)                                                         
         LA    RE,SORTDEMS         SAVE UNWEIGHTED VALUES FOR TOTS              
         LR    R0,RE                                                            
         LR    RE,R3                                                            
         SR    RE,R0                                                            
         LA    RE,LASTDEMS(RE)                                                  
         ST    R1,0(RE)                                                         
         ICM   R0,15,SORTWT                                                     
         MR    R0,R0               WEIGHT BY COUNT                              
         ST    R1,0(R3)                                                         
         LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,DEMPUT4                                                       
         SPACE 1                                                                
         LA    R3,SORTDEMS                                                      
         ZIC   R4,NEXDEMS                                                       
         SRL   R4,1                                                             
         SPACE 1                                                                
DEMPUT6  OC    0(4,R3),0(R3)       IF FIRST OPERAND IS ZERO                     
         BNZ   *+10                                                             
         XC    4(4,R3),4(R3)       MAKE THE SECOND ZERO                         
         LA    R3,8(R3)                                                         
         BCT   R4,DEMPUT6                                                       
         EJECT                                                                  
*              DERIVE OTHER FIELDS                                              
         SPACE 3                                                                
         CLI   RESSRCE,C'P'                                                     
         BE    DEMPUT11                                                         
         XC    SORTDATE,SORTDATE                                                
         MVC   SORTMNTH,SORTBOOK   FOR NAD, BOOK=MONTH                          
         XC    SORTBOOK+2(5),SORTBOOK+2                                         
         CLC   RESSRCE(3),=C'NAD'                                               
         BE    DEMPUT10                                                         
         MVC   SORTWEEK(1),SORTWKSQ (SORTWKSQ HAS REPLACED SORTRQBK)            
         MVI   SORTWEEK+1,0                                                     
         GOTO1 NETUNWK,DMCB,WORKBOOK,WORK,GETDAY,ADDAY                          
         MVC   MONDATE,WORK                                                     
         GOTO1 DATCON,DMCB,WORK,(8,WORK+6)                                      
         MVC   SORTWEEK+2(5),WORK+6                                             
         MVC   SORTBOOK(2),WORKBOOK                                             
*                                                                               
         CLI   SORTBOOK+1,49       SPECIAL STUFF FOR BLACK WEEKS                
         BL    DEMPUT7             HANDLE REGULAR WEEKS                         
         BE    DEMBL1                                                           
         CLI   SORTBOOK+1,51                                                    
         BL    DEMBL2                                                           
         BE    DEMBL3                                                           
         CLI   SORTBOOK+1,53                                                    
         BL    DEMBL4                                                           
         BE    DEMBL5                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
DEMBL1   MVC   SORTBOOK+2(5),=C'APR-3'   WEEK 49                                
         MVI   SORTBOOK+1,16       WEEK                                         
         MVI   SORTMNTH+1,4        MONTH                                        
         B     DEMPUT9                                                          
         SPACE 1                                                                
DEMBL2   MVC   SORTBOOK+2(5),=C'JUN-3'   WEEK 50                                
         MVI   SORTBOOK+1,24       WEEK                                         
         MVI   SORTMNTH+1,6        MONTH                                        
         B     DEMPUT9                                                          
         SPACE 1                                                                
DEMBL3   MVC   SORTBOOK+2(5),=C'AUG-3'   WEEK 51                                
         MVI   SORTBOOK+1,32       WEEK                                         
         MVI   SORTMNTH+1,8        MONTH                                        
         B     DEMPUT9                                                          
         SPACE 1                                                                
DEMBL4   MVC   SORTBOOK+2(5),=C'DEC-3'   WEEK 52                                
         MVI   SORTBOOK+1,48       WEEK                                         
         MVI   SORTMNTH+1,12       MONTH                                        
         B     DEMPUT9                                                          
         SPACE 1                                                                
DEMBL5   MVC   SORTBOOK+2(5),=C'AUG-3'   WEEK 53 (1984 ONLY)                    
         MVI   SORTBOOK+1,33       WEEK                                         
         MVI   SORTMNTH+1,8        MONTH                                        
         B     DEMPUT9                                                          
         EJECT                                                                  
DEMPUT7  TM    SORTBOOK+1,X'01'    IF WEEK IS ODD, THEN BOOK=WEEK               
         BO    DEMPUT8                                                          
         ZIC   R1,SORTBOOK+1       ELSE ADJUST TO PREVIOUS WEEK                 
         BCTR  R1,0                                                             
         STC   R1,SORTBOOK+1                                                    
         SPACE 1                                                                
DEMPUT8  ZIC   R1,WORKBOOK+1       FIGURE OUT MONTH FROM WEEK NO.               
         BCTR  R1,0                                                             
         SRL   R1,2                                                             
         LA    R1,1(R1)                                                         
         STC   R1,SORTMNTH+1                                                    
         SPACE 1                                                                
         BCTR  R1,0                SHOW BOOK AS MMM-1 OR MMM-2                  
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   SORTBOOK+2(3),0(R1)                                              
         MVC   SORTBOOK+5(2),=C'-1'                                             
         TM    SORTBOOK+1,X'02'                                                 
         BNO   DEMPUT9                                                          
         MVI   SORTBOOK+6,C'2'                                                  
         SPACE 1                                                                
DEMPUT9  XC    SORTRQBK,SORTRQBK                                                
         ZIC   R2,SORTDAY          COMPUTING DATE - GET DAY NO IN R2            
         LTR   R2,R2                                                            
         BNZ   *+8                                                              
         LA    R2,1                M-F=MON                                      
         CH    R2,=H'8'                                                         
         BL    *+8                                                              
         LA    R2,1                M-S=MON                                      
         BCTR  R2,0                CHANGE TO 0-6                                
         GOTO1 ADDAY,DMCB,MONDATE,WORK,(R2)                                     
         MVC   SORTDATE,WORK                                                    
         BAS   RE,NETPIN           PROGRAM DETAILS TO BUFFER                    
         SPACE 1                                                                
DEMPUT10 MVC   SORTQURT,SORTMNTH                                                
         ZIC   R1,SORTQURT+1                                                    
         BCTR  R1,0                                                             
         SR    R0,R0                                                            
         D     R0,=F'3'                                                         
         STC   R1,SORTQURT+1                                                    
         STC   R1,SORTQ2                                                        
         MVC   SORTQ2+1,SORTQURT                                                
         MVC   SORTYEAR,SORTQURT                                                
         LH    R1,SORTTIME         ENSURE EARLY AM AFTER MIDNIGHT               
         CH    R1,=H'600'                                                       
         BNL   *+8                                                              
         LA    R1,2400(R1)                                                      
         STH   R1,SORTTIME                                                      
         EJECT                                                                  
*              BUILD SORT KEY                                                   
         SPACE 3                                                                
DEMPUT11 L     R3,RECSTART                                                      
         LA    R2,DETS                                                          
         ZIC   R0,NDETS                                                         
         SPACE 1                                                                
DEMPUT12 ZIC   R1,1(R2)            DATA NUMBER                                  
         BCTR  R1,0                                                             
         SLL   R1,4                                                             
****     LA    R1,DATALIST(R1)                                                  
         L     R4,=A(DATALIST)                                                  
         AR    R1,R4                                                            
         L     R4,8(R1)            ADDRESS                                      
         A     R4,RELO                                                          
         ZIC   R5,8(R1)            LENGTH                                       
         XC    0(MAXFLEN,R3),0(R3)                                              
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)                                                    
         LA    R2,4(R2)                                                         
         LA    R3,MAXFLEN(R3)                                                   
         BCT   R0,DEMPUT12                                                      
         GOTO1 SORTER,DMCB,=C'PUT',RECSTART                                     
         BAS   RE,ANYTRACE                                                      
         MVI   ANYSORT,C'Y'                                                     
         ZIC   R2,NDETS                                                         
         SH    R2,=H'2'                                                         
         SLL   R2,2                                                             
         LA    R2,DETS(R2)         POSITION TO LAST DETAIL                      
         LA    R3,SORTREC+MAXFLEN*9                                             
         ZIC   R0,NDETS                                                         
         BCTR  R0,0                                                             
         LTR   R0,R0                                                            
         BZ    DEMPUT16                                                         
         SPACE 1                                                                
DEMPUT14 MVC   0(MAXFLN1,R3),=24X'FF'   GENERATE AVE/TOT RECORDS                
         MVC   MAXFLN1(1,R3),3(R2)                                              
         CLI   MAXFLN1(R3),0                                                    
         BE    DEMPUT15                                                         
         MVC   LASTKEY,SORTKEY                                                  
*                                  DON'T WEIGHT THE TOTALS                      
         CLI   3(R2),C'T'                                                       
         BNE   DEMPU14A                                                         
         MVC   SORTDEMS,LASTDEMS                                                
         MVC   SORTCNT,=F'1'                                                    
         MVC   SORTWT,=F'1'                                                     
         OC    SORTDCNT(8),SORTDCNT                                             
         BZ    DEMPUT17                                                         
         MVC   SORTDCNT(8),SORTCNT                                              
*                                                                               
DEMPU14A GOTO1 SORTER,DMCB,=C'PUT',RECSTART                                     
         MVC   SORTKEY,LASTKEY                                                  
         SPACE 1                                                                
DEMPUT15 SH    R2,=H'4'                                                         
         LA    R1,MAXFLEN                                                       
         SR    R3,R1                    MAXFLEN                                 
         BCT   R0,DEMPUT14                                                      
         SPACE 1                                                                
DEMPUT16 CLI   REPAVE,C'N'         REPORT AVE/TOT                               
         BE    XIT                                                              
         MVC   0(MAXFLN1,R3),=24X'FF'                                           
         MVC   MAXFLN1(1,R3),REPAVE                                             
*                                  DON'T WEIGHT THE TOTALS                      
         CLI   REPAVE,C'T'                                                      
         BNE   DEMPUT17                                                         
         MVC   SORTDEMS,LASTDEMS                                                
         MVC   SORTCNT,=F'1'                                                    
         MVC   SORTWT,=F'1'                                                     
         OC    SORTDCNT(8),SORTDCNT                                             
         BZ    DEMPUT17                                                         
         MVC   SORTDCNT(8),SORTCNT                                              
*                                                                               
DEMPUT17 GOTO1 SORTER,DMCB,=C'PUT',RECSTART                                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO TRACE SORT RECORDS OPTIONALLY                         
         SPACE 3                                                                
ANYTRACE NTR1                                                                   
         CLI   TRACEOPT,C'Y'                                                    
         BNE   XIT                                                              
         L     R1,RECSTART                                                      
         MVC   P,0(R1)                                                          
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         GOTO1 HEXOUT,DMCB,RECSTART,P,132,=C'SEP'                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         GOTO1 VPRINT,DMCB,P2-1,=C'BL02'                                        
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         B     XIT                                                              
         EJECT                                                                  
*              FIGURE OUT LENGTH IN 1/4 HRS FROM TIME                           
         SPACE 3                                                                
FIGLEN   NTR1                                                                   
         MVC   SORTQHRS,=F'2'                                                   
         OC    SORTTIME+2(2),SORTTIME+2                                         
         BZ    XIT                                                              
         XC    DUB,DUB                                                          
         LA    R2,SORTTIME                                                      
         LA    R3,DUB+3                                                         
         BAS   RE,GETQ                                                          
         LA    R2,2(R2)                                                         
         LA    R3,DUB+7                                                         
         BAS   RE,GETQ                                                          
         LM    R0,R1,DUB           START R0, END R1                             
         SR    R1,R0                                                            
         BNP   XIT                                                              
         ST    R1,SORTQHRS                                                      
         B     XIT                                                              
         SPACE 1                                                                
GETQ     NTR1                                                                   
         LH    R1,0(R2)            MILITARY TIME TO 1/4 HOUR                    
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CH    R1,=H'6'                                                         
         BNL   *+8                                                              
         LA    R1,24(R1)                                                        
         SH    R1,=H'6'                                                         
         SLL   R1,2                                                             
         LR    R2,R1                                                            
         SRDA  R0,32                                                            
         D     R0,=F'15'                                                        
         AR    R2,R1                                                            
         STC   R2,0(R3)                                                         
         B     XIT                                                              
         EJECT                                                                  
FIGMINS  NTR1                      WORK OUT MINUTES FROM SORTTIME               
         MVC   SORTMINS,=F'30'                                                  
         OC    SORTTIME+2(2),SORTTIME+2                                         
         BZ    XIT                                                              
         XC    DUB,DUB                                                          
         LA    R2,SORTTIME                                                      
         LA    R3,DUB                                                           
         BAS   RE,GETMINS                                                       
         LA    R2,2(R2)                                                         
         LA    R3,DUB+4                                                         
         BAS   RE,GETMINS                                                       
         LM    R0,R1,DUB           START R0, END R1                             
         SR    R1,R0                                                            
         BNP   XIT                                                              
         ST    R1,SORTMINS                                                      
         B     XIT                                                              
         SPACE 1                                                                
GETMINS  NTR1                                                                   
         LH    R1,0(R2)            MILITARY TIME TO MINUTES                     
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CH    R1,=H'6'                                                         
         BNL   *+8                                                              
         LA    R1,24(R1)                                                        
         MH    R1,=H'60'                                                        
         AR    R1,R0                                                            
         ST    R1,0(R3)                                                         
         B     XIT                                                              
         EJECT                                                                  
*              SAVE NETWORK PROGRAM DETAILS                                     
         SPACE 1                                                                
NETPIN   NTR1                                                                   
         L     R2,APRGBUFF                                                      
         SPACE 1                                                                
NETPIN2  CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),0             FIND A SLOT                                  
         BE    NETPIN4                                                          
         CLC   SORTPROG,0(R2)      OR PROGRAM ALREADY THERE                     
         BNE   NETPIN3                                                          
         CLC   SORTNET,20(R2)      OR PROGRAM ALREADY THERE                     
         BE    NETPIN4                                                          
         SPACE 1                                                                
NETPIN3  LA    R2,25(R2)                                                        
         B     NETPIN2                                                          
         SPACE 1                                                                
NETPIN4  MVC   0(16,R2),SORTPROG   NOTE PROGRAM                                 
         MVC   20(5,R2),SORTNET    AND NETWORK                                  
         GOTO1 DATCON,DMCB,SORTDATE,(2,SORTFAD)                                 
         CLC   SORTFAD,18(R2)                                                   
         BE    NETPIN5                                                          
         LH    R1,16(R2)           ADD 1 TO TIMES AIRED                         
         LA    R1,1(R1)                                                         
         STH   R1,16(R2)                                                        
         SPACE 1                                                                
NETPIN5  CLI   18(R2),0            UPDATE FIRST AIR DATE                        
         BE    NETPIN6                                                          
         CLC   SORTFAD,18(R2)                                                   
         BH    NETPIN8                                                          
         SPACE 1                                                                
NETPIN6  MVC   18(2,R2),SORTFAD                                                 
         SPACE 1                                                                
NETPIN8  XC    SORTFAD,SORTFAD                                                  
         XC    SORTNUM,SORTNUM                                                  
         B     XIT                                                              
         EJECT                                                                  
*              DIG OUT NETWORK PROGRAM DETAILS                                  
         SPACE 3                                                                
NETPDIG  NTR1                                                                   
         CLC   RESSRCE(3),=C'NTI'  R2=A(DETS)                                   
         BNE   XIT                 R4=A(SORTKEY)                                
*                                  R0=NDETS                                     
         XC    SORTPROG,SORTPROG                                                
         XC    SORTNET,SORTNET                                                  
         XC    SORTNUM,SORTNUM                                                  
         XC    SORTFAD,SORTFAD                                                  
         SPACE 1                                                                
NETPDIG2 CLI   1(R2),7             LOOKING FOR PROGRAM                          
         BNE   *+10                                                             
         MVC   SORTPROG,0(R4)                                                   
         CLI   1(R2),1                     AND NETWORK                          
         BNE   *+10                                                             
         MVC   SORTNET,0(R4)                                                    
         LA    R2,4(R2)                                                         
         LA    R4,MAXFLEN(R4)                                                   
         BCT   R0,NETPDIG2                                                      
         SPACE 1                                                                
         L     R2,APRGBUFF         LOOK FOR PROGRAM IN OUR BUFFER               
         SPACE 1                                                                
NETPDIG6 CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLC   SORTPROG,0(R2)      NEED A PROGRAM MATCH                         
         BNE   NETPDIG7                                                         
         OC    SORTNET,SORTNET     AND NETWORK IF REQUESTED                     
         BZ    NETPDIG8                                                         
         CLC   SORTNET,20(R2)                                                   
         BE    NETPDIG8                                                         
         SPACE 1                                                                
NETPDIG7 LA    R2,25(R2)                                                        
         B     NETPDIG6                                                         
         SPACE 1                                                                
NETPDIG8 MVC   SORTNUM,16(R2)      FOUND SO PASS BACK NUMBER                    
         MVC   SORTFAD,18(R2)      AND FIRST AIR DATE                           
         XC    16(4,R2),16(R2)     ENSURE USE ONLY ONCE                         
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL READING OF SORT RECORDS                                  
         SPACE 3                                                                
GET      NTR1                                                                   
         CLI   ANYSORT,C'N'                                                     
         BE    GETEND                                                           
         MVI   ANYSORT,C'N'                                                     
         MVI   LASTREC,X'FF'                                                    
         MVC   LASTREC+1(199),LASTREC                                           
         MVC   LASTREC+200(120),LASTREC                                         
         MVI   ANYRANK,C'N'                                                     
         MVI   SORTFRST,C'Y'                                                    
         XC    MYRANK,MYRANK                                                    
         XC    MYTIE,MYTIE                                                      
         XC    MYVALUE,MYVALUE                                                  
         XC    DROPKEY,DROPKEY                                                  
         XC    DROPCNT(32),DROPCNT                                              
         XC    DROPDEMS,DROPDEMS                                                
         B     GET3                                                             
         SPACE 1                                                                
GET2     L     R1,RECSTART                                                      
         MVC   LASTREC(200),0(R1)                                               
         MVC   LASTREC+200(120),200(R1)                                         
         SPACE 1                                                                
GET3     GOTO1 AGETSORT,DMCB,(RC)  GET NEXT RECORD                              
         TM    DMCB+8,X'80'                                                     
         BO    GETEND                                                           
*                                                                               
         LA    R2,DETS                                                          
         L     R4,RECSTART                                                      
         LA    R3,LASTREC                                                       
         LA    R5,P                                                             
         A     R5,DISP                                                          
         MVC   HEADP,SPACES                                                     
         LA    RF,HEADP                                                         
         A     RF,DISP                                                          
         ZIC   R0,NDETS                                                         
         BAS   RE,NETPDIG                                                       
         XC    MYAPRANK,MYAPRANK                                                
*                                                                               
         EJECT                                                                  
GET4     CLC   0(MAXFLN1,R4),=24X'FF'   TEST FOR AVE/TOT RECORDS                
         BE    GET10                                                            
         ZIC   R1,1(R2)            INDEX INTO DATALIST                          
         BCTR  R1,0                                                             
         SLL   R1,4                                                             
****     LA    R1,DATALIST(R1)                                                  
         L     RE,=A(DATALIST)                                                  
         AR    R1,RE                                                            
         CLI   0(R2),C'R'          RANK PRINTS ANYWAY                           
         BE    GET5                                                             
         CLI   DOWNOPT,C'D'        DOWNLOAD EMITS ANYWAY                        
         BE    GET5                                                             
         ZIC   RE,8(R1)            FIELD LENGTH (INPUT)                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),0(R4)                                                    
         BE    GET5B                                                            
         SPACE 1                                                                
GET5     BAS   RE,ROWS             SELECTIVELY FORMAT INTO P                    
*                                                                               
         MVI   LASTREC,X'FF'                                                    
         MVC   LASTREC+1(199),LASTREC                                           
         MVC   LASTREC+200(120),LASTREC                                         
         SPACE 1                                                                
GET5B    ST    R5,SAVER5           ALWAYS FORMAT INTO HEADP                     
         LR    R5,RF                                                            
         BAS   RE,ROWS                                                          
         L     R5,SAVER5                                                        
         SPACE 1                                                                
GET6     ZIC   RE,12(R1)           FIELD LENGTH (OUTPUT)                        
         LA    R5,1(R5,RE)                                                      
         LA    RF,1(RF,RE)                                                      
         LA    R2,4(R2)                                                         
         LA    R3,MAXFLEN(R3)                                                   
         LA    R4,MAXFLEN(R4)                                                   
         BCT   R0,GET4             NEXT DETAIL                                  
         SPACE 1                                                                
         MVI   AVSW,C'A'                                                        
         BAS   RE,COLS                                                          
*                                                                               
         L     R1,ARANK                                                         
         LTR   R1,R1                                                            
         BZ    GET6D                                                            
         CLC   4(4,R1),=F'1'       WAS A RAW RANK OF 1 SET?                     
         BNE   GET6D                                                            
         XC    MYRANK,MYRANK       THEN RESET VALUES                            
         XC    MYTIE,MYTIE                                                      
         XC    MYVALUE,MYVALUE                                                  
*                                                                               
GET6D    CLI   SOLOOPT,C'N'        OPTION TO SUPPRESS SHOWS                     
         BNE   GET6G               THAT ONLY AIRED ONCE                         
         CLC   SORTNUM,=H'1'                                                    
         BE    GET9A               BYPASS                                       
         SPACE 1                                                                
GET6G    OC    MINOPT,MINOPT       CHECK FOR MINIMUM OPTION                     
         BZ    GET7                                                             
         CLC   MINTWO,MINOPT                                                    
         BL    GET9                BYPASS                                       
         SPACE 1                                                                
GET7     OC    MAXOPT,MAXOPT       AND FOR MAXIMUM OPTION                       
         BZ    GET8                                                             
         CLC   MAXTWO,MAXOPT                                                    
         BH    GET9                BYPASS                                       
         SPACE 1                                                                
GET8     BAS   RE,SETRANK          GO AND FINALIZE THE RANKING                  
*                                                                               
         OC    TOPOPT,TOPOPT                                                    
         BZ    GET8B                                                            
         CLC   MYRANK,TOPOPT                                                    
         BH    GET9                BYPASS                                       
         SPACE 1                                                                
GET8B    CLI   DOWNOPT,C'D'        TEST DOWNLOAD                                
         BNE   GET8F                                                            
         BAS   RE,DOWNLINE         DOWNLOAD THE LINE                            
         B     GET8H                                                            
         SPACE 1                                                                
GET8F    GOTO1 SPOOL,DMCB,(R8)                                                  
GET8H    MVC   HEADP,SPACES                                                     
         B     GET2                                                             
         SPACE 1                                                                
GET9     MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
         MVC   HEADP,SPACES                                                     
*                                                                               
GET9A    LA    R3,DROPCNT          ADD IN THE NUMBERS TO DROPPED RECORD         
         LA    R2,SORTCNT                                                       
         L     R0,NUMCOLS                                                       
GET9B    L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,GET9B                                                         
*                                                                               
         B     GET3                                                             
         EJECT                                                                  
*              CONTROL TOTALLING AND AVERAGES                                   
         SPACE 1                                                                
GET10    CLI   DOWNOPT,C'D'        TEST DOWNLOAD                                
         BE    GET10E                                                           
         SPACE 1                                                                
GET10C   MVC   P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)     SPACE 1                                      
*                                                                               
GET10E   OC    DROPCNT(32),DROPCNT                                              
         BZ    GET10J                                                           
         LA    R3,DROPCNT          SUBTRACT DROPPED NUMBERS                     
         LA    R2,SORTCNT                                                       
         L     R0,NUMCOLS                                                       
GET10G   L     R1,0(R2)                                                         
         S     R1,0(R3)                                                         
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,GET10G                                                        
         XC    DROPKEY,DROPKEY                                                  
         XC    DROPCNT(32),DROPCNT                                              
         XC    DROPDEMS,DROPDEMS                                                
*                                                                               
GET10J   CLI   MAXFLN1(R4),C'A'                                                 
         BE    GET11                                                            
         CLI   MAXFLN1(R4),C'B'                                                 
         BNE   GET12                                                            
         SPACE 1                                                                
GET11    MVI   AVSW,C'A'                                                        
         MVC   0(5,R5),=C'(AVE)'   SHOW AVERAGES                                
*                                                                               
         BAS   RE,COLS                                                          
*                                                                               
         CLI   DOWNOPT,C'D'        TEST DOWNLOAD                                
         BNE   GET11C                                                           
         ST    R5,AVETOTAD                                                      
         BAS   RE,DOWNLINE         DOWNLOAD THE LINE                            
         B     GET12                                                            
         SPACE 1                                                                
GET11C   MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
GET12    CLI   MAXFLN1(R4),C'T'                                                 
         BE    GET13                                                            
         CLI   MAXFLN1(R4),C'B'                                                 
         BNE   GET2                                                             
         SPACE 1                                                                
GET13    MVI   AVSW,C'T'                                                        
         MVC   0(5,R5),=C'(TOT)'   AND/OR TOTALS                                
*                                                                               
         EJECT                                                                  
GET13C   BAS   RE,COLS                                                          
*                                                                               
         CLI   DOWNOPT,C'D'        TEST DOWNLOAD                                
         BNE   GET16                                                            
         ST    R5,AVETOTAD                                                      
         BAS   RE,DOWNLINE         DOWNLOAD THE LINE                            
         B     GET2                                                             
*                                                                               
GET16    MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     GET2                                                             
         SPACE 1                                                                
GETEND   GOTO1 SORTER,DMCB,=C'END' ONE FOR MEL                                  
         B     XIT                                                              
         EJECT                                                                  
*              SEND A FORMATTED LINE TO DOWNLOAD                                
*                                                                               
DOWNLINE NTR1                                                                   
         LA    R2,DETS                                                          
         LA    R5,P                                                             
         A     R5,DISP                                                          
         ZIC   R0,NDETS                                                         
DNLN30   ZIC   R3,1(R2)            INDEX INTO DATALIST                          
         BCTR  R3,0                                                             
         SLL   R3,4                                                             
****     LA    R3,DATALIST(R3)                                                  
         L     RE,=A(DATALIST)                                                  
         AR    R3,RE                                                            
*                                                                               
         OC    AVETOTAD,AVETOTAD   TEST AVE/TOT LINE                            
         BZ    DNLN50                                                           
         C     R5,AVETOTAD         TEST AVE/TOT ADDRESS                         
         BH    DNLN40                (BLANK TO THE RIGHT)                       
         BE    DNLN50                (EMIT '(AVE)' OR '(TOT)')                  
         ZIC   RE,12(R3)           FIELD LENGTH (OUTPUT)                        
         LA    R1,1(R5,RE)                                                      
         C     R1,AVETOTAD         TEST AVE/TOT ADDRESS                         
         BE    DNLN50                (EMIT FIELD TO LEFT OF AVE/TOT)            
DNLN40   ZIC   R1,12(R3)                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),SPACES      BLANK OUT OTHER FIELDS                       
*                                                                               
DNLN50   GOTO1 =A(DOWN),DMCB,1     1=PRINT TEXT FIELD                           
*                                                                               
         ZIC   RE,12(R3)           FIELD LENGTH (OUTPUT)                        
         LA    R5,1(R5,RE)                                                      
         LA    R2,4(R2)                                                         
         BCT   R0,DNLN30           NEXT DETAIL                                  
*                                                                               
         XC    AVETOTAD,AVETOTAD   SET AVE/TOT ADDRESS TO ZERO                  
*                                                                               
         LA    R5,P                                                             
         A     R5,DEMDISP                                                       
         ZIC   R0,NUMDEMS                                                       
DNLN70   EQU   *                                                                
         GOTO1 =A(DOWN),DMCB,2     2=PRINT NUMERIC FIELD                        
*                                                                               
         LA    R5,9(R5)            9-BYTE COLUMN (HARD CODED)                   
         BCT   R0,DNLN70           NEXT DEMO                                    
*                                                                               
         GOTO1 =A(DOWN),DMCB,3     3=END OF LINE                                
*                                                                               
         MVC   P,SPACES            CLEAR THE PRINT LINE                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET RANK NUMBER IN PRINT LINE                         
         SPACE 3                                                                
SETRANK  NTR1                                                                   
         L     R2,ARANK            ARE ADDRESSES ACTIVE                         
         LTR   R2,R2                                                            
         BZ    XIT                                                              
         L     R5,MYAPRANK                                                      
         LTR   R5,R5                                                            
         BZ    XIT                                                              
         L     R1,MYRANK           YES SO ADD 1 TO RANK                         
         LA    R1,1(R1)                                                         
         ST    R1,MYRANK                                                        
         CLC   0(4,R2),MYVALUE     IS THERE A CHANGE IN VALUE                   
         BE    SETR2                                                            
         MVC   MYTIE,MYRANK        YES SO RESET TIE                             
         MVC   MYVALUE,0(R2)       AND SAVE NEW VALUE                           
         B     SETR4                                                            
         SPACE 1                                                                
SETR2    MVI   3(R5),C'='          SHOW EQUAL SIGN FOR TIES                     
         SPACE 1                                                                
SETR4    EDIT  (4,MYTIE),(3,0(R5)) AND EDIT RANK NO.                            
         B     XIT                                                              
         EJECT                                                                  
*              FORMAT A FIELD IN THE ROWS                                       
         SPACE 3                                                                
ROWS     NTR1                                                                   
         L     RF,=V(ROWSC)                                                     
         BASR  RE,RF                                                            
         B     XIT                                                              
*              FORMAT THE COLUMNS                                               
         SPACE 3                                                                
COLS     NTR1                                                                   
         LA    R2,AVDEMOS                                                       
         MVI   MINMAX,1                                                         
         LA    R3,SORTDEMS                                                      
         LA    R4,P                                                             
         A     R4,DEMDISP                                                       
         ZIC   R5,NUMDEMS                                                       
         MVC   SPACING,SPACOPT     USER SELECTED SPACING (1-3)                  
         SPACE 1                                                                
COLS2    OC    0(4,R3),0(R3)                                                    
         BNZ   COLS4                                                            
         CLC   MINMAX,MINCOL                                                    
         BNE   *+10                                                             
         XC    MINTWO,MINTWO                                                    
         CLC   MINMAX,MAXCOL                                                    
         BNE   *+10                                                             
         XC    MAXTWO,MAXTWO                                                    
         SPACE 1                                                                
COLS4    L     R1,0(R3)                                                         
         SR    R0,R0                                                            
         MVC   DUB(8),SORTCNT                                                   
         LM    RE,RF,DUB           COUNT, WEIGHT TO RE,RF                       
         CLI   2(R2),1             UNLESS IT'S HOMES DATA                       
         BE    COLS5                                                            
         CLI   2(R2),16            OR CABLE RATINGS                             
         BE    COLS5                                                            
         CLI   2(R2),19                                                         
         BE    COLS5                                                            
         MVC   DUB(8),SORTDCNT                                                  
         LM    RE,RF,DUB           USE DEMO WEIGHTING                           
         SPACE 1                                                                
COLS5    CLC   DBFILE,=C'TP '                                                   
         BNE   *+8                                                              
         SRL   RE,1                DIVIDE COUNT BY 2 FOR TP                     
         CLI   AVSW,C'T'           MULTIPLY TOTALS BY COUNT                     
         BNE   *+6                                                              
         MR    R0,RE                                                            
         LTR   RF,RF               UNWEIGHT                                     
         BZ    COLS12                                                           
         TM    0(R2),X'20'         CPP/CPM DON'T NEED UNWEIGHTING               
         BNO   COLS6                                                            
         CLC   RESSRCE(3),=C'NAD'                                               
         BE    COLS6                                                            
         CLI   2(R2),X'FF'                                                      
         BNE   COLS12                                                           
         EJECT                                                                  
COLS6    OC    4(4,R3),4(R3)       IS THIS A DOUBLE DEMO                        
         BZ    COLS10                                                           
         L     RF,=F'1000'         YES SO DIVIDE BY SECOND                      
         CLI   1(R2),C'S'                                                       
         BNE   COLS8                                                            
         CLI   RESSRCE,C'P'                                                     
         BNE   COLS8                                                            
         L     RF,=F'100'          ADJUST FOR NETWORK SHARE                     
         SPACE 1                                                                
COLS8    MR    R0,RF                                                            
         L     RF,4(R3)                                                         
         SPACE 1                                                                
COLS10   SLDA  R0,1                                                             
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         SPACE 1                                                                
COLS12   L     R0,0(R3)                                                         
         ST    R1,0(R3)                                                         
         BAS   RE,EDITDEM                                                       
         ST    R0,0(R3)                                                         
*                                                                               
COLS14   LA    R2,3(R2)            3 BYTE DEMO                                  
         LA    R3,8(R3)            4 BYTE DEMO VALUE (X2)                       
         LA    R4,9(R4)            9 BYTE OUTPUT AREA                           
         AI    MINMAX,1                                                         
         BCT   R5,COLS2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EDIT A DEMO VALUE                                     
         SPACE 3                                                                
*              INPUTS              R2=A(3 BYTE DEMO)                            
*                                  R3=A(4 BYTE DEMO VALUE)                      
*                                  R4=A(9 BYTE OUTPUT AREA)                     
         SPACE 1                                                                
EDITDEM  NTR1                                                                   
         L     R0,0(R3)                                                         
         MVI   MULT,1                                                           
         SPACE 1                                                                
GETDEM   MVC   DUB(1),DBFILE                                                    
         MVC   DUB+1(1),1(R2)                                                   
         MVI   DUB+2,0                                                          
         LA    R1,EDITTAB          R1=A(EDIT TABLE)                             
*                                  SEARCH TABLE FOR DEMO                        
GETDEM2  CLI   0(R1),X'FF'         TEST E-O-LIST                                
         BE    GETDEM4                                                          
         CLC   0(2,R1),DUB         MATCH FILE/DEMO MODIFIER                     
         BE    *+12                                                             
         LA    R1,L'EDITTAB(R1)                                                 
         B     GETDEM2                                                          
         MVC   DUB+2(1),2(R1)      EXTRACT EDIT VALUES                          
*                                  EDIT DEMO VALUE                              
GETDEM4  TM    DUB+2,X'80'         TEST DEMO NEEDS SCALING                      
         BZ    *+8                                                              
         MH    R0,=H'10'                                                        
         TM    DUB+2,X'02'         TEST EDIT TO 2 DECIMALS                      
         BO    GETDEM6                                                          
         TM    DUB+2,X'01'         TEST EDIT TO 1 DECIMAL                       
         BO    GETDEM8                                                          
         SPACE 1                                                                
GETDEM5  EDIT  (R0),(9,0(R4))                                                   
         MVI   MULT,100                                                         
         B     GETDEMX                                                          
         SPACE 1                                                                
GETDEM6  EDIT  (R0),(9,0(R4)),2,ZERO=BLANK                                      
         B     GETDEMX                                                          
         SPACE 1                                                                
GETDEM8  EDIT  (R0),(9,0(R4)),1,ZERO=BLANK                                      
         MVI   MULT,10                                                          
         B     GETDEMX                                                          
         SPACE 1                                                                
GETDEMX  ZIC   R1,MULT                                                          
         MR    R0,R0                                                            
         CLC   MINMAX,MINCOL       FROM SPECIFIED MINIMUN COL NO.               
         BNE   *+8                                                              
         ST    R1,MINTWO           PASS BACK COLUMN 1 TO 2 DEC PLACES           
         CLC   MINMAX,MAXCOL       AND THE SAME FOR MAX                         
         BNE   *+8                                                              
         ST    R1,MAXTWO                                                        
         B     XIT                                                              
         SPACE 3                                                                
*                                  TABLE FOR DEMO EDITTING ROUTINE              
EDITTAB  DS    0XL3                FILE/DEMO MODIFIER/EDIT RULES                
         SPACE 1                                                                
         DC    C'NV',X'01'         NETWORK NTI                                  
         DC    C'NR',X'01'                                                      
         DC    C'NP',X'01'                                                      
         DC    C'NS',X'01'                                                      
         DC    C'NO',X'01'         TP PUT                                       
         DC    C'NQ',X'01'         TP SHR                                       
         DC    C'NL',X'01'         GAA RATING                                   
         DC    C'EV',X'01'         NETWORK EVN                                  
         DC    C'ER',X'01'                                                      
         DC    C'EP',X'81'                                                      
         DC    C'ES',X'01'                                                      
         DC    X'FF',X'00'                                                      
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R5),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, LTORG ETC                                             
         SPACE 3                                                                
SORTNET  DS    CL5                                                              
SORTRQBK DS    CL2                                                              
SORTBOOK DS    CL7                                                              
SORTDAY  DS    CL1                                                              
SORTTIME DS    F                                                                
SORTPROG DS    CL16                                                             
SORTEPIS DS    CL16                                                             
SORTUPDT DS    CL16                                                             
SORTCODE DS    CL6                                                              
SORTNTI  DS    CL5                                                              
SORTLPRO DS    CL25                LONG PROGRAM NAME                            
SORTLEPI DS    CL25                LONG EPISODE NAME                            
         SPACE 1                                                                
SORTMNTH DS    CL2                                                              
SORTQURT DS    CL2                                                              
SORTQ2   DS    CL2                                                              
SORTYEAR DS    CL1                                                              
SORTWEEK DS    CL7                                                              
SORTFILT DS    CL4                                                              
SORTRANK DC    F'0'                                                             
SORTDATE DS    CL6                                                              
SORTQHRS DS    F                                                                
SORTMINS DS    F                                                                
SORTDP   DS    CL5                                                              
SORTDPT  DS    CL7                                                              
SORTTYP  DS    CL7                                                              
SORTPER  DS    CL1                                                              
SORTMKT  DS    CL16                                                             
SORTEFF  DS    CL3                                                              
SORTFAD  DS    CL2                                                              
SORTNUM  DS    CL2                                                              
SORTWKSQ DS    CL1                                                              
         SPACE 1                                                                
RELOC    DC    A(*)                                                             
DAYLIST  DC    C'M-FMONTUEWEDTHUFRISATSUNM-SVARS-S'                             
SPOTDAYS DC    X'7C402010080402017F9003FF'                                      
QUARTS   DC    C'1ST.2ND.3RD.4TH.'                                              
ANYSORT  DC    C'N'                                                             
ANYUP    DC    C'N'                                                             
SIDOVER  DC    A(0)                                                             
         EJECT                                                                  
*              ADDRESSES    LTORG                                               
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,NNN,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=NNN'                                   
         EJECT                                                                  
NETTITLE DC    CL40'NETWORK RESEARCH REPORT'                                    
         SPACE                                                                  
         ENTRY DATALIST                                                         
DATALIST DS    0D                                                               
         DC    C'NETWORK ',AL1(05),AL3(SORTNET),AL1(07),AL3(0)   1              
         DC    C' BOOK   ',AL1(09),AL3(SORTRQBK),AL1(06),AL3(0)  2              
         DC    C'WEEK    ',AL1(07),AL3(SORTWEEK),AL1(05),AL3(0)  3              
         DC    C'MONTH   ',AL1(02),AL3(SORTMNTH),AL1(06),AL3(0)  4              
         DC    C'QUARTER ',AL1(02),AL3(SORTQURT),AL1(07),AL3(0)  5              
         DC    C'YEAR    ',AL1(01),AL3(SORTYEAR),AL1(04),AL3(0)  6              
         DC    C'PROGRAM ',AL1(16),AL3(SORTPROG),AL1(16),AL3(0)  7              
         DC    C' DAY    ',AL1(01),AL3(SORTDAY),AL1(05),AL3(0)   8              
         DC    C'TIME    ',AL1(04),AL3(SORTTIME),AL1(11),AL3(0)  9              
         DC    C'CODE    ',AL1(06),AL3(SORTCODE),AL1(06),AL3(0)  10             
         DC    C'RANK    ',AL1(04),AL3(SORTRANK),AL1(05),AL3(0)  11             
         DC    C'NTI     ',AL1(05),AL3(SORTNTI),AL1(06),AL3(0)   12             
         DC    C'FILTER  ',AL1(04),AL3(SORTFILT),AL1(6),AL3(0)   13             
         DC    C'DATE    ',AL1(06),AL3(SORTDATE),AL1(5),AL3(0)   14             
         DC    C'LENGTH  ',AL1(04),AL3(SORTMINS),AL1(6),AL3(0)   15             
         DC    C'DAYPART ',AL1(07),AL3(SORTDPT),AL1(7),AL3(0)    16             
         DC    C'EFFECT  ',AL1(03),AL3(SORTEFF),AL1(6),AL3(0)    17             
         DC    C'TIMES   ',AL1(02),AL3(SORTNUM),AL1(5),AL3(0)    18             
         DC    C'FIRST   ',AL1(02),AL3(SORTFAD),AL1(5),AL3(0)    19             
         DC    C'QUARTER ',AL1(02),AL3(SORTQ2),AL1(07),AL3(0)    20             
         DC    C'EPISODE ',AL1(16),AL3(SORTEPIS),AL1(16),AL3(0)  21             
         DC    C'PROGRAM ',AL1(25),AL3(SORTLPRO),AL1(25),AL3(0)  22             
         DC    C'EPISODE ',AL1(25),AL3(SORTLEPI),AL1(25),AL3(0)  23             
*                                                                               
         SPACE 2                                                                
BUMPLIST DC    AL1(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,49)                   
         DC    AL1(17,18,19,20,21,22,23,24,50)                                  
         DC    AL1(25,26,27,28,29,30,31,32,51,53) (53 IS FOR 1984 ONLY)         
         DC    AL1(33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,52)          
*                                                                               
         DC    AL1(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,49)                   
         DC    AL1(17,18,19,20,21,22,23,24,50)                                  
         DC    AL1(25,26,27,28,29,30,31,32,51,53) (53 IS FOR 1984 ONLY)         
         DC    AL1(33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,52)          
         DC    X'FF'                                                            
*                                                                               
         SPACE 2                                                                
VTYPES   DS    0F                  THESE MUST MATCH ENTRIES IN DSECT            
         DC    A(HOOK)             STARTING WITH AHOOK..                        
         DC    A(STACK)                                                         
         DC    A(DATALIST)                                                      
         DC    A(GETSORT)                                                       
         DC    A(PROGBUFF)                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
*  GET ALL ACTIVE SYNDICATORS                                                   
         SPACE 3                                                                
GETALL   CSECT                     GET ALL SYNDICATORS                          
         NMOD1 0,**GTAL**                                                       
         L     RC,0(R1)                                                         
         LA    R4,SYNSAVE          FIRST MERGE IN THE REGUALR NETS              
         LA    R5,NETSAVE                                                       
         ZIC   R3,NUMNETS                                                       
SEEDNET  CLC   0(5,R5),=C'ZZZ C'                                                
         BE    *+10                                                             
         CLC   0(5,R5),=C'ZZZ M'                                                
         BE    *+10                                                             
         CLC   0(5,R5),=C'ZZZ S'                                                
         BNE   *+14                                                             
         MVC   BYTE(1),4(R5)                                                    
         B     SEEDNET1                                                         
         SPACE 1                                                                
         MVC   0(5,R4),0(R5)                                                    
         LA    R4,5(R4)                                                         
SEEDNET1 LA    R5,5(R5)                                                         
         BCT   R3,SEEDNET                                                       
         ZIC   R3,NUMNETS          DECREMENT 1 FOR ZZZZ ENTRY                   
         BCTR  R3,R0                                                            
         SPACE 1                                                                
         LA    R5,KEYSAVE          NOW FIND SYNDICATORS                         
         USING PRKEY,R5                                                         
         XC    KEYSAVE,KEYSAVE     INIT                                         
         MVC   PRCODE(3),=C'PNN'                                                
GETALL1  MVI   PRSTAT+4,X'FF'      FORCE NEXT STATION                           
         XC    PRKMKT(10),PRKMKT                                                
         MVC   KEY,KEYSAVE                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PAVDIR',KEY,KEYSAVE                   
         CLI   8(R1),0                                                          
         BNE   GETALLX                                                          
         CLC   KEYSAVE(3),=C'PNN'                                               
         BNE   GETALLX                                                          
         CLC   PRSTAT+4(1),BYTE                                                 
         BNE   GETALL1                                                          
         MVC   0(5,R4),PRSTAT                                                   
         CLI   4(R4),C'C'          FUDGE FOR CABLE, STUPID CONFORMED            
         BNE   *+8                 IS FLAGGED IN 5TH BYTE                       
         MVI   4(R4),C'W'          SO CABLE IS SET TO W                         
         LA    R4,5(R4)                                                         
         LA    R3,1(R3)                                                         
         CH    R3,=H'150'                                                       
         BE    GETALLX                                                          
         B     GETALL1                                                          
*                                                                               
GETALLX  LA    R4,SYNSAVE                                                       
         STM   R3,R4,DMCB                                                       
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*  GET ALL ACTIVE SYNDICATORS                                                   
         SPACE 3                                                                
GETALN   CSECT                     GET ALL SYNDICATORS                          
         NMOD1 0,**GTAN**                                                       
         L     RC,0(R1)                                                         
         LA    R4,SYNSAVE          FIRST MERGE IN THE REGUALR NETS              
         LA    R5,NETSAVE                                                       
         ZIC   R3,NUMNETS                                                       
SEEDNS   CLC   0(5,R5),=C'ZZZ M'                                                
         BNE   *+14                                                             
         MVC   BYTE(1),4(R5)                                                    
         B     SEEDNS1                                                          
         SPACE 1                                                                
         MVC   0(5,R4),0(R5)                                                    
         LA    R4,5(R4)                                                         
SEEDNS1 LA     R5,5(R5)                                                         
         BCT   R3,SEEDNS                                                        
         ZIC   R3,NUMNETS          DECREMENT 1 FOR ZZZZ ENTRY                   
         BCTR  R3,R0                                                            
         SPACE 1                                                                
         LA    R5,KEYSAVE          NOW FIND SYNDICATORS                         
         USING PMKEY,R5                                                         
         XC    KEYSAVE,KEYSAVE     INIT                                         
         MVC   PMCODE(3),=C'QNN'                                                
GETALN1  MVI   PMSTAT+4,X'FF'      FORCE NEXT STATION                           
         XC    PMSTYP(08),PMSTYP                                                
         MVC   KEY,KEYSAVE                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PAVDIR',KEY,KEYSAVE                   
         CLI   8(R1),0                                                          
         BNE   GETALNX                                                          
         CLC   KEYSAVE(3),=C'QNN'                                               
         BNE   GETALNX                                                          
         CLC   PMSTAT+4(1),BYTE                                                 
         BNE   GETALN1                                                          
         MVC   0(5,R4),PMSTAT                                                   
         LA    R4,5(R4)                                                         
         LA    R3,1(R3)                                                         
         CH    R3,=H'150'                                                       
         BE    GETALNX                                                          
         B     GETALN1                                                          
*                                                                               
GETALNX  LA    R4,SYNSAVE                                                       
         STM   R3,R4,DMCB                                                       
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              CONTROL SORT RETRIEVAL                                           
         SPACE 3                                                                
GETSORT  CSECT                                                                  
         NMOD1 0,**GET***                                                       
         L     RC,0(R1)                                                         
         CLI   SORTFRST,C'Y'       FIRST TIME                                   
         BNE   GS4                                                              
         MVI   SORTFRST,C'N'                                                    
         XC    SORTKEY,SORTKEY                                                  
         BAS   RE,SORTOUT          ADD LIKE RECORDS TOGETHER AND PUT            
*                                  RESULTS TO DISK                              
         LA    R2,DETS                                                          
         L     R3,RECSTART                                                      
         SR    R4,R4               R4=NUMBER OF SORT FIELD                      
         ZIC   R0,NDETS                                                         
         SPACE 1                                                                
GS2      ST    R3,ARANK            SAVE ADDRESS OF RANK COLUMN                  
         STC   R4,NRANKCOL                                                      
         CLI   0(R2),C'R'          SEE IF RANK IS ACTIVE                        
         BNE   *+12                                                             
         MVI   ANYRANK,C'Y'        YES                                          
         B     GS8                                                              
         LA    R2,4(R2)                                                         
         LA    R3,MAXFLEN(R3)                                                   
         LA    R4,1(R4)                                                         
         BCT   R0,GS2                                                           
         B     GS6                                                              
         SPACE 1                                                                
GS4      CLI   ANYRANK,C'Y'                                                     
         BE    GS16                                                             
         SPACE 1                                                                
GS6      BAS   RE,GETOUT                                                        
         B     GSXIT                                                            
         SPACE 1                                                                
GETOUT   NTR1                                                                   
         L     R4,RECSTART                                                      
         SH    R4,=H'4'                                                         
         GET   SOUT,(R4)                                                        
         B     GSXIT                                                            
         SPACE 1                                                                
GETEOF   OI    DMCB+8,X'80'                                                     
         CLOSE (SOUT)                                                           
         FREEPOOL SOUT                                                          
         B     GSXIT                                                            
         EJECT                                                                  
*              RANKING - NEED ANOTHER SORT                                      
         SPACE 3                                                                
**GS8      GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                 
GS8      LA    R2,SORTCARD                                                      
         LA    R2,80(R2)                                                        
         GOTO1 SORTER,DMCB,SORTCARD,(R2),0                                      
         SPACE 1                                                                
GS10     BAS   RE,GETOUT                                                        
         TM    DMCB+8,X'80'                                                     
         BO    GS14                                                             
         L     R2,ARANK            POSITION TO RANK IN KEY                      
         CLI   0(R2),X'FF'         DONT MODIFY A TOTAL                          
         BE    GS12                                                             
         TM    DEMOS,X'20'         FOR CPP/M                                    
         BNO   GS10B               DON'T NEED TO UNWEIGHT                       
         CLC   RESSRCE(3),=C'NAD'                                               
         BE    GS10B                                                            
         MVC   0(4,R2),SORTDEMS                                                 
         B     GS12                                                             
         SPACE 1                                                                
GS10B    ICM   R0,15,SORTDEMS      COMPUTE AVERAGE VALUE                        
         SRDA  R0,31                                                            
         ICM   RF,15,SORTWT                                                     
         CLI   DEMOS+2,1                                                        
         BE    *+8                                                              
         ICM   RF,15,SORTDWT                                                    
         LTR   RF,RF                                                            
         BNZ   GS11                                                             
         SR    R1,R1                                                            
         B     GS11B                                                            
         SPACE 1                                                                
GS11     OC    SORTDEMS+4(4),SORTDEMS+4                                         
         BZ    GS11A                                                            
         M     R0,=F'1000'                                                      
         ICM   RF,15,SORTDEMS+4    PICK UP SECOND OPERAND                       
         SPACE 1                                                                
GS11A    DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         SPACE 1                                                                
GS11B    AH    R1,=H'1'            INSURE NON-ZERO COMPLEMENT                   
         LCR   R1,R1                                                            
         ST    R1,0(R2)                                                         
         SPACE 1                                                                
GS12     GOTO1 SORTER,DMCB,=C'PUT',RECSTART                                     
         B     GS10                                                             
         SPACE 1                                                                
GS14     XC    LASTVAL,LASTVAL                                                  
         MVC   RANKNO,=F'1'                                                     
         SPACE 1                                                                
GS16     GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BNZ   GS18                                                             
         OI    DMCB+8,X'80'        EOF                                          
         B     GSXIT                                                            
         EJECT                                                                  
*              RANKING - CONTROL RANK NUMBERS                                   
         SPACE 3                                                                
GS18     L     R2,RECSTART         MOVE RECORD INTO MY AREA                     
         MVI   DMCB+8,0                                                         
         L     R1,LSORTKEY                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R3)       FIRST THE KEY                                
         SPACE 1                                                                
         A     R2,LSORTKEY                                                      
         A     R3,LSORTKEY                                                      
         L     R1,NUMCOLS                                                       
         SLL   R1,2                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R3)       NOW THE COLUMNS                              
         SPACE 1                                                                
         L     R2,ARANK            INSPECT RANK COLUMN                          
         CLC   0(MAXFLN1,R2),=25X'FF'                                           
         BNE   GS20                                                             
*                                  TOTAL RECORD - RESET RANK CONTROL            
         MVC   RANKNO,=F'1'                                                     
         B     GSXIT                                                            
         SPACE 1                                                                
GS20     CLI   NRANKCOL,0          UNLESS RANK IS ON THE WHOLE REPORT           
         BE    GS21                                                             
         ZIC   R1,NRANKCOL                                                      
         SLL   R1,4                                                             
         BCTR  R1,0                                                             
         L     RE,RECSTART                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LASTRFLD(0),0(RE)                                                
         BE    GS21                CHECK CONTROL BREAK ON PREVIOUS              
         MVC   RANKNO,=F'1'        RESET RANK NO                                
         SPACE 1                                                                
GS21     L     RE,RECSTART                                                      
         MVC   LASTRFLD,0(RE)                                                   
         SPACE 1                                                                
         MVC   4(4,R2),RANKNO                                                   
         L     R3,RANKNO                                                        
         LA    R3,1(R3)                                                         
         ST    R3,RANKNO                                                        
         B     GSXIT                                                            
         EJECT                                                                  
*              ROUTINE TO GET RECORDS FROM SORTER AND                           
*              PUT THEM TO SEQUENTIAL DISK FILE                                 
         SPACE 2                                                                
SORTOUT  NTR1                                                                   
         XC    SORTKEY,SORTKEY                                                  
         XC    SORTCNT(32),SORTCNT                                              
         XC    SORTDEMS,SORTDEMS                                                
         OPEN  (SOUT,(OUTPUT))                                                  
         L     R4,RECSTART                                                      
         SPACE 1                                                                
SO2      GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BNZ   SO4                                                              
         GOTO1 SORTER,DMCB,=C'END' ONE FOR MEL                                  
         BAS   RE,PUTOUT                                                        
         CLOSE (SOUT)                                                           
         FREEPOOL SOUT                                                          
         OPEN  (SOUT,(INPUT))                                                   
         B     GSXIT                                                            
         SPACE 1                                                                
SO4      L     R5,LSORTKEY                                                      
         BCTR  R5,0                                                             
         OC    SORTKEY,SORTKEY                                                  
         BZ    SO6                                                              
         CLI   EVERYOPT,C'Y'       OPTION TO SHOW EVERY RECORD                  
         BNE   SO4D                                                             
         LA    R1,SORTREC+MAXFLEN*9      CHECK IT IS NOT A TOTAL                
         ZIC   R0,NDETS                                                         
         SPACE 1                                                                
SO4B     CLC   0(MAXFLN1,R1),=25X'FF'                                           
         BE    SO4D                                                             
         LA    RE,MAXFLEN                                                       
         SR    R1,RE                                                            
         BCT   R0,SO4B                                                          
         B     SO5                                                              
         SPACE 1                                                                
SO4D     EX    R5,*+8              IS THIS KEY THE SAME                         
         B     *+10                                                             
         CLC   0(0,R4),0(R3)                                                    
         BE    SO6                                                              
         SPACE 1                                                                
SO5      BAS   RE,PUTOUT           NO - SO PUT OUT THE SAVED RECORD             
         XC    SORTCNT(32),SORTCNT                                              
         XC    SORTDEMS,SORTDEMS                                                
         SPACE 1                                                                
SO6      EX    R5,*+8              SAVE KEY                                     
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         A     R3,LSORTKEY         ADD IN THE NUMBERS                           
         LA    R2,SORTCNT                                                       
         L     R0,NUMCOLS                                                       
         SPACE 1                                                                
SO8      L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,SO8                                                           
         B     SO2                                                              
         SPACE 1                                                                
PUTOUT   NTR1                                                                   
         L     R4,RECSTART                                                      
         SH    R4,=H'4'                                                         
         MVC   0(4,R4),SOUTHEAD                                                 
         PUT   SOUT,(R4)                                                        
         B     GSXIT                                                            
         EJECT                                                                  
*              DCB FOR INTERNAL FILE AND OTHER ODDMENTS                         
         SPACE 3                                                                
SOUT     DCB   RECFM=VB,BLKSIZE=4000,DSORG=PS,MACRF=(PM,GM),           X        
               LRECL=2000,DDNAME=SOUT,EODAD=GETEOF                              
         SPACE 1                                                                
GSXIT    XIT1                                                                   
         SPACE 1                                                                
LASTVAL  DC    F'0'                                                             
LASTRFLD DC    XL128'00'                                                        
NRANKCOL DC    X'00'                                                            
RANKNO   DC    F'1'                                                             
RANKTIE  DC    F'1'                                                             
MFLHALF  DC    AL2(MAXFLEN)                                                     
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
********************************************                                    
* DOWNLOAD ROUTINE                                                              
*                                                                               
*                                                                               
DOWN     CSECT                                                                  
         NMOD1 0,**DOWNLD                                                       
*                                                                               
         LA    R4,DOWNWORK                                                      
         USING DLCBD,R4                                                         
         LA    R2,PRNTLN+1                                                      
         ST    R2,DLCBAPL          PRINT LINE                                   
         LA    R2,DOWNHOOK                                                      
         ST    R2,DLCBAPR          PRINT ROUTINE                                
*                                                                               
         L     R2,0(R1)            ACTION CODE                                  
         CH    R2,=H'2'                                                         
         BH    DOWN40                                                           
         CH    R2,=H'1'                                                         
         BL    DNLD1ST             0=START OF REPORT                            
         BE    DNTEXT              1=PRINT TEXT FIELD                           
         B     DNNUM               2=PRINT NUMERIC FIELD                        
DOWN40   CH    R2,=H'4'                                                         
         BL    DNLDEOL             3=END OF LINE                                
         BE    DNNODATA            4='NO DATA'                                  
         B     DNLDEND             5=END OF REPORT                              
*                                                                               
*        I/P - R5 CONTAINS ADDRESS OF PRINT LINE FIELD                          
DNNUM    MVI   DLCBTYP,DLCBNUM     TYPE (NUMERIC)                               
         LA    R1,8                HARD CODED 8 (WIDTH OF COLUMN-1)             
         B     DNTEXT10                                                         
*                                                                               
*        I/P - R5 CONTAINS ADDRESS OF PRINT LINE FIELD                          
*              12(1,R3) IS FIELD LENGTH                                         
DNTEXT   MVI   DLCBTYP,DLCBTXT     TYPE (TEXT)                                  
         ZIC   R1,12(R3)           USE COLUMN WIDTH                             
         BCTR  R1,0                                                             
DNTEXT10 MVI   DLCBACT,DLCBPUT     ACTION (PUT)                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),SPACES      TEST IF FIELD IS ALL SPACES                  
         BNE   DNTEXT30                                                         
         LA    R1,1                TRANSMIT MINIMUM LENGTH FIELD                
         CLI   DLCBTYP,DLCBNUM     TEST TYPE (NUMERIC)                          
         BNE   DNTEXT80            TRANSMIT ONE SPACE (TEXT)                    
         MVI   0(R5),C'0'                                                       
         B     DNTEXT80            TRANSMIT ONE ZERO (NUMERIC)                  
*                                                                               
DNTEXT30 LA    R6,0(R1,R5)         R6 - LAST BYTE OF FIELD                      
         LA    R1,1(R1)                                                         
DNTEXT40 CLI   0(R6),C' '          SCAN FOR TRAILING SPACES                     
         BNE   DNTEXT50                                                         
         BCTR  R6,0                                                             
         BCT   R1,DNTEXT40                                                      
         DC    H'0'                                                             
*                                                                               
DNTEXT50 CLI   DLCBTYP,DLCBTXT     TEST TYPE (TEXT)                             
         BE    DNTEXT80                                                         
         CLI   0(R5),C' '          SCAN FOR LEADING SPACES IN NUMERIC           
         BNE   DNTEXT80                                                         
         LA    R5,1(R5)                                                         
         BCT   R1,DNTEXT50                                                      
         DC    H'0'                                                             
DNTEXT80 STC   R1,DLCBLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD,0(R5)       MOVE PRINT LINE DATA TO DATA FIELD           
*                                                                               
DNLINK   LR    R1,R4                                                            
         L     RF,=V(DLFLD)                                                     
         BASR  RE,RF                                                            
*                                                                               
DOWNX    XIT1                                                                   
*                                                                               
         SPACE 3                                                                
DNLD1ST  MVI   DLCBACT,DLCBSOR     ACTION (START OF REPORT)                     
         B     DNLINK                                                           
         SPACE 1                                                                
DNLDEND  MVI   DLCBACT,DLCBEOR     ACTION (END OF REPORT)                       
         B     DNLINK                                                           
         SPACE 1                                                                
DNNODATA MVC   DLCBFLD(7),=C'NO DATA'                                           
         MVI   DLCBACT,DLCBPUT     ACTION (PUT)                                 
         MVI   DLCBTYP,DLCBTXT     TYPE (TEXT)                                  
         LR    R1,R4                                                            
         L     RF,=V(DLFLD)                                                     
         BASR  RE,RF                                                            
*              FALL THRU...        ISSUE END-OF-LINE                            
*                                                                               
DNLDEOL  MVI   DLCBACT,DLCBEOL     ACTION (END OF PRINT LINE)                   
*                                                                               
         LR    R1,R4                                                            
         L     RF,=V(DLFLD)                                                     
         BASR  RE,RF                                                            
         MVI   PRNTLN,X'40'                                                     
         MVC   PRNTLN+1(L'PRNTLN-1),PRNTLN                                      
         B     DOWNX                                                            
*                                                                               
         SPACE 1                                                                
DOWNHOOK NTR1                                                                   
         GOTO1 VPRINT,DMCB,PRNTLN,=C'BL01'                                      
         XIT1                                                                   
*                                                                               
         SPACE 1                                                                
DOWNWORK DS    CL100                                                            
PRNTLN   DC    CL168' '                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
ROWSC    CSECT                                                                  
         USING *,RF                                                             
         NTR1                                                                   
         LA    RB,ROWSC                                                         
         USING ROWSC,RB                                                         
         DROP  RF                                                               
         ZIC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,ROWBRNCH(R1)                                                  
         BR    R1                                                               
         SPACE 1                                                                
ROWBRNCH B     ROW2        1       NETWORK                                      
         B     ROW4        2       BOOK                                         
         B     ROW8        3       WEEK                                         
         B     ROW6        4       MONTH                                        
         B     ROW10       5       QUARTER                                      
         B     ROW12       6       YEAR                                         
         B     ROW14       7       PROGRAM                                      
         B     ROW16       8       DAY                                          
         B     ROW18       9       TIME                                         
         B     ROW21       10      CODE                                         
         B     ROW20       11      RANK                                         
         B     ROW19       12      NTI                                          
         B     ROW22       13      FILTERS                                      
         B     ROW24       14      DATE                                         
         B     ROW26       15      LENGTH                                       
         B     ROW28       16      DAYPART                                      
         B     ROW36       17      EFFECTIVE DATE                               
         B     ROW38       18      NUMBER                                       
         B     ROW40       19      FIRST AIR DATE                               
         B     ROW42       20      QUARTER BY YEAR                              
         B     ROW44       21      EPISODE TITLE                                
         B     ROW46       22      LONG PROGRAM                                 
         B     ROW48       23      LONG EPISODE TITLE                           
         SPACE 1                                                                
ROW2     MVC   2(4,R5),0(R4)       NETWORK                                      
         CLC   RESSRCE(3),=C'NAD'                                               
         BNE   ROW3                                                             
         MVI   5(R5),C' '                                                       
         B     RXIT                                                             
ROW3     CLI   5(R5),C'A'          TEST ASCRIBED                                
         BNE   RXIT                                                             
         MVI   5(R5),C' '          REPLACE 'A' WITH BLANK                       
         B     RXIT                                                             
         SPACE 1                                                                
ROW4     LA    R4,2(R4)                                                         
         CLC   RESSRCE(3),=C'NAD'  NAD USES MONTHLY BOOKS                       
         BNE   ROW8                                                             
         SPACE 1                                                                
ROW6     EDIT  (1,(R4)),(2,4(R5))                                               
         ZIC   R1,1(R4)                                                         
         BCTR  R1,R0                                                            
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   1(3,R5),0(R1)                                                    
         B     RXIT                                                             
         SPACE 1                                                                
ROW8     CLC   RESSRCE(3),=C'NAD'                                               
         BNE   ROW9                                                             
         MVC   0(4,R5),0(R4)       WEEKS (NAD)                                  
         B     RXIT                                                             
         SPACE 1                                                                
ROW9     MVC   0(5,R5),2(R4)       WEEK (NETWORK)                               
         B     RXIT                                                             
         SPACE 1                                                                
ROW10    ZIC   R1,1(R4)            QUARTER                                      
         SLL   R1,2                                                             
         LA    R1,QUARTS(R1)                                                    
         MVC   0(4,R5),0(R1)                                                    
         EDIT  (1,(R4)),(2,5(R5))                                               
         B     RXIT                                                             
         SPACE 1                                                                
ROW12    MVC   0(2,R5),=C'19'      YEAR                                         
         EDIT  (1,(R4)),(2,2(R5))                                               
         B     RXIT                                                             
         SPACE 1                                                                
ROW14    MVC   0(16,R5),0(R4)      PROGRAM                                      
         OC    0(16,R5),SPACES                                                  
         B     RXIT                                                             
         SPACE 1                                                                
ROW16    ZIC   R1,0(R4)            DAY                                          
         MH    R1,=H'3'                                                         
         LA    R1,DAYLIST(R1)                                                   
         MVC   1(3,R5),0(R1)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   RXIT                                                             
         MVC   1(3,R5),=C'ALL'                                                  
         B     RXIT                                                             
         SPACE 1                                                                
ROW18    LH    R1,0(R4)            CONVERT BACK EARLY AM                        
         MVC   DUB(4),0(R4)                                                     
         CH    R1,=H'2400'                                                      
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         STH   R1,DUB                                                           
         CLI   DPOPT,C'Y'          OPTION TO USE DAYPART DAY/TIME               
         BNE   *+24                                                             
         LH    R1,2(R4)            CONVERT BACK EARLY AM                        
         CH    R1,=H'2400'                                                      
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         STH   R1,DUB+2                                                         
         GOTO1 UNTIME,DMCB,DUB,(R5)                                             
         B     RXIT                TIME                                         
         SPACE 1                                                                
ROW19    MVC   0(L'SORTNTI,R5),0(R4)       NTI CODE                             
         B     RXIT                                                             
         SPACE 1                                                                
ROW20    OC    MYAPRANK,MYAPRANK                                                
         BNZ   RXIT                                                             
         ST    R5,MYAPRANK         RANK - SAVE ADDRESS FOR LATER                
         B     RXIT                                                             
         SPACE 1                                                                
ROW21    MVC   0(6,R5),0(R4)       CODE                                         
         B     RXIT                                                             
         SPACE 1                                                                
ROW22    MVC   1(4,R5),0(R4)       FILTERS                                      
         B     RXIT                                                             
         SPACE 1                                                                
ROW24    GOTO1 DATCON,DMCB,(0,(R4)),(8,WORK)                                    
         MVC   0(5,R5),WORK        DATE                                         
         B     RXIT                                                             
         SPACE 1                                                                
ROW26    L     R1,0(R4)            LENGTH                                       
         EDIT  (R1),(4,(R5))                                                    
         B     RXIT                                                             
         SPACE 1                                                                
ROW28    MVC   0(7,R5),0(R4)       DAYPART                                      
         B     RXIT                                                             
         SPACE 1                                                                
ROW36    OC    0(3,R4),0(R4)                                                    
         BZ    RXIT                                                             
         GOTO1 DATCON,DMCB,(3,0(R4)),(4,(R5))                                   
         B     RXIT                                                             
         SPACE 1                                                                
ROW38    EDIT  (2,SORTNUM),(3,0(R5))   TIMES AIRED                              
         B     RXIT                                                             
         SPACE 1                                                                
ROW40    OC    SORTFAD,SORTFAD     FIRST AIR DATE                               
         BZ    RXIT                                                             
         GOTO1 DATCON,DMCB,(2,SORTFAD),(4,(R5))                                 
         B     RXIT                                                             
         SPACE 1                                                                
ROW42    ZIC   R1,0(R4)            QUARTER IN QUARTER SEQUENCE                  
         SLL   R1,2                                                             
         LA    R1,QUARTS(R1)                                                    
         MVC   0(4,R5),0(R1)                                                    
         EDIT  (1,1(R4)),(2,5(R5))                                              
         B     RXIT                                                             
         SPACE 1                                                                
ROW44    MVC   0(16,R5),0(R4)      EPISODE TITLE                                
         B     RXIT                                                             
         SPACE 1                                                                
ROW46    MVC   0(25,R5),0(R4)      LONG PROGRAM                                 
         B     RXIT                                                             
         SPACE 1                                                                
ROW48    MVC   0(25,R5),0(R4)      LONG EPISODE TITLE                           
         B     RXIT                                                             
         SPACE 1                                                                
RXIT     XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     CSECT                                                                  
         USING *,RF                                                             
         SPACE 1                                                                
HOOKNTR  NTR1                                                                   
         LA    RB,HOOK                                                          
         USING HOOK,RB                                                          
         DROP  RF                                                               
         SPACE 1                                                                
         MVC   RESTITA,SPACES                                                   
         MVC   RESTITB,SPACES                                                   
         MVC   RESTITC,SPACES                                                   
         MVI   HOWWIDE,132                                                      
         OC    P,HEADP             SHOW FULL DETAILS                            
         LA    R2,H6               NETWORK/STATION                              
         CLI   NUMNETS,1           ONLY IF 1 SPECIFIED                          
         BNE   HOOK4                                                            
         MVC   0(7,R2),=C'NETWORK'                                              
         MVC   10(4,R2),SORTNET                                                 
         CLI   13(R2),C'A'         TEST ASCRIBED                                
         BNE   HOOK4                                                            
         MVI   13(R2),C' '         REPLACE 'A' WITH BLANK                       
         EJECT                                                                  
*              HEADINGS                                                         
         SPACE 3                                                                
HOOK4    LA    R2,DETS                                                          
         LA    R3,RESTITA                                                       
         A     R3,DISP                                                          
         MVC   RESCOLS,SPACES           INITIALIZE BOXES                        
         LA    R4,RESCOLS-1                                                     
         A     R4,DISP                                                          
         MVI   0(R4),C'L'                                                       
         ZIC   R0,NDETS                                                         
*                                                                               
         SPACE 1                                                                
* DETAIL COLUMN HEADINGS                                                        
*                                                                               
HOOKDET  ZIC   R5,1(R2)            PICK UP DATA NUMBER                          
         BCTR  R5,0                                                             
         SLL   R5,4                                                             
         A     R5,ADTALIST                                                      
         MVC   0(8,R3),0(R5)       OUTPUT COLUMN HEADER                         
         CLC   0(5,R3),=C'FIRST'   FLESH OUT ODD SECOND LINES                   
         BNE   *+10                                                             
         MVC   132(5,R3),=C'AIRED'                                              
         CLC   0(5,R3),=C'TIMES'                                                
         BNE   HOOKDET2                                                         
         MVC   132(5,R3),=C'AIRED'                                              
         CLC   RESSRCE(5),=CL5'NTI,T'                                           
         BNE   HOOKDET2                                                         
         MVC   0(5,R3),=CL5'HALF '                                              
         MVC   132(5,R3),=CL5'HOURS'                                            
HOOKDET2 CLI   BOXOPT,C'Y'                                                      
         BE    HOOKDET4                                                         
         GOTO1 UNDERLIN,DMCB,(8,(R3)),132(R3),8                                 
         SPACE 1                                                                
HOOKDET4 ZIC   R1,12(R5)           OUTPUT LENGTH                                
         LA    R3,1(R1,R3)         +1                                           
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),C'C'                                                       
         LA    R2,4(R2)                                                         
         BCT   R0,HOOKDET          NEXT DETAIL HEADING                          
*                                                                               
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'C'                                                       
*                                                                               
         EJECT                                                                  
* DEMO CATEGORY HEADINGS                                                        
*                                                                               
         LA    R2,DEMOS                                                         
         ZIC   R0,ACTNDEM                                                       
         SPACE 1                                                                
HOOKDEM  MVC   DUB(3),0(R2)                                                     
         CLI   DUB+1,C'T'          CHANGE T TO I                                
         BNE   *+8                                                              
         MVI   DUB+1,C'I'                                                       
         GOTO1 DEMOCON,DMCB,(0,DUB),(7,WORK),(0,DBLOCK)                         
         GOTO1 CENTER,DMCB,WORK,7                                               
         MVC   1(7,R3),WORK                                                     
         MVC   134(5,R3),=C'(000)'                                              
         CLC   RESSRCE(3),=C'NAD'                                               
         BNE   *+10                                                             
         MVC   134(5,R3),=C' 0000'                                              
         CLI   DUB+1,C'R'                                                       
         BNE   *+10                                                             
         MVC   134(5,R3),=C'(RTG)'                                              
         CLI   DUB+1,C'S'                                                       
         BNE   *+10                                                             
         MVC   134(5,R3),=C'(SHR)'                                              
         CLI   DUB+1,C'U'                                                       
         BNE   *+10                                                             
         MVC   134(5,R3),=C'(UNI)'                                              
         CLI   DUB+1,C'D'                                                       
         BNE   *+10                                                             
         MVC   134(5,R3),=C'(COM)'                                              
         CLI   DUB+1,C'V'                                                       
         BNE   *+10                                                             
         MVC   134(5,R3),=C'(VPH)'                                              
         CLI   DUB+1,C'O'                                                       
         BNE   *+10                                                             
         MVC   134(5,R3),=C'TPPUT'                                              
         CLI   DUB+1,C'Q'                                                       
         BNE   *+10                                                             
         MVC   134(5,R3),=C'TPSHR'                                              
         CLI   DUB+1,C'L'                                                       
         BNE   *+10                                                             
         MVC   134(5,R3),=C'GARTG'                                              
         CLI   DUB+1,C'N'                                                       
         BNE   *+10                                                             
         MVC   134(5,R3),=C'GAIMP'                                              
         CLI   DUB+1,C'M'                                                       
         BNE   *+10                                                             
         MVC   134(5,R3),=C'GAVPH'                                              
         CLI   DUB+1,C'P'                                                       
         BNE   HOOKDEM3                                                         
         MVC   134(5,R3),=C'(PUT)'                                              
         CLI   DUB+2,1                                                          
         BNE   HOOKDEM3                                                         
         MVI   135(R3),C'H'                                                     
         SPACE 1                                                                
HOOKDEM3 CLC   RESSRCE(3),=C'NAD'  THIRD LINE FOR NAD                           
         BNE   HOOKDEM4                                                         
         GOTO1 CENTER,DMCB,WORK+12,7                                            
         MVC   265(7,R3),WORK+12                                                
         SPACE 1                                                                
HOOKDEM4 LA    R2,3(R2)                                                         
         LA    R3,9(R3)            9 POSITION COLUMN                            
         LA    R4,9(R4)            9 POSITION COLUMN                            
         MVI   0(R4),C'C'                                                       
         BCT   R0,HOOKDEM          NEXT DEMO CATEGORY HEADING                   
*                                                                               
         MVI   0(R4),C'R'                                                       
*                                                                               
         CLI   DOWNOPT,C'D'        TEST DOWNLOAD                                
         BE    HOOKDOWN                                                         
         SPACE 1                                                                
         GOTO1 VRESHEAD                                                         
HOOKDEMX XIT1                                                                   
*                                                                               
         EJECT                                                                  
HOOKDOWN EQU   *                   ONE-TIME-ONLY ROUTINE                        
         LA    R5,RESTITA                                                       
         LA    R4,3                RESTITA - RESTITB - RESTITC                  
*                                                                               
HOOKDN10 LA    R2,DETS                                                          
         A     R5,DISP                                                          
         ZIC   R0,NDETS                                                         
*                                                                               
         SPACE 1                                                                
* DETAIL COLUMN HEADINGS                                                        
*                                                                               
HOOKDN20 ZIC   R3,1(R2)            PICK UP DATA NUMBER                          
         BCTR  R3,0                                                             
         SLL   R3,4                                                             
         A     R3,ADTALIST         12(1,R3) - FIELD LENGTH                      
*                                                                               
         GOTO1 =A(DOWN),DMCB,1     2=PRINT TEXT FIELD                           
*                                                                               
         ZIC   R1,12(R3)           OUTPUT LENGTH                                
         LA    R5,1(R1,R5)         +1                                           
         LA    R2,4(R2)                                                         
         BCT   R0,HOOKDN20         NEXT DETAIL HEADING                          
*                                                                               
         SPACE 1                                                                
* DEMO CATEGORY HEADINGS                                                        
*                                                                               
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
         LA    R3,=XL13'08'        12(1,R3) - FIELD LENGTH                      
         ZIC   R0,ACTNDEM                                                       
         SPACE 1                                                                
*                                                                               
HOOKDN60 EQU   *                                                                
         GOTO1 =A(DOWN),DMCB,1     2=PRINT TEXT FIELD                           
*                                                                               
         LA    R5,9(R5)            9 POSITION COLUMN                            
         BCT   R0,HOOKDN60         NEXT DEMO CATEGORY HEADING                   
*                                                                               
         BCT   R4,*+8              HAVE 3 LINES BEEN PROCESSED                  
         B     HOOKDN90            YES                                          
         CH    R4,=H'2'            ARE WE AT LINE TWO OR THREE                  
         BNE   *+12                                                             
         LA    R5,RESTITB          PROCESS LINE TWO                             
         B     HOOKDN80                                                         
         CLC   RESSRCE(3),=C'NAD'  THIRD LINE FOR NAD                           
         BNE   HOOKDN90                                                         
         LA    R5,RESTITC          PROCESS LINE THREE                           
*                                                                               
HOOKDN80 EQU   *                                                                
         GOTO1 =A(DOWN),DMCB,3     3=END OF LINE                                
         B     HOOKDN10                                                         
*                                                                               
HOOKDN90 EQU   *                                                                
         GOTO1 =A(DOWN),DMCB,3     3=END OF LINE                                
         B     HOOKDEMX                                                         
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BUFFER AREAS FOR RESEARCH                                        
         SPACE 3                                                                
BUFFC    CSECT                                                                  
         ENTRY STACK                                                            
         ENTRY PROGBUFF                                                         
         ENTRY HEDSPECS                                                         
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**SPECS*'                                                      
HEDSPECS SSPEC H1,1,C'MEDIA'                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,RUN                                                        
         SSPEC H5,97,REPORT                                                     
         SSPEC H5,114,PAGE                                                      
         DC    X'00'                                                            
         SPACE 1                                                                
         DC    C'***STACK'                                                      
STACK    DC    5000X'00'                                                        
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'PROGBUFF'                                                      
PROGBUFF DC    75000X'00'                                                       
         DC    X'FF'                                                            
TRKBUF   CSECT                                                                  
         DS    3000X'00'          DIR OPT AREA                                  
         DS    50000X'00'         DIR TRACK BUFFER                              
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NERESALL3                                                      
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NERESF5D                                                       
         EJECT                                                                  
*              LOCAL STORAGE                                                    
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   OVWORK                                                           
REPAVE   DS    CL1                                                              
DETS     DS    CL40                                                             
NDETS    DS    CL1                                                              
WDETS    DS    CL1                                                              
UNIVOPT  DS    F                                                                
SPACOPT  DS    CL1                                                              
ROWOPT   DS    CL1                                                              
HOMEOPT  DS    F                                                                
SAVBMKT  DS    CL7                                                              
QRTOPT   DS    CL1                                                              
DPOPT    DS    CL1                                                              
AVOPT    DS    CL1                                                              
TOPOPT   DS    F                                                                
MINOPT   DS    F                                                                
MAXOPT   DS    F                                                                
SURVOPT  DS    CL1                                                              
MINCOL   DS    CL1                                                              
MAXCOL   DS    CL1                                                              
OVEROPT  DS    CL1                                                              
FFTNOPT  DS    CL1                                                              
SOLOOPT  DS    CL1                                                              
EFFOPT   DS    CL1                                                              
SCHEMOPT DS    CL1                                                              
HUT52OPT DS    CL1                                                              
TPOPT    DS    CL1                                                              
SEPOPT   DS    CL1                                                              
NOVOPT   DS    CL1                                                              
HUTOVER  DS    H                                                                
TYPEOPT  DS    CL1                                                              
TRACEOPT DS    CL1                                                              
EVERYOPT DS    CL1                                                              
NAVE     DS    CL1                                                              
DOWNOPT  DS    CL1                                                              
DPTOPT   DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE SPGENPROGA                                                     
       ++INCLUDE NEGETNUND                                                      
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
*              DSECT FOR RESEARCH SYSTEM                                        
         SPACE 3                                                                
RESD     DSECT                                                                  
TRIM     DS    CL1                                                              
COLUMNS  DS    CL8                                                              
WORKBOOK DS    CL2                                                              
AVSW     DS    CL1                                                              
SORTFRST DS    CL1                                                              
         SPACE 1                                                                
AHOOK    DS    A                   ATYPES                                       
ASTACK   DS    A                                                                
ADTALIST DS    A                                                                
AGETSORT DS    A                                                                
APRGBUFF DS    A                                                                
         DS    CL20                                                             
         SPACE 1                                                                
RELO     DS    A                                                                
SPDEMUP  DS    V                                                                
SIDIO    DS    A                                                                
SAVESTAK DS    2F                                                               
EQSW     DS    CL1                                                              
BOOKDISP DS    A                                                                
DEMDISP  DS    A                                                                
RANK     DS    F                                                                
RECSTART DS    A                                                                
ARANK    DS    A                                                                
ANYRANK  DS    CL1                                                              
ROWSW    DS    CL1                                                              
MONDATE  DS    CL6                                                              
         EJECT                                                                  
*              SORT RECORD ETC                                                  
         SPACE 3                                                                
         DS    0D                                                               
SORTREC  DS    0CL448                                                           
SORTKEY  DS    CL(MAXFLEN*10)                                                   
SORTCNT  DS    CL4                                                              
SORTWT   DS    CL4                                                              
SORTDCNT DS    CL4                                                              
SORTDWT  DS    CL4                                                              
SORTWCST DS    CL4                                                              
         DS    CL12                                                             
SORTDEMS DS    CL256                                                            
SORTJUNK DS    CL240                                                            
         SPACE 3                                                                
LASTREC  DS    0CL448                                                           
LASTKEY  DS    CL(MAXFLEN*10)                                                   
LASTCNT  DS    CL4                                                              
         DS    CL28                                                             
LASTDEMS DS    CL256                                                            
LASTJUNK DS    CL240                                                            
         SPACE 3                                                                
DROPREC  DS    0CL448                                                           
DROPKEY  DS    CL(MAXFLEN*10)                                                   
DROPCNT  DS    CL4                                                              
         DS    CL28                                                             
DROPDEMS DS    CL256                                                            
DROPJUNK DS    CL240                                                            
         EJECT                                                                  
DISP     DS    F                                                                
LSORT    DS    F                                                                
LSORTKEY DS    F                                                                
NUMCOLS  DS    F                                                                
ACTNDEM  DS    CL1                                                              
AVDEMOS  DS    CL97                                                             
UPDEMOS  DS    CL193                                                            
EXDEMOS  DS    CL193                                                            
NEXDEMS  DS    CL1                                                              
SOUTHEAD DS    F                                                                
MINMAX   DS    CL1                                                              
MULT     DS    CL1                                                              
MINTWO   DS    F                                                                
MAXTWO   DS    F                                                                
NLINES   DS    F                                                                
THISBOOK DS    CL3                                                              
THISSIDP DS    CL1                                                              
HEADP    DS    CL132                                                            
SAVER1   DS    F                                                                
SAVER3   DS    F                                                                
SAVER5   DS    F                                                                
SAVERF   DS    F                                                                
PROGKEY  DS    0CL16               PROGRAM KEY                                  
PROGKSTA DS    CL5                                                              
PROGKDAY DS    CL1                                                              
PROGKTIM DS    CL4                                                              
PROGKBK  DS    CL2                                                              
         DS    CL4                                                              
         SPACE 1                                                                
         DS    0D                                                               
MYRANK   DS    F                                                                
MYTIE    DS    F                                                                
MYVALUE  DS    F                                                                
MYAPRANK DS    A                                                                
AVETOTAD DS    A                                                                
SAVFUNCT DS    CL1                                                              
SHARCALC DS    CL1                                                              
NETPTR   DS    D                                                                
SVNETPTR DS    D                                                                
SAVDBLOK DS    CL256                                                            
SYNSAVE  DS    CL760               SAVE AREA FOR 'ZZZ S' REQUEST                
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'157NERES15   05/01/02'                                      
         END                                                                    
