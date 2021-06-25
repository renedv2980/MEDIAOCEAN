*          DATA SET SPREPLR41  AT LEVEL 030 AS OF 05/01/02                      
         TITLE 'CREATE 2-YR PUT AVG OR SEASONAL ADJ FACTORS'                    
*PHASE SPLR02B,+0                                                               
*INCLUDE DEMTIME                                                                
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
SPLR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPLR02,R3,RR=R5                                                
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
*                                                                               
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPLRRA                                                     
         ST    R5,RELO                                                          
         L     R9,ACOMFACS                                                      
         USING COMFACSD,R9                                                      
*                                                                               
         L     R1,=A(YYBUFF)                                                    
         ST    R1,AYYBUFF                                                       
         L     R1,=A(BK2BUF)                                                    
         ST    R1,ABK2BUF                                                       
         L     R1,=A(SVMKTBL)                                                   
         ST    R1,ASVMKT                                                        
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    RQFST                                                            
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
*RQFST - FIRST TIME IN FOR REQUEST                                              
**********************************************************************          
*                                                                               
RQFST    DS    0H                                                               
         XC    NMKTS(24),NMKTS                                                  
         OPEN  (OUT,(OUTPUT))                                                   
         MVI   FORCEHED,C'Y'                                                    
         L     RE,ASVMKT                                                        
         L     RF,=F'10000'                                                     
         XCEF                                                                   
*                                                                               
         PACK  DUB,QBOOK1(2)       QBOOK1 = START YY &  END YY                  
         CVB   RE,DUB                                                           
         STC   RE,STYY             START YEAR                                   
         LA    RE,1(RE)            END YY                                       
         STC   RE,ENDYY            END YEAR                                     
*                                                                               
         LA    RE,DEMOLST                                                       
         XC    DEMOLST,DEMOLST                                                  
         LA    R1,DEMOTBL          BUILD DEMOLIST TO PASS DEMOUT                
RQF10    CLI   0(R1),X'FF'                                                      
         BE    RQF15                                                            
         MVI   1(RE),C'P'                                                       
         MVC   2(1,RE),1(R1)       MOVE IN DEMO#                                
         LA    R1,L'DEMOTBL(R1)                                                 
         LA    RE,3(RE)            NEXT DEMO IN DEMOLST                         
         B     RQF10                                                            
RQF15    MVI   0(RE),X'FF'                                                      
         L     R4,ADBLOCK          GET LIST MKTS FOR END YY NOV BK              
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBFUNCT,DBGETMKB                                                 
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELSRC,C'N'       SRC = NEILSEN                                
         MVC   DBSELBK(1),ENDYY    BLD MKT LIST OFF END YY                      
         MVI   DBSELBK+1,X'0B'     AND NOV BK                                   
         L     RE,ADBUY            ADBUY = I/O AREA                             
         ST    RE,DBAREC                                                        
         GOTO1 DEMAND,DMCB,ADBLOCK,SVMRKT                                       
         L     R1,NMKTS                                                         
         LTR   R1,R1                                                            
         BZ    EXIT                NO MKTS FOURND FOR BOOK                      
*        MVC   MKTCTR,NMKTS        LOOP THRU MKTS IN TABLE                      
         SPACE 2                                                                
*                                                                               
**********************************************************************          
*LOOPS -  MKTS         - SVMKT TABLE                                            
*           DAY        - M-F/SAT/SUN                                            
*            *TIME     - EASTERN OR CNTL TABLE                                  
*               SWEEP  - SWEEP TABLE                                            
*                 YEAR - END/START                                              
*      *RECORD RELEASED UPON CHG OF TIME IN TIME LOOP                           
**********************************************************************          
                                                                                
*     1) --- MKT LOOP -----                                                     
         XC    MKTCTR,MKTCTR                                                    
MKTLP    L     R1,MKTCTR                                                        
         LTR   R1,R1                                                            
         BZ    MKTLP5                                                           
         MH    R1,=Y(SVMKTQ)                                                    
MKTLP5   L     R6,ASVMKT                                                        
         AR    R6,R1                                                            
         USING SVMKTD,R6                                                        
         MVC   MKTNUM,SVMKTNUM                                                  
         OC    MKTNUM,MKTNUM                                                    
         BZ    RQFRX                                                            
         DROP  R6                                                               
*PRINT YY                                                                       
*        MVC   P(9),=C'START YY:'                                               
*        GOTO1 HEXOUT,DMCB,STYY,P+10,1                                          
*        GOTO1 REPORT                                                           
*        MVC   P(9),=C'END   YY:'                                               
*        GOTO1 HEXOUT,DMCB,ENDYY,P+10,1                                         
*        GOTO1 REPORT                                                           
*        GOTO1 REPORT                                                           
*TEST                                                                           
*        MVC   MKTNUM,=H'101'                                                   
*TEST                                                                           
*                                                                               
         MVC   HALF,MKTNUM         FIND TIME ZONE FROM MKT #                    
         LH    RF,HALF             SET ATIMTBL TO EASTER OR CNTL TBL            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         STC   RF,HALF                                                          
         LA    R1,TIMESTN          ASSUME EASTERN TIME                          
         CLI   HALF,2                                                           
         BNE   *+8                                                              
         LA    R1,TIMCNTL          SET TO CNTL/MTN TIME                         
         CLI   HALF,3                                                           
         BNE   *+8                                                              
         LA    R1,TIMCNTL          SET TO CNTL/MTN TIME                         
         ST    R1,ATIMTBL                                                       
                                                                                
*   2) --- DAY LOOP -----                                                       
         LA    R6,DAYTBL           DAYS LOOP                                    
         ST    R6,DAYPTR                                                        
DAYLP    DS    0H                                                               
         L     R6,DAYPTR                                                        
         MVC   DAY,0(R6)           DAY BIT FOR DMD                              
         MVC   KDAY,2(R6)          DAY FOR KEY IS CHAR                          
                                                                                
*   3) --- TIME LOOP -----                                                      
         L     R6,ATIMTBL          TIMES LOOP                                   
         ST    R6,TIMPTR                                                        
TIMELP   DS    0H                  TIMES LOOP                                   
         MVI   PNT,C'N'                                                         
         MVC   STIME,2(R6)         MILITARY TIME FOR DMAND                      
         MVC   ETIME,4(R6)                                                      
         GOTO1 =V(HRTOQH),DMCB,STIME,STQHR                                      
         GOTO1 =V(HRTOQH),DMCB,ETIME,ENQHR                                      
         L     RE,AYYBUFF          INIT BUFFER FOR YEAR                         
         LH    RF,=Y(L'YYBUFF)                                                  
         XCEF                                                                   
                                                                                
*   4) --- SWEEP LOOP ----                                                      
         LA    R6,SWPTBL           SWEEP LOOP                                   
         ST    R6,SWPPTR                                                        
SWPLP    DS    0H                                                               
         MVC   SETBK+1(1),0(R6)     DO END MONTH FIRST                          
         MVC   SETBK(1),ENDYY                                                   
         L     R4,ADBLOCK          R4 -> DBLOCK                                 
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELSRC,C'N'       SRC = NEILSEN                                
         L     RE,ADBUY            GET A MKT TOTAL RECD                         
         ST    RE,DBAREC                                                        
         MVC   DBSELBK,SETBK                                                    
         MVC   DBSELRMK,MKTNUM                                                  
         MVC   DBSELDAY,DAY                                                     
         MVC   DBSELTIM(2),STIME                                                
         MVC   DBSELTIM+2(2),ETIME                                              
         MVI   DBFUNCT,DBGETTOT                                                 
         L     R1,ABK2BUF                                                       
         XC    0(L'BK2BUF,R1),0(R1)                                             
         MVI   HKCNT,0                                                          
         GOTO1 DEMAND,DMCB,ADBLOCK,DEMHK       GET END YY DEMOS FIRST           
         OC    DBDIVSOR,DBDIVSOR                                                
         BZ    SWPLPX              NO DATA FOR END YY, BYPASS SWEEP             
         MVC   BK2DIV,DBDIVSOR                                                  
         MVC   DBSELBK(1),STYY     GET START YR DEMOS FOR SWEEP                 
         GOTO1 DEMAND,DMCB,ADBLOCK,DEMHK    NOW DO START YY                     
         CLI   DBERROR,X'10'       START YEAR NOT FOUND?                        
         BE    SWLP30              IT'S OKAY                                    
         CLI   DBERROR,X'80'       EOF                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         ICM   R1,3,DBDIVSOR                                                    
         AH    R1,BK2DIV                                                        
         STH   R1,BK2DIV                                                        
*                                                                               
SWLP30   OC    BK2DIV,BK2DIV       IF ZERO, NOTHING TO SLOT                     
         BZ    *+8                                                              
         BAS   RE,SLOTBK           SLOT 2YR AVG BK IN FULL YY BUFFER            
*                                                                               
SWPLPX   L     R6,SWPPTR           NEXT SWEEP                                   
         LA    R6,L'SWPTBL(R6)                                                  
         ST    R6,SWPPTR                                                        
         CLI   0(R6),X'FF'         DONE WITH ALL SWEEP BOOKS?                   
         BNE   SWPLP                                                            
*                                                                               
         BAS   RE,BLDREC           BLD AND RELS DEMO RECS FOR DAYPT             
         L     R6,TIMPTR                                                        
         LA    R6,TIMEQ(R6)        NEXT TIME FOR DAY                            
         ST    R6,TIMPTR                                                        
         CLI   0(R6),X'FF'                                                      
         BNE   TIMELP              LOOP TO DO NEXT TIME PART FOR DAY            
*                                                                               
         L     R6,DAYPTR           BUMP DAY IN DAY LOOP                         
         LA    R6,L'DAYTBL(R6)                                                  
         ST    R6,DAYPTR                                                        
         CLI   0(R6),X'FF'         DO ALL DAYS                                  
         BNE   DAYLP                                                            
*                                                                               
MKTLPX   L     R1,MKTCTR                                                        
         LA    R1,1(R1)                                                         
         ST    R1,MKTCTR                                                        
         CLC   MKTCTR,NMKTS                                                     
*TEST                                                                           
*        B     RQFRX                                                            
*TEST                                                                           
         BE    RQFRX                                                            
         B     MKTLP               NEXT MKT                                     
*                                                                               
RQFRX    DS    0H                                                               
                                                                                
         CLOSE (OUT)                                                            
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
*SVMRKT -  DEMAND HOOK TO SLOT MKTS INTO SVMKTBL                                
**********************************************************************          
         DS    0D                                                               
         USING *,RF                                                             
SVMRKT   NTR1  BASE=SPLRRB         SAVE MKT# AND NAME FROM MKT RECD             
         LM    RA,RC,SPLRRA                                                     
         DROP  RF                                                               
         L     R4,ADBLOCK                                                       
         L     R5,DBAREC                                                        
         USING DMKEY,R5                                                         
         L     R6,ASVMKT                                                        
         USING SVMKTD,R6                                                        
SVMRKT2  OC    SVMKTNUM,SVMKTNUM   NEXT AVAIL SLOT IN TBL?                      
         BZ    SVMRKT3                                                          
         CLC   DMRMKT,SVMKTNUM     ALREADY GOT THIS MKT?                        
         BE    EXIT                                                             
         LA    R6,SVMKTQ(R6)       NEXT MKT ENTRY IN TABLE                      
         B     SVMRKT2                                                          
*                                                                               
SVMRKT3  L     R1,NMKTS                                                         
         LA    R1,1(R1)                                                         
         ST    R1,NMKTS                                                         
         MVC   SVMKTNUM,DMRMKT     MKT #                                        
         LA    R5,DMFRSTEL                                                      
         USING DMELEM,R5                                                        
         ZIC   R1,DMLEN                                                         
         SH    R1,=H'5'                                                         
         LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVMKTNAM(0),DMMNAME   SAVE MKT NAME                              
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* BLDREC - BLD DEMHR RECD AND RELEASE                                           
**********************************************************************          
BLDREC   NTR1                                                                   
         XC    MYKEY,MYKEY         KEY TEMPLATE USED W/ALL DEMOS                
         LA    R6,MYKEY                                                         
         USING SVID,R6                                                          
         MVC   SVIMED(3),=C'TNH'   FOR 94-95 AVG USE TNH                        
         MVI   SVIACT,C'A'         ACTION ADD                                   
         SR    RE,RE               MKT                                          
         LH    RE,MKTNUM                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVIMKT,DUB                                                       
         MVC   SVIDAY,KDAY         DAY                                          
         ZIC   RE,STQHR            START QHR                                    
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVISQHR,DUB                                                      
         ZIC   RE,ENQHR            END QHR                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVIEQHR,DUB                                                      
         LA    R7,1                R7=DEMO CTR                                  
*                                                                               
BLD10    DS    0H                                                               
         LA    R6,RECIO                                                         
         LR    RE,R6               CLEAR BUFFER W/EA NEW DEMO CTGY              
         L     RF,=F'2000'                                                      
         XCEF                                                                   
         MVC   SVIKEY(SVIKEYLN),MYKEY                                           
         CVD   R7,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVIDEMO,DUB                                                      
         LA    R6,SVIMONS                                                       
         BAS   RE,INDEX            INDX TO NOV,SLOT AS CHAR VAL IN REC          
                                                                                
*---------------------------------------                                        
*        LR    RE,R7               RE=DEMO NUMBER                               
*        BCTR  RE,0                                                             
*        SLL   RE,2                4BYTE BUCKETS IN BUFFER                      
*        A     RE,AYYBUFF          RE -> 1ST MONTHS DEMO VALUE                  
*        LA    R1,12               12 MONTHS TO DO                              
*LD20    SR    RF,RF                                                            
*        L     RF,0(RE)                                                         
*        CVD   RF,DUB                                                           
*        OI    DUB+7,X'0F'                                                      
*        UNPK  1(3,R6),DUB                                                      
*        LA    RE,YYMMQ(RE)        DISP TO SAME DEMO, NEXT MONTH                
*        LA    R6,4(R6)            NEXT MM IN RECD                              
*        BCT   R1,BLD20                                                         
*---------------------------------------------------------------                
                                                                                
         MVC   RECIOLN,=Y(SVIRECQ)                                              
         LA    R6,RECIO                                                         
         L     R5,=A(OUT)                                                       
         PUT   (R5),(R6)                                                        
*                                                                               
         CLI   PNT,C'Y'                                                         
         BNE   BLD30                                                            
         MVC   P(80),RECIO                                                      
         GOTO1 REPORT                                                           
         MVC   P,SPACES                                                         
         GOTO1 HEXOUT,DMCB,RECIO,P,24       PRINT RECORD                        
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,RECIO+24,P+18,48     PRINT RECORD                    
         GOTO1 REPORT                                                           
         LA    RE,P+18                                                          
         LA    RF,RECIO+24                                                      
         LA    R1,12                                                            
BLD25    EDIT  (4,(RF)),(8,(RE)),ZERO=NOBLANK                                   
         LA    RE,8(RE)            NEXT OUTPUT LINE                             
         LA    RF,4(RF)            NEXT YY                                      
         BCT   R1,BLD25                                                         
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVI   PNT,C'N'            ONLY PRINT ONE PER TIME                      
*                                                                               
BLD30    DS    0H                                                               
         LA    R7,1(R7)            BLD AND RELS NEXT DEMO                       
         CH    R7,=H'8'                                                         
         BE    BLD30               DON'T RELEASE METROA                         
         CH    R7,=Y(NDEMOS)       PROCESS ALL DEMOS                            
         BNH   BLD10                                                            
*                                                                               
BLDRECX  B     XIT                                                              
*                                                                               
**********************************************************************          
*INDEX  - CONVERT RAW DEMO VALUES TO INDICES OFF THE NOV BOOK                   
*     INPUT -  R6 -> SVIMONS IN KEY (OUTPUT AREA)                               
*              R7 = DEMO NUMBER BEING PROCESSED                                 
*     OUTPUT - MONTHS IN KEY WILL HOLD INDEXED VALUES IN CHAR FORM              
**********************************************************************          
INDEX    NTR1                                                                   
         BCTR  R7,0                DEMO NUMBER BEING PROCESSED                  
         SLL   R7,2                4BYTE BUCKETS IN BUFFER                      
         A     R7,AYYBUFF          R7 -> 1ST MONTHS DEMO VALUE                  
*                                                                               
         LA    R1,10*YYMMQ         DISP TO NOV                                  
         AR    R1,R7                                                            
         ICM   R0,15,0(R1)         R0 = NOV DEMO VALUE                          
         LTR   R0,R0                                                            
         BZ    INDEXZER            BASE IS ZERO                                 
*                                                                               
         SR    RE,RE                                                            
         LA    R1,12               R0=NOV VALUE R1=MM LP  R7->INPUT DEM         
INDEX2   ICM   RF,15,0(R7)         RAW HUT VALUE IN RF                          
         M     RE,=F'100'          X 100                                        
         SLDA  RE,1                X 2 FOR ROUNDING                             
         DR    RE,R0               DIVIDE BY BASE(NOV) VALUE                    
         AH    RF,=H'1'                                                         
         SRA   RF,1                ROUND                                        
         CH    RF,=H'255'          INDICES ARE ABOVE 255                        
         BL    *+8                                                              
         LH    RF,=H'100'           SET TO 100                                  
         CH    RF,=H'25'           INDICES BELOW 25                             
         BH    *+8                                                              
         LA    RF,25                SET TO 25                                   
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         SLOT AS DECIMAL INTO DUB                     
         UNPK  1(3,R6),DUB                                                      
         LA    R6,4(R6)            NEXT MM IN KEY                               
         LA    R7,YYMMQ(R7)        POINT TO NEXT RAW VALUE                      
         BCT   R1,INDEX2                                                        
         B     INDEXOUT                                                         
*                                                                               
INDEXZER LA    R1,12               FOR A BASE VALUE OF ZERO                     
         MVC   1(3,R6),=C'100'                                                  
         LA    R6,4(R6)                                                         
         BCT   R1,*-10                                                          
         B     INDEXOUT                                                         
*                                                                               
INDEXOUT B     XIT                                                              
         SPACE 2                                                                
*                                                                               
**********************************************************************          
* DEMHK  - PICK OFF THE DEMOS WE NEED SUM AND SAVE THEM IN BK2BUFF              
**********************************************************************          
         DS    0D                                                               
         USING *,RF                                                             
DEMHK    NTR1  BASE=SPLRRB                                                      
         LM    RA,RC,SPLRRA                                                     
         DROP  RF                                                               
         ZIC   R1,HKCNT            NUMBER OF TIMES IN HK THIS CALL              
         LA    R1,1(R1)                                                         
         STC   R1,HKCNT                                                         
*                                                                               
         XC    DEMOVLS,DEMOVLS                                                  
         L     R5,DBAREC           PULL DEMOS FROM RECD                         
         USING DRKEY,R5                                                         
         L     R4,ADBLOCK                                                       
         GOTO1 CDEMOUT,DMCB,(C'L',DEMOLST),DBLOCKD,DEMOVLS                      
         MVC   DBNUMVLS,=Y(NDEMOS)                                              
         OC    DBFACTOR,DBFACTOR                                                
         BZ    *+8                                                              
         BAS   RE,DEMTH            MULT DEMOS BY DBFACT & ADD TO BK2BUF         
*                                                                               
DEMHKX   DS    0H                                                               
         OC    DBDIVSOR,DBDIVSOR                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*CALCULATIONS - MULTIPLY DEMOS IN DEMOLIST BY DBFACTOR.                         
*        R0 = DBNUMVLS (NUMBER DEMOS IN DEMOLIST)                               
*        R6 = ACUMBUFF (SUM OF RECORDS IN DEMAND CALL)                          
*        R7 = VALUE BEING PROCESSED                                             
*        RE/RF = WORK REGS                                                      
**********************************************************************          
DEMTH    NTR1                                                                   
         LA    R0,NDEMOS          R0=NUMBER OF DEMOS IN DEMOLIST                
         SR    R7,R7                                                            
         ICM   R7,3,DBFACTOR                                                    
         LA    R5,DEMOVLS          VALUES GOTTEN FROM DEMOUT                    
         L     R6,ABK2BUF          WHERE TO BK2BUF RCD VALUES                   
*                                                                               
DEMTH2   ICM   RF,15,0(R5)         RF=DEMO VALUE                                
         LTR   RF,RF               VALUE IN WORK AREA                           
         BZ    DEMTH5                                                           
         BP    *+10                                                             
         SR    RF,RF               IF NEG OR ZERO, BYPASS MULTIPLY              
         B     DEMTH5                                                           
         MR    RE,R7               MULTIPLY VALUE BY DBFACTOR                   
*                                                                               
DEMTH5   ICM   R1,15,0(R6)         ADD: NEW RECD TO BK2BUF                      
         AR    RF,R1                                                            
*                                                                               
DEMTH8   ST    RF,0(R6)            SAVE RESULT                                  
         LA    R2,5(R2)            POINT TO NEXT DISPL TAB ENTRY                
         LA    R5,4(R5)            BUMP TO NEXT FIELD                           
         LA    R6,4(R6)            BUMP BK2BUF BUFFER                           
         BCT   R0,DEMTH2           LOOP THRU DBNUMVLS                           
*                                                                               
DEMTHX   DS      0H                                                             
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*SLOTBK - FIRST DIVIDE DEMOS IN BK2BUF BY BK2DIV (DBDIVSOR)                     
*         SLOT AS 2-BTYE DEMOS INTO YYBUFF PER SWEEP TABLE                      
**********************************************************************          
SLOTBK   NTR1                                                                   
         LA    R0,NDEMOS                                                        
         L     RF,SWPPTR                                                        
         ZIC   R1,1(RF)            FIRST BK TO SLOT INTO                        
         BCTR  R1,0                                                             
         MH    R1,=Y(NDEMOS*4)     DISP TO MONTH IN YYBUFF                      
         A     R1,AYYBUFF          R1 -> 1ST MM TO FILL IN                      
         ST    R1,DUB              LATER, COPY FROM HERE TO OTHER MM'S          
*                                                                               
         L     R6,ABK2BUF                                                       
         LH    R7,BK2DIV                                                        
SLT10    L     RF,0(R6)                                                         
         MH    RF,=H'10'           *10                                          
         SR    RE,RE                                                            
         DR    RE,R7               DIVIDE BY DIVSOR                             
         AH    RF,=H'5'            +5                                           
         SR    RE,RE                                                            
         D     RE,=F'10'           /10 ...ROUND                                 
*                                                                               
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)            NEXT DEMO IN BUFFER                          
         LA    R6,4(R6)                                                         
         BCT   R0,SLT10            DO NEXT DEMO                                 
*                                                                               
         L     RF,SWPPTR           SLOT INTO OTHER MONTHS?                      
         CLI   2(RF),0                                                          
         BE    SLOTBKX             NO MORE TO DO                                
         ZIC   R1,2(RF)                                                         
         BCTR  R1,0                                                             
         MH    R1,=Y(NDEMOS*4)     DISP TO MONTH IN YYBUFF                      
         A     R1,AYYBUFF          R1 -> NXT MM TO FILL IN                      
         L     R6,DUB              COPY FROM 1ST MM TO OTHER MM'S               
         MVC   0(NDEMOS*4,R1),0(R6)                                             
*                                                                               
         CLI   3(RF),0             COPY INTO ANOTHER MONTH                      
         BE    SLOTBKX             NO MORE TO DO                                
         ZIC   R1,3(RF)                                                         
         BCTR  R1,0                                                             
         MH    R1,=Y(NDEMOS*4)     DISP TO MONTH IN YYBUFF                      
         A     R1,AYYBUFF          R1 -> NXT MM TO FILL IN                      
         MVC   0(NDEMOS*4,R1),0(R6)                                             
*                                                                               
SLOTBKX  B     XIT                                                              
**********************************************************************          
* HEADER FOR PRINTING REPT                                                      
**********************************************************************          
         DS    0D                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=SPLRRB                                                      
         LM    RA,RC,SPLRRA                                                     
         DROP  RF                                                               
*        MVC   H6+64(7),DNAME1                                                  
         CLI   QOPT1,C'N'                                                       
         BE    MYHEADX                                                          
         MVC   H6+40(4),=C'    '                                                
         MVC   H7+40(4),=C'    '                                                
         MVC   H8+40(4),=C'    '                                                
         MVC   H6+47(4),=C'ADI '                                                
MYHEADX  B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* PRINT REPORT                                                                  
**********************************************************************          
PREP     NTR1                                                                   
*                                                                               
*        EDIT  (RF),(11,PLHMSUNV),,COMMAS=YES                                   
*        GOTO1 REPORT                                                           
*                                                                               
PREPX    B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*LTORG                                                                          
*----------------------------------------------------------------------         
         LTORG                                                                  
         SPACE 2                                                                
SPLRRA DC      F'0'                                                             
SPLRRB DC      F'0'                                                             
SPLRRC DC      F'0'                                                             
         SPACE 2                                                                
*---------------------------------------------------------------------          
*DAY AND TIME TABLES                                                            
*---------------------------------------------------------------------          
DAYTBL   DS    0XL5                      1ST BYTE = DAYBIT FOR DEMAND           
         DC    X'02',X'60',CL3'SAT'                                             
         DC    X'01',X'70',CL3'SUN'                                             
         DC    X'95',X'95',CL3'M-F'                                             
         DC    X'FF'                                                            
DAYTBLQ  EQU   *-DAYTBL                                                         
         SPACE 3                                                                
                                                                                
SWPTBL   DS    0XL4                  SWEEP BK FOLLOWED BY BKS IT ALSO           
         DC    AL1(02,01,02,03)      APPLIES TO                                 
         DC    AL1(05,04,05,06)      MAJOR SWEEPS ARE :FEB,MAY,JUL,NOV          
         DC    AL1(07,07,08,09)                                                 
         DC    AL1(11,10,11,12)                                                 
         DC    AL1(01,01,00,00)      EXTRA SWEEPS: JAN                          
         DC    AL1(03,03,00,00)                    MAR                          
         DC    AL1(10,10,00,00)                    OCT                          
         DC    X'FF'                                                            
                                                                                
*                                                                               
TIMESTN  DS    0XL6                   EASTERN TIMES (NEPTAB)                    
         DC    AL1(23),X'17',AL2(900),AL2(1200)                                 
         DC    AL1(27),X'17',AL2(1200),AL2(1600)                                
         DC    AL1(31),X'17',AL2(1600),AL2(1800)                                
         DC    AL1(36),X'17',AL2(1800),AL2(1930)                                
         DC    AL1(40),X'17',AL2(1930),AL2(2000)                                
         DC    AL1(43),X'17',AL2(2300),AL2(2330)                                
         DC    AL1(45),X'17',AL2(2330),AL2(0100)                                
         DC    AL1(03),X'17',AL2(2000),AL2(2100)                                
         DC    AL1(06),X'17',AL2(2100),AL2(2300)                                
         DC    X'FF'                                                            
*                                                                               
TIMCNTL  DS    0XL6                   CENTRAL TIMES  (NCPTAB)                   
         DC    AL1(23),X'17',AL2(900),AL2(1200)                                 
         DC    AL1(27),X'17',AL2(1200),AL2(1500)                                
         DC    AL1(31),X'17',AL2(1500),AL2(1700)                                
         DC    AL1(36),X'17',AL2(1700),AL2(1830)                                
         DC    AL1(40),X'17',AL2(1830),AL2(1900)                                
         DC    AL1(43),X'17',AL2(2200),AL2(2230)                                
         DC    AL1(45),X'17',AL2(2230),AL2(0100)                                
         DC    AL1(03),X'17',AL2(1900),AL2(2000)                                
         DC    AL1(06),X'17',AL2(2000),AL2(2200)                                
         DC    X'FF'                                                            
*                                                                               
TIMEQ    EQU   L'TIMCNTL              QHR CODE FOR KEY,ST-END TIMES             
*                                                                               
DEMOTBL  DS    0XL9                          KEY#, DEMO#, DEMONAME              
         DC    AL1(01),AL1(001),C'HOMES  '                                      
         DC    AL1(02),AL1(045),C'WOMEN  '                                      
         DC    AL1(03),AL1(042),C'W1849  '                                      
         DC    AL1(04),AL1(095),C'MEN    '                                      
         DC    AL1(05),AL1(092),C'M1849  '                                      
         DC    AL1(06),AL1(125),C'TEENS  '                                      
         DC    AL1(07),AL1(122),C'CH2-11 '                                      
         DC    AL1(08),AL1(002),C'METROA '                                      
         DC    AL1(09),AL1(129),C'V1234  '                                      
         DC    AL1(10),AL1(048),C'W2554  '                                      
         DC    AL1(11),AL1(098),C'M2554  '                                      
         DC    AL1(12),AL1(123),C'CH6-11 '                                      
         DC    AL1(13),AL1(142),C'A1849  '                                      
         DC    AL1(14),AL1(141),C'A1834  '                                      
         DC    X'FF'                                                            
*                                                                               
NDEMOS   EQU   14                                                               
*                                                                               
         EJECT                                                                  
RELO     DC    F'0'                                                             
NMKTS    DC    F'0'                                                             
MKTCTR   DC    F'0'                                                             
HKCNT    DC    X'00'                                                            
TST      DC    X'00'                                                            
OCOUNT   DC    F'00'                                                            
PNT      DS    X                                                                
DAYPTR   DS    F                                                                
TIMPTR   DS    F                                                                
EORPTR   DS    F                                                                
SWPPTR   DS    F                                                                
MYKEY    DS    CL24                                                             
ENDYY    DS    X                                                                
STYY     DS    X                                                                
SETBK    DS    XL2                                                              
BK2DIV   DS    H                                                                
ATIMTBL  DS    F                                                                
*                                                                               
AYYBUFF  DS    A                                                                
ABK2BUF  DS    A                                                                
ASVMKT   DS    A                                                                
*                                                                               
         DS    0H                                                               
MKTNUM   DS    CL2                 MKT CURRENTLY BEING PROCESSED                
DAY      DS    CL1                 DAY   "         "      "                     
STQHR    DS    CL1                 START QHR (1/2HR) CODE FOR KEY               
ENQHR    DS    CL1                 END QHR (1/2HR) CODE FOR KEY                 
STIME    DS    CL2                 START TIME "    "      "                     
ETIME    DS    CL2                 END TIME        "      "                     
KDAY     DS    CL3                 DAY FOR KEY     "      "                     
*                                                                               
DEMOLST  DS    XL45                UP TO 15 DEMOS                               
DEMOVLS  DS    XL60                UP TO 15 DEMOS                               
*                                                                               
OFORMAT  DS    0XL10                                                            
OFILE    DS    CL3                                                              
OMED     DS    CL1                                                              
OINTFIL  DS    CL1                                                              
OINTMED  DS    CL1                                                              
OSOURCE  DS    CL1                                                              
OBOOK    DS    XL2                                                              
OFILTER  DS    XL1                                                              
*                                                                               
         DS    0F                                                               
RECIOLN  DS    F                                                                
RECIO    DS    2000F                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'*BK2BUF*'                                                    
BK2BUF   DS    XL(NDEMOS*4)        ONE SWEEP 2-YR AVG                           
*                                                                               
         DS    0D                                                               
         DC    CL8'*YYBUFF*'                                                    
YYBUFF   DS    XL(12*NDEMOS*4)     AVGD BKS FOR ENTIRE YR                       
         DS    0D                                                               
         DC    CL8'*SVMKTS*'                                                    
SVMKTBL  DS    CL10000                                                          
*                                                                               
YYMMQ    EQU   NDEMOS*4            DISP TO NEXT MONTH IN BUFFER                 
*                                                                               
         DS    0D                                                               
OUT      DCB   DDNAME=OUT,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=0080,                                             X        
               BLKSIZE=8000,                                           X        
               MACRF=PM                                                         
*                                                                               
                                                                                
SVID     DSECT                                                                  
SVIKEY   DS    0C                                                               
SVIMED   DS    CL1                 MEDIA                                        
SVIRAT   DS    CL1                 SERVICE                                      
SVISRC   DS    CL1                 SOURCE                                       
         DS    CL1                                                              
SVIMKT   DS    CL4                 MARKET                                       
         DS    CL1                                                              
SVIDAY   DS    CL3                 DAY M-F,SAT,SUN                              
         DS    CL1                                                              
SVISQHR  DS    CL2                 START QH                                     
         DS    CL1                                                              
SVIEQHR  DS    CL2                 END QH                                       
         DS    CL1                                                              
SVIDEMO  DS    CL2                 AUDIENCE TYPE                                
         DS    CL1                                                              
SVIACT   DS    CL1                 ACTION CODE                                  
         DS    CL1                                                              
SVIKEYLN EQU   *-SVIKEY                                                         
SVIMONS  DS    CL48                MONTHS                                       
         DS    CL8                 SPARE                                        
SVIRECQ  EQU   *-SVID              LENGTH OF RECD                               
*                                                                               
SVMKTD DSECT                                                                    
SVMKTNUM DS    CL2                                                              
SVMKTNAM DS    CL30                                                             
SVMKTQ   EQU   *-SVMKTD                                                         
         SPACE 2                                                                
         SPACE 2                                                                
HOMETABD DSECT                                                                  
HSTA     DS    CL5                                                              
HMKT     DS    CL2                                                              
HOMTABEN DS    0C                                                               
HOMELEN  EQU   HOMTABEN-HSTA                                                    
         SPACE 2                                                                
         EJECT                                                                  
PLINE    DSECT                                                                  
         DS    CL1                                                              
PLMNUM   DS    CL4                                                              
         DS    CL3                                                              
PLMNAME  DS    CL30                                                             
         DS    CL3                                                              
PLMRANK  DS    CL3                                                              
         DS    CL3                                                              
PLSRANK  DS    CL3                                                              
         DS    CL2                                                              
PLUSPCT  DS    CL6                                                              
         DS    CL1                                                              
PLHMSUNV DS    CL11                                                             
         SPACE 2                                                                
PSLINE   DSECT                                                                  
         DS    CL3                                                              
PSSTA    DS    CL4                                                              
         DS    CL2                                                              
PSMKTNUM DS    CL4                                                              
         DS    CL2                                                              
PSMKTNAM DS    CL30                                                             
         DS    CL4                                                              
PSBOOKS  DS    CL80                                                             
         EJECT                                                                  
*                                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDEMFILE                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030SPREPLR41 05/01/02'                                      
         END                                                                    
