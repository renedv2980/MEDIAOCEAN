*          DATA SET SPMIS02    AT LEVEL 060 AS OF 11/19/19                      
*PHASE T20B02C                                                                  
*==========================================================                     
* 23AUG06 MHER LOCKED GOAL TITLE                                                
*   JUN06 MHER MORE SPOTLENGTHS                                                 
*   JAN04 MHER SUPPORT 2-DECIMAL RATINGS                                        
* 01JUL03 AWIL GRIDS                                                            
* 04MAY01 MHER SUPPORT STATION OR MARKET LOCKIN                                 
*==========================================================                     
         TITLE 'T20B02 - MIS SCREEN DISPLAY MODULE'                             
T20B02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20B02                                                         
         USING GENOLD,RC                                                        
         L     RC,0(R1)            WORK                                         
         USING T20BFFD,RA          TWA                                          
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T20B02+4096,R9                                                   
                                                                                
*===================================================================            
* GET EQUIVALENCE BASE LENGTH TO DISPLAY                                        
*===================================================================            
                                                                                
         XC    MISEQU,MISEQU       CLEAR EQUIV DISPLAY                          
         FOUT  MISEQUH                                                          
*                                                                               
         LHI   RE,30*2                                                          
         A     RE,VSLNTAB          POINT TO 30 SEC ENTRY                        
         SR    R1,R1                                                            
         IC    R1,0(RE)            GET 30 SEC DSPL                              
         LA    RE,EQUSECT1(R1)     POINT TO 30 SEC FACTOR                       
*                                                                               
         LHI   RF,60*2                                                          
         A     RF,VSLNTAB          POINT TO 60 SEC ENTRY                        
         SR    R1,R1                                                            
         IC    R1,0(RF)            GET 60 SEC DSPL                              
         LA    RF,EQUSECT1(R1)     POINT TO 60 SEC FACTOR                       
*                                                                               
         CLC   0(2,RE),0(RF)       IF 30/60 FACTORS EQUAL, NO EQUIV             
         BE    B8                                                               
*                                                                               
         LHI   R0,30                                                            
         CLC   0(2,RE),=H'1000'    IS 30 SEC 1000                               
         BE    B6                                                               
*                                                                               
         LHI   R0,60                                                            
         CLC   0(2,RF),=H'1000'                                                 
         BE    B6                                                               
*                                                                               
         L     R1,VSLNTAB                                                       
         AHI   R1,2                POINT TO ENTRY FOR 1 SECOND SPOTS            
         LHI   R0,1                SET FIRST SLN                                
*                                                                               
B2       CLM   R0,1,1(R1)          IS THIS A BASIC SLN                          
         BNE   B4                                                               
         SR    RE,RE                                                            
         IC    RE,0(R1)            GET DSPL TO EQUIV FACTOR                     
         LA    RE,EQUSECT1(RE)     POINT TO ENTRY FOR THIS SLN                  
         CLC   0(2,RE),=H'1000'    IS IT THE BASE                               
         BE    B6                                                               
*                                                                               
B4       AHI   R1,2                NEXT TABLE ENTRY                             
         AHI   R0,1                NEXT SLN                                     
         CHI   R0,250                                                           
         BNH   B2                                                               
         DC    H'0'                                                             
*                                                                               
B6       FOUT  MISEQUH,=CL13'EQVBAS=   (+)'                                     
         EDIT  (R0),(3,MISEQU+7)                                                
*                                                                               
B8       TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    B150                . YES, SKIP MAINFRAME HEADINGS               
*                                                                               
B10      CLI   FORMIND,C'D'        DEMO DISPLAY?                                
         BE    B100                                                             
         SPACE 1                                                                
* LOCKIN DISPLAY                                                                
         TM    SVOPT2,SVOPT2_MLK+SVOPT2_SLK  ANY LOCKIN REQUESTED ?             
         BZ    B90                                                              
         XC    MISL8(L'MISL8),MISL8                                             
         XC    MISL9(L'MISL9),MISL9                                             
         MVC   MISL8(L'FOR1HDG1),FOR1HDG1                                       
         MVI   MISL8+12,C'-'                                                    
         MVC   MISL8+13(12),MISL8+12                                            
         MVC   MISL8+15(6),=C'LOCKIN'                                           
         MVC   MISL8+54(9),=C'LKIN/PRCH'                                        
*                                                                               
         MVC   MISL9(L'FOR1HDG2),FOR1HDG2                                       
         FOUT  MISL8H                                                           
         FOUT  MISL9H                                                           
         B     B140                                                             
* GOAL V. PURCHASED *                                                           
         SPACE 1                                                                
B90      CLI   SVDEMNMS,C'R'       TEST RATING                                  
         BE    B91                                                              
         CLI   SVDEMNMS,C'E'       OR EXTENDED RATING                           
         BE    B91                                                              
         MVC   FOR1CPPG(61),FOR1H1A      MOVE CPM/IMPS TITLE                    
B91      MVC   FOR1DEM+9(7),SVDEMNMS     FIRST DEMO TITLE                       
*                                                                               
B92      FOUT  MISL8H,FOR1HDG1                                                  
         FOUT  MISL9H,FOR1HDG2                                                  
*                                                                               
         CLC   MISFOR(3),=C'LVP'   TEST GOAL LOCKIN REQUEST                     
         BNE   B140                                                             
         MVC   MISL8+12(4),=C'LKGL'                                             
*                                                                               
         B     B140                                                             
         SPACE 1                                                                
* PUT DEMOGRAPHIC HEADING *                                                     
         SPACE 1                                                                
B100     XC    MISL8(L'MISL8),MISL8                                             
         XC    MISL9(L'MISL9),MISL9                                             
         MVI   MISL8+17,C'-'                                                    
         MVC   MISL8+18(59),MISL8+17                                            
*                                                                               
         MVC   MISL8+41(12),=C'DEMOGRAPHICS'                                    
         FOUT  MISL8H                                                           
*                                                                               
         GOTO1 DEMOHL,DMCB,MISL9   FORMAT DEMO HEADLINE                         
         FOUT  MISL9H                                                           
*                                                                               
B140     XC    MISPFK,MISPFK                                                    
         MVC   MISPFK(15),=C'PF1=Top, 3=Next'                                   
         OI    MISPFKH+6,X'80'     TRANSMIT IT                                  
*                                                                               
B150     XC    LINECTR,LINECTR                                                  
         LA    R2,MISL11H          1ST TWA DATA FIELD                           
         MVI   BYTE4,1             PAGE CTR                                     
*                                                                               
         USING MISBUCKS,R6                                                      
         L     R6,ABUCKETS                                                      
         MVC   LASTDPT,BUCDPTCD    SAVE 1ST DAYPART                             
*                                                                               
         CLC   MISSTA(4),=C'LIST'  STATION LIST OPTION?                         
         BE    B154                                                             
         CLC   MISPRD(3),=C'ALL'                                                
         BNE   B156                                                             
*                                                                               
B154     BAS   RE,BRAND            DISPLAY BRANDS ON EACH LINE                  
         B     ENDFOUT                                                          
*                                                                               
B156     CLI   WEEKIND,C'W'        WEEKS REQUESTED?                             
         BNE   READ1                                                            
*                                                                               
         BAS   RE,WEEKS            DISPLAY WEEKS                                
ENDFOUT  BAS   RE,FOUTTWA          FOUT NON-ZERO FIELDS                         
         MVC   SVGLCTR,GLCTR       SAVE LINE COUNTER                            
*                                                                               
EQXIT    CR    RB,RB               SET CC =                                     
         J     EXIT                                                             
NEQXIT   LTR   RB,RB               SET CC NOT =                                 
EXIT     XIT1                                                                   
*                                                                               
*                                  BUILD TOTAL LINE                             
DTOTAL   TM    PCDRIVEN,PCGRIDQ    GRIDS                                        
         BZ    DTOT1                                                            
         LA    R4,WORK2                                                         
         XC    WORK2,WORK2                                                      
         B     *+8                                                              
DTOT1    LA    R4,8(R2)            OUTPUT LINE                                  
*                                                                               
         GOTO1 ,DMCB,TOTACC,(255,(R4))  TOTAL DISPLAY PARAMETERS                
*                                                                               
         MVC   0(8,R4),=C'TOTAL*  '                                             
*                                                                               
         MVI   BYTE,C'*'           SET FOR TOTALS                               
         CLI   SAVLEN,0            ALL LENGTHS?                                 
         BNE   *+8                                                              
         MVI   BYTE2,X'FF'         SET FOR EQUIV                                
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    DTOT3                                                            
         GOTO1 VFORMGRD,DMCB,GCLTOT,(R6),TOTACC                                 
         BNE   ENDFOUT                                                          
         GOTO1 VFORMGRD,DMCB,GFEOR                                              
         BNE   ENDFOUT                                                          
         B     *+8                                                              
DTOT3    BAS   RE,FORMAT                                                        
         B     ENDFOUT                                                          
*                                                                               
READBUFF SR    R4,R4               READ BUFFER ELEMENT                          
         IC    R4,BUCELLEN         ELEM LENGTH                                  
         LA    R6,0(R4,R6)         NEXT BUCKET                                  
*                                                                               
READ1    CLI   BUCCODE,0           LAST ELEMENT?                                
         BE    B220                                                             
*                                                                               
         CLC   BUCPRD,=C'ZZZ'      POL?                                         
         BNE   *+10                                                             
         MVC   BUCPRD,=C'POL'      ZZZ IS FOR BUCKET SORT                       
*                                                                               
         CLI   PIGCODE,0           WAS PIGGYBACK REQUESTED                      
         BNE   B170                YES                                          
*                                                                               
         CLC   BUCPRD,MISPRD       REQUESTED PRODUCT?                           
         BNE   READBUFF                                                         
*                                                                               
B170     CLI   SAVLEN,0            ALL LENGTHS REQUESTED?                       
         BE    B172                                                             
         CLC   SAVLEN,BUCLEN       SAME LENGTH?                                 
         BNE   READBUFF            NO                                           
*                                                                               
B172     CLI   SAVDPTCD,0          ALL DAYPARTS REQUESTED?                      
         BE    B174                YES                                          
         CLC   SAVDPTCD,BUCDPTCD   MATCH REQUESTED DAYPART                      
         BNE   READBUFF            NO - NEXT BUCKET                             
*                                                                               
B174     SR    R0,R0                                                            
         IC    R0,LASTDPT                                                       
         SR    R1,R1                                                            
         IC    R1,BUCDPTCD                                                      
*                                                                               
         CR    R0,R1               MATCH ON DAYPARTS                            
         BE    B300                YES                                          
         CLI   SAVDPTCD,0          ALL DAYPARTS REQUESTED?                      
         BNE   B220                YES, DIPLAY TOTAL                            
         SRL   R0,4                                                             
         SRL   R1,4                                                             
         CR    R0,R1               MATCH ON MASTER DPT                          
         BNE   B220                NO, DISPLAY TOTAL                            
         CHI   R0,0                IS THE MASTER DPT 0                          
         BE    B220                YES, DISPLAY TOTAL                           
         MVC   LASTDPT,BUCDPTCD    SAVE DAYPART CODE                            
         MVI   HALF,C'Y'                                                        
         B     B300                                                             
*                                                                               
* NEW DAYPART - DISPLAY DAYPART TOTAL LINE *                                    
                                                                                
B220     CLI   LINECTR+1,1         LINES DISPLAYED                              
         BL    B275                                                             
         BH    *+14                                                             
* 1 LINE                                                                        
         CLC   HALF2,=H'1000'      EQUIV FACTOR FOR PREVIOUS LINE               
         BE    B275                                                             
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    B222                                                             
         LA    R4,WORK2                                                         
         XC    WORK2,WORK2                                                      
         B     *+8                                                              
B222     LA    R4,8(R2)            OUTPUT AREA                                  
*                                                                               
         MVI   BYTE,C'*'           SET TOTAL IND                                
         GOTOR DAYPART,DMCB,LASTDPT,(R4)                                        
*                                                                               
         GOTO1 ,DMCB,DPTACC,(255,(R4))  FORMAT LINE                             
*                                                                               
         CLI   SAVLEN,0            ALL LENGTHS?                                 
         BNE   *+8                                                              
         MVI   BYTE2,X'FF'         SET FOR EQUIV                                
         MVC   3(4,R4),=C'-TOT'                                                 
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    B230                                                             
         GOTO1 VFORMGRD,DMCB,GCLSUB,(R6),DPTACC                                 
         BNE   ENDFOUT                                                          
         B     *+8                                                              
B230     BAS   RE,FORMAT                                                        
*                                                                               
         MVC   3(4,R4),=C'-TOT'                                                 
* GET NEXT LINE                                                                 
         CLC   SVXFRCTL,=C'SPONWS'  TEST CALL FROM SPOT/NWS                     
         BE    B275                 YES - JUST GET TOTALS                       
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    B275                                                             
         GOTOR NEXTLINE,DMCB,(R2)                                               
         L     R2,DMCB                                                          
         LTR   R2,R2               REQUESTED PAGE?                              
         BZ    ENDFOUT                                                          
*                                                                               
B275     MVC   LASTDPT,BUCDPTCD    SAVE DAYPART CODE                            
         CLI   BUCCODE,0           LAST BUCKET?                                 
         BE    DTOTAL                                                           
*                                                                               
         MVI   BYTE,0              CLEAR TOTAL INDICATOR                        
         XC    LINECTR,LINECTR                                                  
*                                                                               
         XC    DPTACC(52),DPTACC   DAYPART COUNTERS                             
         SPACE 1                                                                
* LINE DISPLAY *                                                                
         SPACE 1                                                                
*                                                                               
B300     TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    B310                                                             
         LA    R4,WORK2                                                         
         B     *+8                                                              
B310     LA    R4,8(R2)            OUTPUT AREA                                  
*                                                                               
         MVI   NODSPLY,0           RESET SWITCH                                 
         CLI   HALF,C'Y'           DISPLAY DAYPART?                             
         BE    B312                                                             
         CLI   LINECTR+1,0                                                      
         BNE   B320                                                             
B312     XC    WORK2,WORK2                                                      
         XC    HALF,HALF                                                        
*                                                                               
         GOTOR DAYPART,DMCB,BUCDPTCD,(R4)   DAYPART LITERAL                     
         BE    B320                                                             
         MVI   NODSPLY,C'X'                                                     
         B     B330                                                             
*                                                                               
B320     TM    PCDRIVEN,PCGRIDQ    GRIDS?                                       
         BO    B330                KEEP LENGTH AND DAYPART SEPERATE             
*                                                                               
         MVI   3(R4),C'-'                                                       
         EDIT  BUCLEN,(3,4(R4))    LENGTH                                       
*                                                                               
         CLI   BUCLEN,99                                                        
         BH    *+14                                                             
         MVC   4(2,R4),5(R4)                                                    
         MVI   6(R4),C' '                                                       
*                                                                               
B330     GOTO1 ROUND,DMCB,BUCGDOLS,2    ELIMINATE PENNIES                       
*                                                                               
         MVC   LINACC(ACCLEN2Q),BUCPSPTS   BUILD LINE ACCUMULATORS              
         XC    LINACC+ACCLEN2Q(ACCLEN3Q),LINACC+ACCLEN2Q                        
         GOTO1 EQUIV,DMCB,(BUCLEN,LINACC+ACCLEN1Q),                    X        
               (BUCDPTCD,LINACC+ACCLEN2Q),ACCNUM1Q,HALF2                        
         CLI   NODSPLY,0                                                        
         BNE   B350                                                             
         GOTO1 ,DMCB,LINACC,(R4)   OUTPUT PARAMETERS                            
*                                                                               
         MVI   BYTE,0              SET TOTAL IND OFF                            
         MVI   BYTE2,0             TURN EQUIV OFF                               
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    B344                                                             
         GOTO1 VFORMGRD,DMCB,GCLDET,(R6),LINACC                                 
         BNE   ENDFOUT                                                          
         B     B346                                                             
*                                                                               
B344     BAS   RE,FORMAT                                                        
*                                                                               
         GOTOR NEXTLINE,DMCB,(R2)  GET NEXT LINE                                
         L     R2,DMCB                                                          
         LTR   R2,R2               REQUESTED PAGE?                              
         BZ    ENDFOUT                                                          
*                                                                               
B346     LH    R4,LINECTR          LENGTH CTR                                   
         LA    R4,1(R4)                                                         
         STH   R4,LINECTR                                                       
*                                                                               
B350     DS    0H                                                               
         GOTO1 ADDCTRS,DMCB,LINACC,DPTACC,ACCNUM2Q ADD TO DAYPART CTRS          
*                                                                               
         GOTO1 ADDCTRS,DMCB,LINACC,TOTACC,ACCNUM2Q ADD TO TOTAL CTRS            
*                                                                               
         B     READBUFF            GET NEXT BUFFER ELEMENT                      
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------------------         
*        WEEK DISPLAY ROUTINE *                                                 
*----------------------------------------------------------------------         
WEEKS    NTR1                                                                   
*                                                                               
         LA    R7,MONDATES         MONDAY DATES                                 
         LA    R8,MISL11H          LINE 11                                      
         L     R6,ABUCKETS                                                      
         CLI   MONDATES+26,0       14 WEEKS?                                    
         BE    *+8                                                              
         LA    R8,MISL10H          LINE 10                                      
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    *+10                                                             
         MVC   MISL9(6),=CL6'WEEK'                                              
*                                                                               
         CLI   BUCCODE,0                                                        
         BE    WKTOTAL                                                          
*                                                                               
W100     CLI   BUCCODE,0           LAST BUCKET?                                 
         BE    WEEKPRT                                                          
*                                                                               
         CLC   BUCDATE,0(R7)       SAME DATE?                                   
         BNE   WEEKPRT                                                          
*                                                                               
         CLI   SAVDPTCD,0          REQUEST DAYPART                              
         BE    *+14                                                             
*                                                                               
         CLC   BUCDPTCD,SAVDPTCD   DAYPART MATCH?                               
         BNE   W300                                                             
*                                                                               
         CLI   SAVLEN,0            REQUEST LENGTH                               
         BE    *+14                                                             
*                                                                               
         CLC   BUCLEN,SAVLEN       LENGTH MATCH?                                
         BNE   W300                                                             
*                                                                               
         GOTO1 ROUND,DMCB,BUCGDOLS,2    ELIMINATE PENNIES                       
*                                                                               
         MVC   DPTACC(ACCLEN2Q),BUCPSPTS     BUILD WORK ACCUMULATORS            
         XC    DPTACC+ACCLEN2Q(ACCLEN3Q),DPTACC+ACCLEN2Q                        
*                                                                               
         GOTO1 EQUIV,DMCB,(BUCLEN,DPTACC+ACCLEN1Q),                    X        
               (BUCDPTCD,DPTACC+ACCLEN2Q),ACCNUM1Q,HALF2                        
*                                                                               
         GOTO1 ADDCTRS,DMCB,DPTACC,LINACC,ACCNUM2Q                              
*                                                                               
W300     SR    RE,RE               GET NEXT BUCKET                              
         IC    RE,BUCELLEN         BUCKET LENGTH                                
         LA    R6,0(RE,R6)         NEXT BUCKET                                  
         B     W100                                                             
         EJECT                                                                  
*        -----------------------                                                
*        DISPLAY WEEK PRINT LINE                                                
*        -----------------------                                                
WEEKPRT  DS    0H                                                               
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    W350                                                             
*                                                                               
         GOTO1 VDATCON,DMCB,(2,(R7)),(4,8(R8)) MMMDD FORMAT                     
*                                                                               
W350     GOTO1 ,DMCB,LINACC,8(R8)       FORMAT PARAMETERS                       
*                                                                               
         MVI   BYTE,0              SET TOTAL IND OFF                            
         MVI   BYTE2,0             SET EQUIV OFF                                
         CLI   SAVLEN,0            ALL LENGTHS REQUESTED?                       
         BNE   *+12                                                             
         MVI   BYTE,C'+'           EQUIVALENCE IND                              
         MVI   BYTE2,X'FF'                                                      
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    W400                                                             
         GOTO1 VFORMGRD,DMCB,GCLDET,(R6),LINACC,(R7)                            
         BNE   WEEKXIT                                                          
         B     *+8                                                              
W400     BAS   RE,FORMAT                                                        
*                                                                               
         GOTO1 ADDCTRS,DMCB,LINACC,TOTACC,ACCNUM2Q   ADD TO TOTALS              
         XC    LINACC(56),LINACC                                                
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    W450                                                             
*                                                                               
         GOTOR NEXTLINE,DMCB,(R8)  GET NEXT DISPLAY LINE                        
         L     R8,DMCB                                                          
         LTR   R8,R8               REQUESTED PAGE?                              
         BZ    WEEKXIT                                                          
*                                                                               
W450     LA    R7,2(R7)            NEXT WEEK                                    
         CLI   0(R7),0             LAST WEEK?                                   
         BE    WKTOTAL                                                          
         B     W100                                                             
*                                                                               
WKTOTAL  TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    W500                                                             
         XC    WORK22,WORK22                                                    
         MVC   WORK22(7),=C'TOTAL* '                                            
         B     *+10                                                             
W500     MVC   8(7,R8),=C'TOTAL* '                                              
*                                                                               
         GOTO1 ,DMCB,TOTACC,(1,8(R8))   TOTAL DISPLAY                           
*                                                                               
         MVI   BYTE,C'*'           TOTALS IND                                   
         MVI   BYTE2,0             TURN EQUIV OFF                               
         CLI   SAVLEN,0            ALL LENGTHS REQUESTED?                       
         BNE   *+8                                                              
         MVI   BYTE2,X'FF'                                                      
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    W550                                                             
         GOTO1 VFORMGRD,DMCB,GCLTOT,(R6),TOTACC                                 
         BNE   WEEKXIT                                                          
         GOTO1 VFORMGRD,DMCB,GFEOR                                              
         BNE   WEEKXIT                                                          
         B     *+8                                                              
W550     BAS   RE,FORMAT                                                        
*                                                                               
WEEKXIT  B    EXIT                                                              
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------------------         
*        BRAND DISPLAY ROUTINE                                                  
*----------------------------------------------------------------------         
BRAND    NTR1                                                                   
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    BRD60                                                            
*                                                                               
         LA    R8,MISL10H          OUTPUT LINE-1 (MISL11 IS FIRST)              
         MVC   MISL9(6),=CL6'BRAND'                                             
         CLC   MISSTA(4),=C'LIST'    STATION LIST OPTION?                       
         BNE   *+10                                                             
         MVC   MISL9(7),=C'STATION'                                             
*                                                                               
BRD60    L     R6,ABUCKETS                                                      
         MVI   BYTE3,0             DISPLAY LINE IND                             
*                                                                               
         XC    WORK2,WORK2                                                      
*                                                                               
BRD100   CLI   BUCCODE,0           LAST BUCKET?                                 
         BNE   BRD150                                                           
* LAST                                                                          
         CLI   BYTE3,0             DISPLAY LINE?                                
         BNE   BRD300              DISPLAY                                      
*                                                                               
         B     BRANDXIT            EXIT                                         
*                                                                               
BRD150   CLC   BUCPRD,=C'ZZZ'      POL? (SORT ORDER-POL IS LAST)                
         BNE   *+10                                                             
         MVC   BUCPRD,=C'POL'                                                   
*                                                                               
         CLI   SAVDPTCD,0          ALL DAYPARTS REQUESTED?                      
         BE    *+14                                                             
         CLC   SAVDPTCD,BUCDPTCD   DAYPARTS                                     
         BNE   BRD200                                                           
*                                                                               
         CLI   SAVLEN,0            ALL LENGTHS REQUESTED?                       
         BE    *+14                                                             
         CLC   SAVLEN,BUCLEN       LENGTHS                                      
         BNE   BRD200                                                           
*                                                                               
         CLC   WORK2(3),BUCPRD     NEW PRODUCT?                                 
         BNE   BRD170                                                           
         CLC   WORK2+3(3),BUCPRD2  NEW PIGGYBACK PRODUCT?                       
         BE    *+12                                                             
*                                                                               
*        NEW PRD                                                                
BRD170   CLI   BYTE3,0             DISPLAY LINE?                                
         BNE   BRD300                                                           
         SPACE 1                                                                
* ADD BUCKET TO LINE ACCUMULATORS                                               
         SPACE 1                                                                
BRD175   DS    0H                                                               
         MVC   WORK2(3),BUCPRD     SAVE PRODUCT                                 
         MVC   WORK2+3(3),BUCPRD2  SAVE PIGGYBACK PRODUCT                       
*                                                                               
         GOTO1 ROUND,DMCB,BUCGDOLS,2    ELIM. PENNIES                           
*                                                                               
*        BUILD ACCUMULATORS                                                     
         MVC   DPTACC(ACCLEN2Q),BUCPSPTS                                        
         XC    DPTACC+ACCLEN2Q(ACCLEN3Q),DPTACC+ACCLEN2Q                        
* EQUIVALENCE COUNTERS                                                          
         GOTO1 EQUIV,DMCB,(BUCLEN,DPTACC+ACCLEN1Q),                    X        
               (BUCDPTCD,DPTACC+ACCLEN2Q),ACCNUM1Q,HALF2                        
*                                                                               
         GOTO1 ADDCTRS,DMCB,DPTACC,LINACC,ACCNUM2Q ADD TO LINE ACCUM            
*                                                                               
         MVI   BYTE3,X'FF'         DISPLAY LINE IND                             
*                                                                               
BRD200   SR    RE,RE               GET NEXT BUCKET                              
         IC    RE,BUCELLEN         BUCKET LENGTH                                
         LA    R6,0(RE,R6)         NEXT BUCKET                                  
         B     BRD100                                                           
         EJECT                                                                  
* DISPLAY BRAND LINE                                                            
         SPACE 1                                                                
BRD300   TM    PCDRIVEN,PCGRIDQ                                                 
         BO    BRD302                                                           
         GOTOR NEXTLINE,DMCB,(R8)                                               
         ICM   R8,15,DMCB          REQUESTED PAGE                               
         BZ    BRANDXIT                                                         
*                                                                               
BRD302   CLC   MISSTA(4),=C'LIST'  TEST STATION LIST OPTION                     
         BNE   BRD305                                                           
         CLC   WORK2(3),MISPRD     TEST THIS IS REALLY A PRD                    
         BE    BRD405                                                           
         B     BRD320                                                           
*                                                                               
* DISPLAY BRAND LINE                                                            
*                                                                               
BRD305   XC    WORK22,WORK22                                                    
         MVC   WORK22(3),WORK2       PRD                                        
         CLC   WORK22(3),=C'POL'                                                
         BE    BRD400                                                           
         CLC   WORK2+3(3),=XL3'000000'                                          
         BE    BRD400                                                           
*                                                                               
         LA    R5,WORK22                                                        
         LA    RE,3                                                             
BRD310   CLI   0(R5),X'40'                                                      
         BE    BRD315                                                           
         CLI   0(R5),X'00'                                                      
         BE    BRD315                                                           
         LA    R5,1(R5)                                                         
         BCT   RE,BRD310                                                        
*                                                                               
BRD315   MVI   0(R5),C'-'                                                       
         MVC   1(3,R5),WORK2+3                                                  
         B     BRD400                                                           
*                                                                               
* STATION LIST OPTION                                                           
*                                                                               
BRD320   CLC   WORK2(3),=X'000001' FIRST STATION - GOALS?                       
         BNE   BRD375                                                           
         MVC   WORK22(5),=C'GOALS'                                              
         B     BRD400                                                           
         SPACE 1                                                                
* STATION OPTION PRETENDS PRODUCT IS STATION                                    
         SPACE 1                                                                
BRD375   LA    R1,WORK3                                                         
         USING STAPACKD,R1                                                      
         XC    0(32,R1),0(R1)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPCTRY,SVAGPRF7                                                
         MVC   STAPMED,MISMED                                                   
         MVC   STAPACOM,VCOMFACS                                                
         MVC   STAPMKST+2(3),WORK2   MOVE STATION (MKT=0)                       
*                                                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK22(8),STAPQSTA                                               
         CLI   STAPQSTA,C'0'                                                    
         BL    BRD390                                                           
         CLI   STAPQSTA+4,C' '                                                  
         BE    BRD380                                                           
         CLI   STAPQSTA+4,C'/'     CABLE                                        
         BNE   BRD400                                                           
BRD380   XC    WORK22+5(3),WORK22+5    SKIP BLANK SPACE                         
         MVC   WORK22+4(3),STAPQSTA+5  SKIP BLANK SPACE                         
*                                                                               
BRD390   CLI   MISMED,C'N'         TEST NETWORK                                 
         BNE   BRD400                                                           
         CLI   SVAGPRF7,C'C'       TEST CANADA                                  
         BNE   BRD400                                                           
         CLI   STAPMKST+4,X'B0'    TEST CANADIAN CABLE                          
         BNL   BRD395                                                           
         SR    RE,RE                                                            
         IC    RE,STAPMKST+4       GET NETWORK SEQNUM                           
         BCTR  RE,0                                                             
         SLL   RE,2                 X 4                                         
         A     RE,ANETLST                                                       
         MVI   WORK22+4,C'/'                                                    
         MVC   WORK22+5(4),0(RE)      SHOW NETWORK ID                           
*                                                                               
BRD395   MVI   NOGOALS,C'Y'        SUPPRESS GOAL DISPLAY                        
         DROP  R1                                                               
*                                                                               
BRD400   TM    PCDRIVEN,PCGRIDQ                                                 
         BO    *+10                                                             
         MVC   8(8,R8),WORK22                                                   
*                                                                               
         GOTO1 ,DMCB,LINACC,8(R8)                                               
*                                                                               
         MVI   BYTE,0                                                           
         MVI   BYTE2,0                                                          
         CLI   SAVLEN,0            ALL LENGTHS?                                 
         BNE   *+8                                                              
         MVI   BYTE2,X'FF'         EQUIV(+) IND                                 
         CLC   BUCPRD,=C'POL'                                                   
         BNE   *+8                                                              
         MVI   BYTE,1              *******                                      
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    BRD402                                                           
         GOTO1 VFORMGRD,DMCB,GCLDET,(R6),LINACC                                 
         BNE   BRANDXIT                                                         
         B     *+8                                                              
BRD402   BAS   RE,FORMAT                                                        
*                                                                               
         GOTO1 ADDCTRS,DMCB,LINACC,TOTACC,ACCNUM2Q                              
*                                                                               
BRD405   XC    LINACC(56),LINACC                                                
*                                                                               
         MVI   BYTE3,0             LINE DISPLAY IND                             
         CLI   BUCCODE,0           LAST BUCKET                                  
         BNE   BRD175                                                           
* STATION LIST OPTION?                                                          
         CLC   MISSTA(4),=C'LIST'                                               
         BNE   BRANDX                                                           
* DISPLAY STATION LIST TOTALS                                                   
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    BRD410                                                           
         OC    8(8,R8),8(R8)       ANY DATA THIS LINE ?                         
         BZ    BRD410              NO                                           
         GOTOR NEXTLINE,DMCB,(R8)                                               
         L     R8,DMCB             NEXT LINE                                    
         LTR   R8,R8               FINISHED?                                    
         BZ    BRANDXIT                                                         
*                                                                               
BRD410   B     WKTOTAL             DO STATION LIST TOTALS                       
*                                                                               
BRANDX   TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    BRANDXIT                                                         
         GOTO1 VFORMGRD,DMCB,GFEOR                                              
*                                                                               
BRANDXIT XIT1                                                                   
         EJECT                                                                  
* FOUT TWA LINES WITH DATA                                                      
*                                                                               
FOUTTWA  NTR1                                                                   
         LA    R8,MISL10H                                                       
         SR    RE,RE                                                            
FOUT100  OC    8(L'MISL11,R8),8(R8)     DATA?                                   
         BZ    *+8                                                              
*                                                                               
         FOUT  (R8)                                                             
         IC    RE,0(R8)                 FIELD LENGTH                            
         LA    R8,0(RE,R8)              NEXT FIELD                              
         CLI   0(R8),0                  LAST?                                   
         BNE   FOUT100                                                          
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    EXIT                                                             
         EDIT  (1,BYTE4),(2,MISPAGE),ALIGN=LEFT                                 
         STC   R0,MISPAGEH+5                                                    
         FOUT  MISPAGEH                                                         
         B     EXIT                                                             
* ROUTINE TO ADD COUNTERS                                                       
*        P1=A(INPUT FULL WORD CTRS)                                             
*        P2=A(OUTPUT CTRS)                                                      
*        P3=NUMBER OF COUNTERS                                                  
         SPACE 1                                                                
ADDCTRS  NTR1                                                                   
         L     R8,8(R1)            BCT                                          
*                                                                               
         L     R2,0(R1)            INPUT                                        
         L     R3,4(R1)            OUTPUT                                       
*                                                                               
ADD100   TM    0(R2),X'80'         TEST NEGATIVE                                
         BO    ADD102                                                           
         TM    0(R3),X'80'                                                      
         BO    ADD102                                                           
         B     ADD104                                                           
*                                                                               
ADD102   L     RE,0(R2)                                                         
         A     RE,0(R3)                                                         
         ST    RE,0(R3)                                                         
         B     ADD110                                                           
*                                                                               
ADD104   L     RE,0(R2)            INPUT CTR                                    
         N     RE,=X'3FFFFFFF'     DROP 2-DEC FLAG                              
         LTR   RE,RE                                                            
         BZ    ADD110                                                           
         L     RF,0(R3)                                                         
         N     RF,=X'3FFFFFFF'                                                  
         AR    RE,RF                                                            
         ST    RE,0(R3)                                                         
         TM    0(R2),X'40'                                                      
         BZ    *+8                                                              
         OI    0(R3),X'40'                                                      
*                                                                               
ADD110   LA    R2,4(R2)            NEXT CTR                                     
         LA    R3,4(R3)                                                         
*                                                                               
         BCT   R8,ADD100                                                        
         B     EXIT                                                             
         EJECT                                                                  
* ROUND ACCUMULATOR TO ELIMINATE PENNIES AND DECIMAL                            
* P1=A(ACCUMULATORS)  (BYTE 0 = TYPE INDICATOR)                                 
*                      X'00'=DOLLARS-ELIM PENNIES)                              
*                      X'FF'=DEMO-ELIM 1 DECIMAL)                               
* P2=NUMBER OF FULL WORD COUNTERS                                               
         SPACE 1                                                                
ROUND    NTR1                                                                   
         L     R8,4(R1)            BCT                                          
*                                                                               
         L     R2,0(R1)            A(ACCUMULATORS)                              
         CLI   0(R1),X'FF'         DEMO?                                        
         BE    ROUND200                                                         
*                                                                               
ROUND100 L     R4,0(R2)            DOLLARS                                      
         SRDA  R4,31                                                            
         D     R4,=F'100'                                                       
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AHI   R5,1                                                             
         SRA   R5,1                                                             
*                                                                               
         ST    R5,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R8,ROUND100                                                      
         B     EXIT                                                             
*                                                                               
ROUND200 L     R5,0(R2)                                                         
         N     R5,=X'3FFFFFFF'                                                  
         M     R4,=F'2'                                                         
         LHI   R0,10                                                            
         TM    0(R2),X'40'                                                      
         BZ    *+8                                                              
         LHI   R0,100                                                           
         DR    R4,R0                                                            
         AHI   R5,1                                                             
         SRL   R5,1                                                             
*                                                                               
         ST    R5,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R8,ROUND200                                                      
         B     EXIT                                                             
         EJECT                                                                  
* EQUIVALENCE ROUTINE                                                           
*   P1=A(UNEQUIVALENCED COUNTERS)(BYTE 0=COMMERCIAL LENGTH)                     
*   P2=A(OUTPUT-EQUIVALENCED COUNTERS)  (BYTE 0=DAYPART CODE)                   
*   P3=NUMBER OF FULL WORD COUNTERS                                             
*   P4=A(HALF-WORD FOR EQUIVALENCE FACTOR)                                      
         SPACE 1                                                                
EQUIV    NTR1                                                                   
                                                                                
*                                                                               
EQUIV100 SR    RE,RE                                                            
         IC    RE,0(R1)            GET SLN                                      
         AR    RE,RE               X 2                                          
         A     RE,VSLNTAB          POINT TO ENTRY FOR THIS SLN                  
*                                                                               
         LA    R3,=H'1000'         POINT TO DEFAULT FACTOR                      
         SR    RF,RF                                                            
         IC    RF,0(RE)            GET DSPL TO EQUIV FACTOR                     
         LA    R3,EQUSECT1(RF)     POINT TO FACTOR                              
                                                                                
* CALCULATE EQUIVALENCIES                                                       
                                                                                
EQUIV200 L     R0,8(R1)            BCT LOOP                                     
         L     R4,0(R1)            INPUT CTRS                                   
         L     R5,4(R1)            OUTPUT CTRS                                  
*                                                                               
EQUIV300 L     RF,0(R4)            INPUT                                        
         N     RF,=X'3FFFFFFF'     DROP 2-DEC FLAG                              
         SR    RE,RE                                                            
         ICM   RE,3,0(R3)                                                       
         BNZ   *+8                                                              
         LHI   RE,1000             USE 1000 IF FACTOR IS 0                      
         MR    RE,RE                                                            
         D     RE,=F'1000'                                                      
         CHI   RE,500                                                           
         BL    *+8                                                              
         AHI   RF,1                ROUND                                        
         ST    RF,0(R5)            EQUIVALENCED DATA                            
*                                                                               
         TM    0(R4),X'40'         TEST 2-DEC FLAG                              
         BZ    *+8                                                              
         OI    0(R5),X'40'         SET 2-DEC FLAG IN OUTPUT                     
*                                                                               
         LA    R4,4(R4)            INPUT                                        
         LA    R5,4(R5)            OUPUT                                        
         BCT   R0,EQUIV300                                                      
*                                                                               
         L     RE,12(R1)           A(EQU FACTOR OUTPUT)                         
         MVC   0(2,RE),0(R3)       P4                                           
         B     EXIT                                                             
         EJECT                                                                  
* SUBROUTINE TO BUILD DEMO HEADLINE                                             
*        P1=A(OUTPUT AREA)                                                      
DEMOHL   NTR1                                                                   
         L     R2,0(R1)                                                         
         MVC   0(15,R2),=C'DPT-LN  DOLLARS'                                     
         LA    R2,15(R2)                                                        
*                                                                               
         LA    R3,SVDEMNMS         ESTHDR DEMO DESCRIPTIONS                     
         SR    R4,R4               R4 = NUMBER OF DEMOS                         
*                                                                               
DEMOHL62 AHI   R4,1                INC 1 TO NUMBER OF DEMOS                     
         MVC   2(7,R2),0(R3)       DEMO DESC                                    
         MVC   11(3,R2),=C'CPP'                                                 
         CLI   0(R3),C'R'          RATING?                                      
         BE    DEMOHL64                                                         
         CLI   0(R3),C'E'          EXTENDED RATING ?                            
         BE    DEMOHL64                                                         
         MVI   13(R2),C'M'         CPM                                          
*                                                                               
DEMOHL64 LA    R3,7(R3)            NEXT DEMO NAME                               
         LA    R2,16(R2)           NEXT OUTPUT                                  
*                                                                               
         CLI   0(R3),0             ANY MORE DEMOS?                              
         BE    *+8                                                              
         CHI   R4,4                TOTAL OF 4 DEMOS?                            
         BL    DEMOHL62            NO                                           
         STH   R4,NUMDEMS          STORE NUMBER OF DEMOS                        
         B     EXIT                                                             
         SPACE 2                                                                
* FORMAT ROUINES FOR  GOAL V. PURCHASED DISPLAY                                 
*                 OR  DEMO DISPLAY                                              
*   0(R1) = A(ACCUMULATORS)                                                     
*   4(R1) = A(OUTPUT LINE)                                                      
*   BYTE  = CPP EQUIVALENCE INDICATOR)                                          
*   BYTE2 = X'00'=NO EQUIV-NO ASTERISKS                                         
*           X'01'=NO EQUIV-ASTERISKS                                            
*           X'FF'=EQUIV(+) AND ASTERISKS                                        
         SPACE 1                                                                
FORMAT   NTR1                                                                   
         L     R8,4(R1)            OUTPUT                                       
         USING FORMAT1D,R8         DSECT                                        
*                                                                               
         L     R2,0(R1)            ACCUMULATORS                                 
         USING ACCUMD,R2                                                        
*                                                                               
         CLI   FORMIND,C'D'        TEST DEMO DISPLAY                            
         BE    FMT100                                                           
*                                                                               
         CLI   BYTE2,X'FF'         TOTAL LINE                                   
         BE    *+12                                                             
         CLI   NOGOALS,C'Y'        TEST DO NOT DISPLAY GOALS                    
         BE    FMT5                                                             
*                                                                               
         EDIT  ACC2,(9,F1GDOLS),MINUS=YES   GOAL DOLLARS/LOCKIN DOLLARS         
         LTR   R0,R0                                                            
         BNP   *+8                                                              
         MVI   F1GDOLS+8,C'*'                                                   
         SPACE 1                                                                
* FORMAT GOAL V PURCHASED *                                                     
         SPACE 1                                                                
         L     R1,ACC4             GOAL/LOCKIN POINTS                           
         N     R1,=X'3FFFFFFF'     DROP FLAGS                                   
         TM    SVOPT2,SVOPT2_MLK   MKT LOCKIN TO 1-DEC ONLY                     
         BO    FMT1B                                                            
         TM    SVOPT2,SVOPT2_2DEC                                               
         BZ    FMT1A                                                            
         CHI   R1,10000            MAX FOR 2 DEC IS 99.99                       
         BNL   FMT1A                                                            
         EDIT  (R1),(5,F1GPNTS),2                                               
         B     FMT4                                                             
*                                                                               
FMT1A    TM    ACC4,X'40'          TEST 2-DECIMALS                              
         BZ    FMT1B                                                            
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
FMT1B    CHI   R1,10000            MAX FOR 1 DEC IS 999.9                       
         BNL   FMT2                                                             
         EDIT  (R1),(5,F1GPNTS),1                                               
         B     FMT4                                                             
*                                                                               
FMT2     M     R0,=F'2'            X 2                                          
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(5,F1GPNTS)                                                 
*                                                                               
FMT4     LTR   R1,R1                                                            
         BZ    *+8                                                              
         MVI   F1GPNTS+5,C'*'                                                   
*                                  GOAL CPP                                     
         MVC   F1GCPP+7(1),BYTE                                                 
         LA    R3,ACC4             GOAL PNTS                                    
*                                                                               
         CLI   BYTE2,X'FF'         TOTAL LINE?                                  
         BNE   *+12                                                             
         LA    R3,ACC10            GOAL POINTS-EQUIVALENCED                     
         MVI   F1GCPP+7,C'+'                                                    
*                                                                               
         GOTO1 VCPP,DMCB,(R3),ACC2,F1GCPP  GOAL CPP CALC AND DISPLAY            
*                                                                               
FMT5     L     R1,ACC5             PURCHASED POINTS                             
         N     R1,=X'3FFFFFFF'     DROP FLAG                                    
         TM    SVOPT2,SVOPT2_2DEC  DO WE WANT TO SHOW 2 DECIMALS?               
         JNZ   FMT5A               YES                                          
         TM    ACC5,X'40'          NO, PURCHASED POINTS HAS 2 DEC?              
         JZ    FMT5D                   NO, CHECK IF 1 DEC WILL FIT              
         J     FMT5C                   YES, CONVERT TO 1 DEC                    
*                                                                               
FMT5A    TM    ACC5,X'40'          DO WE HAVE A 2 DECIMAL VALUE?                
         JNZ   FMT5B               YES, NO NEED TO ADJUST                       
         MHI   R1,10               NO, ADJUST TO A 2 DECIMAL VALUE              
*                                                                               
FMT5B    CHI   R1,10000            DO WE HAVE ENOUGH ROOM TO SHOW               
         JL    FMT5G                DEMO VALUE WITH 2 DECIMALS?                 
FMT5C    M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
FMT5D    CHI   R1,10000                                                         
         BNL   FMT6                                                             
         EDIT  (R1),(5,F1PPNTS),1  1 DECIMAL OF PRECISION                       
         B     FMT8                                                             
FMT5G    EDIT  (R1),(5,F1PPNTS),2  2 DECIMALS OF PRECISION                      
         J     FMT8                                                             
*                                                                               
FMT6     M     R0,=F'2'            X 2                                          
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(5,F1PPNTS)    0 DECIMALS OF PRECISION                      
*                                                                               
FMT8     LTR   R1,R1                                                            
         BZ    *+10                                                             
         MVC   F1PPNTS+5(1),BYTE                                                
*                                                                               
         EDIT  ACC3,(8,F1PDOLS),MINUS=YES    PURCHASED DOLLARS                  
         LTR   R1,R1                                                            
         BZ    *+8                                                              
         MVI   F1PDOLS+8,C'*'                                                   
         MVC   F1PCPP+7(1),BYTE    USE * OR + AS REQUIRED                       
*                                                                               
         LA    R3,ACC5             PURCHASED POINTS                             
*                                                                               
         CLI   BYTE2,X'FF'         TOTAL LINE?                                  
         BNE   *+12                                                             
         LA    R3,ACC11            PURCHASED EQUIV POINTS                       
         MVI   F1PCPP+7,C'+'       EQUIV IND                                    
*                                                                               
         GOTO1 VCPP,DMCB,(R3),ACC3,F1PCPP    PURCH CPP CALC                     
*                                                                               
         TM    SVOPT2,SVOPT2_MLK+SVOPT2_SLK  LOCKIN DATA REQUEST ?              
         BZ    FMT10                                                            
         EDIT  ACC9,(4,F1LSPOTS)   LOCKIN SPOTS (INSTEAD OF GOAL CPP)           
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         MVI   F1LSPOTS+4,C'/'                                                  
*                                                                               
FMT10    EDIT  ACC1,(4,F1SPOTS)    SPOTS                                        
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         MVI   F1SPOTS+4,C'*'                                                   
*                                                                               
         ICM   R7,15,ACC1          SPOTS                                        
         BZ    FMT20                                                            
*                                                                               
         L     R5,ACC5             PURCH POINTS                                 
         BZ    FMT11                                                            
         N     R5,=X'3FFFFFFF'                                                  
*                                                                               
         M     R4,=F'2'            X 2                                          
         DR    R4,R7                                                            
         AHI   R5,1                                                             
         SRL   R5,1                                                             
         TM    ACC5,X'40'          TEST POINTS IN 2-DEC                         
         BZ    FMT11                                                            
         M     R4,=F'2'                                                         
         D     R4,=F'10'                                                        
         AHI   R5,1                                                             
         SRL   R5,1                                                             
*                                                                               
FMT11    CHI   R5,999              TEST ROOM TO PRINT DECIMAL                   
         BH    FMT12               NO                                           
* PRINT ONE DECIMAL PLACE                                                       
         EDIT  (R5),(4,F1AVGPTS),1  AVG PNTS PER SPOT                           
         B     FMT14                                                            
* RECALCULATE WITH NO DECIMAL PLACE                                             
*                                                                               
FMT12    L     R5,ACC5             GET POINTS                                   
         N     R5,=X'3FFFFFFF'                                                  
         M     R4,=F'2'                                                         
         LR    RE,R7               GET NUMBER OF SPOTS                          
         MHI   RE,10               X 10                                         
         TM    ACC5,X'40'          TEST 2-DEC                                   
         BZ    *+8                                                              
         MHI   RE,10               X 10 AGAIN                                   
         DR    R4,RE                                                            
         AHI   R5,1                                                             
         SRL   R5,1                                                             
         EDIT  (R5),(4,F1AVGPTS)                                                
*                                                                               
FMT14    LTR   R1,R1                                                            
         BZ    *+8                                                              
         MVI   F1AVGPTS+4,C'*'                                                  
*                                                                               
FMT20    MVI   F1PERACH+3,C'*'                                                  
*                                                                               
         GOTO1 VPRACHMT,DMCB,ACC5,ACC4,F1PERACH   PERCENT ACH-PNTS              
*                                                                               
         MVI   F1PERACH+8,C'*'                                                  
*                                                                               
         GOTO1 VPRACHMT,DMCB,ACC3,ACC2,F1PERACH+5 PERCENT ACH-DOLS              
         B     FMTX                                                             
         EJECT                                                                  
* DEMOGRAPHIC DISPLAY                                                           
         SPACE 1                                                                
FMT100   DS    0H                                                               
         L     R8,4(R1)            OUTPUT                                       
*                                                                               
         EDIT  ACC3,(7,7(R8))      DOLLARS                                      
*                                                                               
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         MVI   14(R8),C'*'                                                      
*                                                                               
         LA    R8,15(R8)           1ST DEMO OUTPUT                              
         LH    R4,NUMDEMS          BCT                                          
         LA    R5,ACC5             PURCH POINTS                                 
         LR    R7,R5                                                            
         CLI   BYTE2,X'FF'         EQUIV                                        
         BNE   *+8                                                              
         LA    R7,ACC11            PURCH EQUIV POINTS                           
*                                                                               
FMT110   MVC   15(1,R8),BYTE                                                    
         CLI   BYTE2,X'FF'                                                      
         BNE   *+8                                                              
         MVI   15(R8),C'+'                                                      
         GOTO1 VCPP,DMCB,(R7),ACC3,8(R8)   CPP PRINT                            
*                                                                               
         L     R1,0(R5)                                                         
         TM    0(R5),X'40'         TEST 2-DEC                                   
         BZ    FMT112                                                           
         N     R1,=X'3FFFFFFF'     DROP FLAG                                    
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
FMT112   CHI   R1,10000                                                         
         BNL   FMT120                                                           
         EDIT  (R1),(7,(R8)),1                                                  
         B     FMT122                                                           
*                                                                               
FMT120   M     R0,=F'2'            X 2                                          
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(7,(R8))       PTS OR IMPS                                  
*                                                                               
FMT122   LTR   R1,R1                                                            
         BZ    *+8                                                              
         MVI   7(R8),C'*'                                                       
*                                                                               
         LA    R8,16(R8)           OUTPUT                                       
         LA    R5,4(R5)            IMP OR PNTS                                  
         LA    R7,4(R7)            EQUIV                                        
*                                                                               
         BCT   R4,FMT110                                                        
*                                                                               
FMTX     MVI   NOGOALS,C'N'        RESET SWITCH                                 
         B     EXIT                                                             
PATCH    DS    CL25                                                             
         EJECT                                                                  
FOR1HDG1 DS    0CL79               FORMAT 1 HEADING -LINE 1                     
*                                                                               
         DC    CL8' '                                                           
FOR1DEM  DC    CL21'----GOAL(RW18-49)----'                                      
         DC    CL2' '                                                           
         DC    CL21'-------PURCHASED-----'                                      
         DC    CL13' '                                                          
         DC    CL3'AVG'                                                         
         DC    CL2' '                                                           
         DC    CL9'PCT-ACHMT'                                                   
         SPACE 1                                                                
FOR1HDG2 DS    0CL79               FORMAT 1 HEADING -LINE 2                     
*                                                                               
         DC    CL8'DPT-LN'                                                      
FOR1CPPG DC    CL21'PNTS   DOLLARS   CPP '                                      
         DC    CL2' '                                                           
FOR1CPPP DC    CL21'PNTS   DOLLARS   CPP '                                      
         DC    CL13'      SPOTS  '                                              
FOR1PTS  DC    CL3'PTS'                                                         
         DC    CL2' '                                                           
FOR1PNTS DC    CL9'PNTS-DOLS'                                                   
FOR1H1A  DC    CL21'IMPS   DOLLARS   CPM '                                      
         DC    CL2' '                                                           
         DC    CL21'IMPS   DOLLARS   CPM '                                      
         DC    CL13'      SPOTS  '                                              
         DC    CL3'IMP'                                                         
         DC    CL2' '                                                           
         DC    CL4'IMPS'                                                        
FOR1H1B  DS    0C                                                               
         LTORG                                                                  
*======================================================================         
*        ROUTINE TO TEST PAGE COUNT AND NEXT LINE                               
*         P1=A(CURRENT LINE)  (RETURN=NEXT - 0 IF REQUESTED PAGE)               
*======================================================================         
NEXTLINE NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
         SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         LA    R2,0(RE,R2)         NEXT LINE                                    
         LA    R0,MISPFKH                                                       
         CR    R2,R0                                                            
         BL    NEXTXIT                                                          
*                                                                               
         CLC   BYTE4,PAGENUM       TEST FOR REQUESTED PAGE                      
         BNE   NEXT100                                                          
         SR    R2,R2               REQUESTED PAGE                               
         B     NEXTXIT                                                          
*                                                                               
NEXT100  LA    R2,MISL10H          START AT FIRST LINE AGAIN                    
*                                                                               
NEXT150  XC    8(L'MISL10,R2),8(R2)                                             
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
*                                                                               
         LA    R0,MISPFKH          LOOP TO ZERO ALL TWA DATA LINES              
         CR    R2,R0               LAST?                                        
         BL    NEXT150                                                          
*                                                                               
         IC    RE,BYTE4            BUMP PAGE NUMBER                             
         LA    RE,1(RE)                                                         
         STC   RE,BYTE4                                                         
         LA    R2,MISL11H                                                       
*                                                                               
NEXTXIT  ST    R2,0(R1)            NEXT LINE                                    
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*        DAYPART EXPANSION ROUTINE                                              
*         P1=A(DAYPART CODE - 1 BYTE)                                           
*         P2=A(3-BYTE OUTPUT AREA)  (EX.= FRG)                                  
*======================================================================         
                                                                                
DAYPART  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            P1                                           
         IC    R0,0(R2)            GET DPT CODE NIBBLE                          
         L     R3,4(R1)            P2                                           
*                                                                               
         LA    R5,SAVMENU          DAYPART MENU                                 
         LA    RE,5                ENTRY LENGTH                                 
         LA    RF,SAVMENU+L'SAVMENU-1                                           
         MVC   0(3,R3),=C'ZZZ'     SLUSH                                        
*                                                                               
         CLI   0(R5),0             LAST ENTRY?                                  
         BE    DAYPARTX                                                         
*                                                                               
DAYPART2 SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         CHI   R0,X'10'            IS IT A ZERO MASTER-DPT?                     
         BL    DAYPART4            YES, DON'T DROP SUB-DPT                      
         CLI   BYTE,C'*'           ARE WE WE DOING TOTALS?                      
         BNE   DAYPART4            NO, DON'T CLEAR                              
         N     R1,=X'000000F0'     DROP SUB-DPT                                 
         N     R0,=X'000000F0'     DROP SUB-DPT                                 
*                                                                               
DAYPART4 CR    R0,R1               SAME MASTER DPT                              
         BE    *+12                                                             
         BXLE  R5,RE,DAYPART2                                                   
         B     DAYPARTX            NOT FOUND                                    
*                                                                               
         CLI   0(R5),C'$'          SUPPRESS THIS WESTERN MOTHERFUCKER           
         JE    NEQXIT                                                           
         MVC   0(3,R3),2(R5)       MOVE LITERAL                                 
DAYPARTX J     EQXIT                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPMISWORK                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060SPMIS02   11/19/19'                                      
         END                                                                    
