*          DATA SET SPREPLR40  AT LEVEL 002 AS OF 05/01/02                      
*PHASE SPLR02B,+0                                                               
         TITLE 'COMBINE 1/4HR MKT RECDS PER MKT AND DAY'                        
SPLR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPLR02,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
*                                                                               
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPLRRA                                                     
         ST    R5,RELO                                                          
*                                                                               
         LA    R1,EXPBUFF                                                       
         ST    R1,AEXPBUF                                                       
         AH    R1,=Y(BUFFQ)                                                     
         ST    R1,AACUMBUF                                                      
         AH    R1,=Y(BUFFQ)                                                     
         ST    R1,ASVMKT                                                        
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    RQFST                                                            
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
XIT      XIT1                                                                   
*                                                                               
**********************************************************************          
*RQFST - FIRST TIME IN FOR REQUEST                                              
**********************************************************************          
*                                                                               
RQFST    DS    0H                                                               
         XC    NMKTS(24),NMKTS                                                  
         OPEN  (OUT,(OUTPUT))                                                   
*                                                                               
         L     R9,ACOMFACS                                                      
         USING COMFACSD,R9                                                      
         GOTO1 CDATCON,DMCB,(5,DUB),(2,TODAYB)                                  
         DROP  R9                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         L     RE,ASVMKT                                                        
         L     RF,=F'10000'                                                     
         XCEF                                                                   
*                                                                               
         L     R4,ADBLOCK          PUT LIST OF MKTS FROM DEMAND TO TBL          
         USING DBLOCKD,R4                                                       
         XC    0(256,R4),0(R4)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBFUNCT,DBGETMKB                                                 
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELSRC,QOPT1      QOPT1  = SRC                                 
         PACK  DUB,QBOOK1(2)       QBOOK1 = BOOK FROM                           
         CVB   RE,DUB                                                           
         STC   RE,DBSELBK                                                       
         PACK  DUB,QBOOK1+2(2)                                                  
         CVB   RE,DUB                                                           
         STC   RE,DBSELBK+1                                                     
         L     RE,ADBUY            ADBUY = I/O AREA                             
         ST    RE,DBAREC                                                        
         GOTO1 DEMAND,DMCB,ADBLOCK,SVMRKT                                       
         L     R1,NMKTS                                                         
         LTR   R1,R1                                                            
         BZ    EXIT                NO MKTS FOURND FOR BOOK                      
         MVC   MKTCTR,NMKTS        LOOP THRU MKTS IN TABLE                      
         SPACE 2                                                                
*                                                                               
MKTLP    OC    MKTCTR,MKTCTR       R6 -> MARKET LOOP                            
         BZ    RQFRX                                                            
         L     R1,MKTCTR                                                        
         BCTR  R1,0                                                             
         ST    R1,MKTCTR                                                        
         BCTR  R1,0                                                             
         MH    R1,=Y(SVMKTQ)                                                    
         L     R6,ASVMKT                                                        
         AR    R6,R1                                                            
         USING SVMKTD,R6                                                        
         MVC   MKTNUM,=H'0101'                                                  
*        MVC   MKTNUM,SVMKTNUM                                                  
         DROP  R6                                                               
*                                                                               
         LA    R7,DAYTBL           R7 -> DAYS LOOP                              
DAYLP    DS    0H                                                               
         CLI   0(R7),X'FF'         DONE WITH DAYS -> DO NEXT MKTNUM             
*TST     BE    MKTLP                                                            
         BE    RQFRX                                                            
         MVC   DAY,0(R7)           DAY BIT FOR DEMAND                           
         MVC   KDAY,1(R7)          DAY FOR KEY                                  
*                                                                               
         LA    R8,TIMETBL          R8 -> TIMES LOOP                             
TIMELP   DS    0H                                                               
         CLI   0(R8),X'FF'         DONE WITH TIMES-> DO NEXT DAY                
         BE    TIMELPX             BUMP TO NEXT DAY                             
         MVC   ENQHR,0(R8)                                                      
         ZIC   R1,0(R8)                                                         
         BCTR  R1,0                                                             
         STC   R1,STQHR            CALC START QHR                               
         MVC   STIME,1(R8)                                                      
         MVC   ETIME,3(R8)                                                      
         LA    R8,L'TIMETBL(R8)    PT TO NEXT TIME FOR LOOP                     
*                                                                               
         L     R4,ADBLOCK          R4 -> DBLOCK                                 
         L     RE,ADBUY                                                         
         ST    RE,DBAREC                                                        
         MVC   DBSELRMK,MKTNUM                                                  
         MVC   DBSELDAY,DAY                                                     
         MVC   DBSELTIM(2),STIME                                                
         MVC   DBSELTIM+2(2),ETIME                                              
         MVI   DBFUNCT,DBGETTOT                                                 
         L     RE,AACUMBUF         CLEAR ACCUM BUFFER                           
         L     RF,=F'2000'                                                      
         XCEF                                                                   
         MVI   HKCNT,0                                                          
         MVI   TST,0                                                            
         DS    0H                                                               
         GOTO1 DEMAND,DMCB,ADBLOCK,HLFHK   <-- MERGE QHRS INTO 1/2HR            
         CLI   DBERROR,X'80'       EOF                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    DBDIVSOR,DBDIVSOR                                                
         BZ    *+12                NO RECORDS FOUND                             
         BAS   RE,BLDREC           BLD 1/2HR RECORD                             
         BAS   RE,PUTOUT           OUPUT RECD AND PRT KEY                       
         B     TIMELP              LOOP TO DO NEXT 1/2HRS FOR DAY               
TIMELPX  DS    0H                                                               
         LA    R7,L'DAYTBL(R7)     PT TO NEXT DAY FOR NEXT TIME LP              
         B     DAYLP                                                            
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
* BLDREC - BLD HLFHR RECD AND RELEASE                                           
**********************************************************************          
BLDREC   NTR1                                                                   
         L     R4,ADBLOCK                                                       
         L     R7,AEXPBUF          BLD A NEW 1/2HR RECD IN EXPBUF               
         USING DRKEY,R7                                                         
         LR    RE,R7                                                            
         L     RF,=F'2000'                                                      
         XCEF                                                                   
*                                                                               
         L     R5,DBAREC           PT TO LAST DEMAND RECD GOTTEN                
         MVC   DRKEY(18),0(R5)     COPY 1/4 KEY FROM DEMAND                     
         MVC   DRHIGHD,KDAY                                                     
         MVC   DRHIQHR,ENQHR                                                    
         MVI   DRFRSTEL,0                                                       
         MVC   DRRLEN,=Y(DRFRSTEL-DRKEY+1)                                      
         MVI   DRSTAT+4,C'S'       SET PROPER KEY                               
*                                                                               
         SR    RE,RE                                                            
         LA    R6,DRFRSTEL         POSN FOR 1ST ELEM IN NEW RECORD              
         LA    R5,DRFRSTEL-DRKEY(R5)   1/4 HR RECD                              
*                                                                               
BLD10    CLI   0(R5),X'5E'                                                      
         BE    BLD50               DONE                                         
         CLI   0(R5),0                                                          
         BE    BLD50                                                            
         CLI   0(R5),X'30'         DON'T COPY DEMO ELEMS                        
         BNL   BLD40                                                            
         IC    RE,1(R5)            COPY ELEM TO NEW RECD                        
         BCTR  RE,0                                                             
         EXMVC RE,0(R6),0(R5)                                                   
*                                                                               
         USING MARELEM,R6                                                       
         CLI   0(R6),X'01'                                                      
         BNE   *+10                                                             
         MVC   MARDATE,TODAYB      SET RECD CREATION DATE                       
         DROP  R6                                                               
*                                                                               
         USING QHELEM,R6                                                        
BLD25    CLI   0(R6),X'20'                                                      
         BNE   BLD30                                                            
         MVC   QHSQH,STQHR                                                      
         MVC   QHEQH,ENQHR                                                      
         MVC   QHDAY,KDAY                                                       
         DROP  R6                                                               
*                                                                               
BLD30    ZIC   RE,1(R6)            BUMP TO NEXT POSN IN OUTPUT RECD             
         AR    R6,RE                                                            
         MVI   0(R6),0             END OF RECD MRKER                            
         ST    R6,EORPTR           SAVE PTR TO END OF RECD                      
         SR    R1,R1                                                            
         ICM   R1,3,DRRLEN         UPDATE RECD LENGTH                           
         AR    R1,RE                                                            
         STCM  R1,3,DRRLEN                                                      
*                                                                               
BLD40    IC    RE,1(R5)            BUMP TO NEXT POSN IN SOURCE RECD             
         AR    R5,RE                                                            
         B     BLD10                                                            
*                                                                               
BLD50    L     R6,AACUMBUF         BLD DEMO ELEMS FROM ACCUM BUFFER             
         L     R9,ACOMFACS                                                      
         USING COMFACSD,R9                                                      
         GOTO1 CDEMEL,DMCB,(C'C',OFORMAT),DBLOCK,(R6)                           
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R9                                                               
*                                  ADD DEMO ELEMENTS TO RECORD                  
         LA    R6,2(R6)            BUMP PAST 2BYTE LENGTH                       
         L     R5,EORPTR           PT TO 1ST POSN TO ADD DEMOS                  
         SR    R1,R1                                                            
*                                                                               
BLD60    CLI   0(R6),0                                                          
         BE    BLDRECX                                                          
         IC    R1,1(R6)            L'ELEM                                       
         BCTR  R1,0                                                             
         EXMVC R1,0(R5),0(R6)      MOVE ELEM TO OUTPUT RECD                     
         LA    R1,1(R1)            RESTORE ACTUAL LENGTH                        
         ICM   RE,3,DRRLEN         UPDATE RECD LENGTH                           
         AR    RE,R1                                                            
         STCM  RE,3,DRRLEN                                                      
         AR    R5,R1               BUMP OUTPUT RECD PTR                         
         AR    R6,R1               BUMP DEMO LIST PTR                           
         MVI   0(R5),0                                                          
         B     BLD60                                                            
*                                                                               
BLDRECX  B     XIT                                                              
         DROP  R7                                                               
*                                                                               
**********************************************************************          
* HLFHK  - SUM QHRS FOR THIS SET OF 1/2HR RECDS                                 
**********************************************************************          
         DS    0D                                                               
         USING *,RF                                                             
HLFHK    NTR1  BASE=SPLRRB                                                      
         LM    RA,RC,SPLRRA                                                     
         DROP  RF                                                               
         ZIC   R1,HKCNT            NUMBER OF TIMES IN HK THIS CALL              
         LA    R1,1(R1)                                                         
         STC   R1,HKCNT                                                         
*                                                                               
         L     R4,ADBLOCK                                                       
         L     R5,DBAREC           ADBUY BUFFER HOLDS 1/4HR RECD                
         USING DRKEY,R5                                                         
         MVC   MYKEY,DRKEY         SAVE KEY FOR LATER RELSG RECD                
         LA    R1,DRFRSTEL         GET 1ST '5E' ELEM                            
*                                                                               
         XC    OFORMAT,OFORMAT                                                  
         SR    R0,R0                                                            
HLF10    CLI   0(R1),0             EOR?                                         
         BNE   *+6                                                              
         DC    H'0'                EOF BEFORE '5E' FOUND                        
         CLI   0(R1),X'5E'         GET FIELDS FOR OFORMAT                       
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     HLF10                                                            
         MVC   OFILE,2(R1)                                                      
         MVC   OBOOK,5(R1)         GET BOOK FROM X'5E' ELEM                     
         MVC   OMED,DBACTMED                                                    
         MVC   OINTFIL(2),DBINTFIL                                              
         MVC   OSOURCE,DBACTSRC                                                 
*                                                                               
         L     R6,AEXPBUF           EXPLODE RECD INTO R6 =EXPBUFF               
         LR    RE,R6               CLEAR BUFFER                                 
         L     RF,=F'2000'                                                      
         XCEF                                                                   
         L     R9,ACOMFACS                                                      
         USING COMFACSD,R9                                                      
         GOTO1 CDEMEL,DMCB,(C'E',OFORMAT),DBLOCK,(R6),RR=RELO                   
         DROP  R9                                                               
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   HKCNT,1                                                          
         BNE   *+12                                                             
         ICM   RF,15,12(R1)     A(DSP TBL)                                      
         ST    RF,ADSPTBL                                                       
         OC    DBFACTOR,DBFACTOR                                                
         BZ    *+8                                                              
         BAS   RE,DEMMATH          MULT BY DBFACTOR AND ADD TO ACUMBUF          
*                                                                               
MTA      B     MTAX                                                             
         L     R1,AEXPBUF                                                       
         L     R2,AACUMBUF                                                      
         L     R3,ADBUY                                                         
         ZIC   RE,TST                                                           
         LA    RE,1(RE)                                                         
         STC   RE,TST                                                           
         CLI   TST,2                                                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DEMMATH          MULT BY DBFACTOR AND ADD TO ACUMBUF          
MTAX     DS    0H                                                               
*                                                                               
HLFHKX   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*CALCULATIONS - MULTIPLY EXPLODED RECORD IN EXPBUFF BY DBFACTOR.                
*        ADD RESULT TO ACCUMULATED VALUES AND SAVE IN ACUMBUFF.                 
*        R0 = DBNUMVLS                                                          
*        R2 = A(DSPTBL)                                                         
*        R5 = EXPBUFF  (EXPLODED RECD JUST GOT IN HOOK)                         
*        R6 = ACUMBUFF (SUM OF RECORDS IN DEMAND CALL)                          
*        R7 = VALUE BEING PROCESSED                                             
*        RE/RF = WORK REGS                                                      
**********************************************************************          
DEMMATH  NTR1                                                                   
         SR    R0,R0                                                            
         ICM   R0,3,DBNUMVLS       R0=NUMBER OF WORK AREA VALUES                
         BZ    DEMMATHX                                                         
         SR    R7,R7                                                            
         ICM   R7,3,DBFACTOR                                                    
         LTR   R7,R7               TEST IF DBFACTOR ZERO                        
         BZ    DEMMATHX                                                         
         L     R5,AEXPBUF                                                       
         L     R6,AACUMBUF                                                      
         L     R2,ADSPTBL          A(OUTPUT DISPLACEMENT TABLE)                 
         LA    R2,10(R2)           POINT PAST HEADER TO 1ST ENTRY               
*                                                                               
DEMMATH2 SR    RE,RE               PICK UP EXPBUFF VALUE                        
         ICM   RF,15,0(R5)                                                      
         CLI   0(R2),C'U'          SKIP OPERATION FOR                           
         BE    DEMMATH8            UNIVERSE VALUES                              
         CLI   0(R2),C'L'                                                       
         BE    DEMMATH8                                                         
*                                                                               
DEMMATH4 LTR   RF,RF               VALUE IN WORK AREA                           
         BZ    DEMMATH5                                                         
         BP    *+10                                                             
         SR    RF,RF               IF NEG OR ZERO, BYPASS MULTIPLY              
         B     DEMMATH5                                                         
         MR    RE,R7               MULTIPLY VALUE BY DBFACTOR                   
*                                                                               
DEMMATH5 ICM   R1,15,0(R6)         ADD: NEW RECD TO ACCUM                       
         AR    RF,R1                                                            
*                                                                               
DEMMATH8 ST    RF,0(R6)            SAVE RESULT                                  
         LA    R2,5(R2)            POINT TO NEXT DISPL TAB ENTRY                
         LA    R5,4(R5)            BUMP TO NEXT FIELD                           
         LA    R6,4(R6)            BUMP ACCUM BUFFER                            
         BCT   R0,DEMMATH2         LOOP THRU DBNUMVLS                           
*                                                                               
DEMMATHX DS    0H                                                               
                                                                                
*        ZIC   R3,TST                                                           
*        LA    R3,1(R3)                                                         
*        STC   R3,TST                                                           
*        CH    R3,=H'2'                                                         
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
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
**********************************************************************          
* PUTOUT - OUTPUT RECD TO FILE AND PRINT KEY                                    
**********************************************************************          
PUTOUT   NTR1                                                                   
         LA    R5,RECIO                                                         
         USING DRKEY,R5                                                         
         XC    RECIOLN,RECIOLN                                                  
         ICM   R1,3,DRRLEN                                                      
         LA    R1,4(R1)                                                         
         STCM  R1,3,RECIOLN                                                     
         DROP  R5                                                               
         LA    R5,RECIOLN                                                       
         L     R7,=A(OUT)                                                       
         PUT   (R7),(R5)                                                        
         L     RE,OCOUNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OCOUNT                                                        
         C     RE,=F'30'                                                        
         BH    PUTOUTX                                                          
         GOTO1 HEXOUT,DMCB,RECIOLN,P,60                                         
         GOTO1 REPORT                                                           
PUTOUTX  XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* PRINT REPORT                                                                  
**********************************************************************          
PREP     NTR1                                                                   
*                                                                               
*        L     R9,NMKTS            SORT MARKETS INTO RANK ORDER                 
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
DAYTBL   DS    0XL6                      1ST BYTE = DAYBIT FOR DEMAND           
         DC    X'40',X'10',CL4'MON '     2ND BYTE = DAY BYTE FOR KEY            
         DC    X'20',X'20',CL4'TUE '                                            
         DC    X'10',X'30',CL4'WED '                                            
         DC    X'08',X'40',CL4'THU '                                            
         DC    X'04',X'50',CL4'FRI '                                            
         DC    X'02',X'60',CL4'SAT '                                            
         DC    X'01',X'70',CL4'SUN '                                            
         DC    X'95',X'95',CL4'M-F '                                            
         DC    X'FF',X'90',CL4'VAR '                                            
DAYTBLQ  EQU   *-DAYTBL                                                         
         SPACE 3                                                                
                                                                                
TIMETBL  DS    0XL5                   QHR CODE FOR KEY,ST-END TIMES             
         DC    X'01',AL2(0600,0630)                                             
         DC    X'03',AL2(0630,0700)                                             
         DC    X'05',AL2(0700,0730)                                             
         DC    X'07',AL2(0730,0800)                                             
         DC    X'09',AL2(0800,0830)                                             
         DC    X'0B',AL2(0830,0900)                                             
         DC    X'0D',AL2(0900,0930)                                             
         DC    X'0F',AL2(0930,1000)                                             
         DC    X'11',AL2(1000,1030)                                             
         DC    X'13',AL2(1030,1100)                                             
         DC    X'15',AL2(1100,1130)                                             
         DC    X'17',AL2(1130,1200)                                             
         DC    X'19',AL2(1200,1230)                                             
         DC    X'1B',AL2(1230,1300)                                             
         DC    X'1D',AL2(1300,1330)                                             
         DC    X'1F',AL2(1330,1400)                                             
         DC    X'21',AL2(1400,1430)                                             
         DC    X'23',AL2(1430,1500)                                             
         DC    X'25',AL2(1500,1530)                                             
         DC    X'27',AL2(1530,1600)                                             
         DC    X'29',AL2(1600,1630)                                             
         DC    X'2B',AL2(1630,1700)                                             
         DC    X'2D',AL2(1700,1730)                                             
         DC    X'2F',AL2(1730,1800)                                             
         DC    X'31',AL2(1800,1830)                                             
         DC    X'33',AL2(1830,1900)                                             
         DC    X'35',AL2(1900,1930)                                             
         DC    X'37',AL2(1930,2000)                                             
         DC    X'39',AL2(2000,2030)                                             
         DC    X'3B',AL2(2030,2100)                                             
         DC    X'3D',AL2(2100,2130)                                             
         DC    X'3F',AL2(2130,2200)                                             
         DC    X'41',AL2(2200,2230)                                             
         DC    X'43',AL2(2230,2300)                                             
         DC    X'45',AL2(2300,2330)                                             
         DC    X'47',AL2(2330,2400)                                             
*                                                                               
         DC    X'49',AL2(2400,2430)                                             
         DC    X'4B',AL2(2430,2500)                                             
*                                                                               
         DC    X'4D',AL2(0100,0130)                                             
         DC    X'4F',AL2(0130,0200)                                             
         DC    X'51',AL2(0200,0230)                                             
         DC    X'53',AL2(0230,0300)                                             
         DC    X'55',AL2(0300,0330)                                             
         DC    X'57',AL2(0330,0400)                                             
         DC    X'59',AL2(0400,0430)                                             
         DC    X'5B',AL2(0430,0500)                                             
         DC    X'5D',AL2(0500,0530)                                             
         DC    X'5F',AL2(0530,0600)                                             
         DC    X'FFFF'                                                          
TIMETBLQ EQU   *-TIMETBL                                                        
*                                                                               
         EJECT                                                                  
RELO     DC    F'0'                                                             
NMKTS    DC    F'0'                                                             
MKTCTR   DC    F'0'                                                             
ADSPTBL  DC    F'0'                                                             
HKCNT    DC    X'00'                                                            
TST      DC    X'00'                                                            
OCOUNT   DC    F'00'                                                            
*TODAYB   DS    XL2                                                             
EORPTR   DS    F                                                                
MYKEY    DS    CL24                                                             
*                                                                               
AEXPBUF  DS    A                                                                
AACUMBUF DS    A                                                                
ASVMKT   DS    A                                                                
*                                                                               
MKTNUM   DS    CL2                 MKT CURRENTLY BEING PROCESSED                
DAY      DS    CL1                 DAY   "         "      "                     
KDAY     DS    CL1                 DAY FOR KEY     "      "                     
STQHR    DS    CL1                 START QHR (1/2HR) CODE FOR KEY               
ENQHR    DS    CL1                 END QHR (1/2HR) CODE FOR KEY                 
STIME    DS    CL2                 START TIME "    "      "                     
ETIME    DS    CL2                 END TIME        "      "                     
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
         SPACE 2                                                                
RECIOLN  DS    F                                                                
RECIO    DS    2000F                                                            
         ORG   RECIO                                                            
EXPBUFF  DS    2000F               EXPAND RECD INTO BUFFER                      
ACCUM    DS    2000F               ACCUMULATED 1/2HR VALUES                     
SVMKTBL  DS    10000C                                                           
BUFFQ    EQU   2000                                                             
         DS    0D                                                               
*                                                                               
OUT      DCB   DDNAME=OUT,DSORG=PS,RECFM=VB,LRECL=2000,                X        
               BLKSIZE=32760,MACRF=PM                                           
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
**PAN#1  DC    CL21'002SPREPLR40 05/01/02'                                      
         END                                                                    
