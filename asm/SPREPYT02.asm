*          DATA SET SPREPYT02  AT LEVEL 049 AS OF 03/23/04                      
*PHASE SPYT02A                                                                  
*INCLUDE DAYUNPK                                                                
*        TITLE 'SPREPYT02 - TBS DATA TRACK INTERFACE FOR M AND F'               
         TITLE 'SPREPYT02 - TBS DATA TRACK INTERFACE FOR M AND F-INIT'          
SPYT02   CSECT                                                                  
*        PRINT NOGEN                                                            
         DS    4000C                                                            
         ORG   SPYT02                                                           
         NMOD1 0,SPYT02,RA                                                      
         L     RC,0(R1)                                                         
         USING SPWORKD,RC,R9                                                    
         LA    R9,2048(RC)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         TITLE 'SPREPYT02 - TBS DATA TRACK INTERFACE FOR M AND F-MODE'          
***********************************************************************         
*                                                                     *         
*        ANALYZE MODE                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         CLI   MODE,ESTFRST                                                     
         BE    BRDINFO                                                          
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    PB                                                               
*                                                                               
         CLI   MODE,REQFRST        FIRST FOR REQUEST                            
         BE    REQF                                                             
*                                                                               
         CLI   MODE,PRDLAST        LAST FOR STATION                             
         BE    STAB                STATION BILLING RECORDS                      
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         TITLE 'SPREPYT02 - TBS DATA TRACK INTERFACE FOR M AND F-RUNF'          
***********************************************************************         
*                                                                     *         
*        FIRST TIME FOR RUN - INITIALIZE SWITCHES                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RUNF     DS    0H                                                               
*                                                                               
         LA    R8,FILETAB          CLEAR OPEN SWITCHES IN FILETAB               
         USING FILETABD,R8         ESTABLISH TABLE ENTRY                        
*                                                                               
RUNFCLLP DS    0H                                                               
*                                                                               
         CLI   0(R8),X'FF'         DONE IF END OF TABLE REACHED                 
         BE    RUNFCLDN                                                         
*                                                                               
         L     RF,FTBOPNBA         POINT TO BUY FILE DCB OPEN SWITCH            
         MVI   0(RF),0             CLEAR IT                                     
*                                                                               
         L     RF,FTBOPNAA         POINT TO AFF FILE DCB OPEN SWITCH            
         MVI   0(RF),0             CLEAR IT                                     
*                                                                               
RUNFCLCN DS    0H                                                               
*                                                                               
         LA    R8,FILETABL(R8)     BUMP TO NEXT ENTRY IN TABLE                  
         B     RUNFCLLP                                                         
*                                                                               
RUNFCLDN DS    0H                                                               
*                                                                               
RUNFX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R8                                                               
*                                                                               
         TITLE 'SPREPYT02 - TBS DATA TRACK INTERFACE FOR M AND F-REQF'          
***********************************************************************         
*                                                                     *         
*        FIRST TIME FOR REQUEST - OPEN OUTPUT DATASETS IF NEEDED      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
REQF     DS    0H                                                               
*                                                                               
*        FIND ENTRY IN FILE TABLE FOR THIS MEDIA                                
*                                                                               
         LA    R8,FILETAB                                                       
         USING FILETABD,R8         ESTABLISH FILE TABLE ENTRY                   
*                                                                               
REQFFTLP DS    0H                                                               
*                                                                               
         CLI   0(R8),X'FF'         MUST FIND TABLE ENTRY                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   FTBMED,QMED         MATCH ON MEDIA                               
         BE    REQFFTFD                                                         
*                                                                               
REQFFTCN DS    0H                                                               
*                                                                               
         LA    R8,FILETABL(R8)     BUMP TO NEXT ENTRY IN TABLE                  
         B     REQFFTLP                                                         
*                                                                               
REQFFTFD DS    0H                                                               
*                                                                               
         ST    R8,SVFTBA           SAVE ATABLE ENTRY)                           
*                                                                               
         L     R3,FTBOPNBA         POINT TO BUY OUTPUT DCB SWITCH               
         CLI   0(R3),C'Y'          SKIP IF BUY OUTPUT DATASET IS OPEN           
         BE    REQFBUYX                                                         
*                                                                               
*        ADD AGENCY AND MEDIA TO DATASET NAME                                   
*                                                                               
         MVC   BUYDSN+11(2),QAGY   SET AGENCY                                   
         MVC   BUYDSN+15(1),QMED   SET MEDIA                                    
*                                                                               
         MVC   BUYDDN+06(1),QMED   SET MEDIA                                    
*                                                                               
*        ALLOCATE TAPE DATASET                                                  
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,BUYDDN),(0,BUYDSN)                              
*                                                                               
         L     R2,FTBBUYA          POINT TO BUY DCB                             
         OPEN  ((R2),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   0(R3),C'Y'          SET BUY OUTPUT DATASET IS OPEN               
*                                                                               
REQFBUYX DS    0H                                                               
*                                                                               
         CLI   QOPT1,C'Y'          SKIP IF AFF DATA NOT WANTED                  
         BNE   REQFAFFX                                                         
*                                                                               
         L     R3,FTBOPNAA         POINT TO AFF OUTPUT DCB SWITCH               
         CLI   0(R3),C'Y'          SKIP IF AFF OUTPUT DATASET IS OPEN           
         BE    REQFAFFX                                                         
*                                                                               
*        ADD AGENCY AND MEDIA TO DATASET NAME                                   
*                                                                               
         MVC   AFFDSN+11(2),QAGY   SET AGENCY                                   
         MVC   AFFDSN+15(1),QMED   SET MEDIA                                    
*                                                                               
         MVC   AFFDDN+06(1),QMED   SET MEDIA                                    
*                                                                               
*        ALLOCATE TAPE DATASET                                                  
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,AFFDDN),(0,AFFDSN)                              
*                                                                               
         L     R2,FTBAFFA          POINT TO AFFADAVIT DCB                       
         OPEN  ((R2),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   0(R3),C'Y'          SET AFF OUTPUT DATASET IS OPEN               
*                                                                               
REQFAFFX DS    0H                                                               
*                                                                               
REQFX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
BUYDSN   DC    CL30'SPTTAPE.SYTAGBYM'  BUY DSN                                  
BUYDDN   DC    CL08'SYTBUY  '          BUY DDNAME                               
AFFDSN   DC    CL30'SPTTAPE.SYTAGAFM'  AFFIDAVIT DSN                            
AFFDDN   DC    CL30'SYTAFF  '          AFFIDAVIT DDNAME                         
*                                                                               
         TITLE 'SPREPYT02 - TBS DATA TRACK INTERFACE FOR M - F-BRDINFO'         
***********************************************************************         
*                                                                     *         
*        BUILD TABLE OF BROADCAST MONTHS AND WEEKS                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BRDINFO  DS    0H                                                               
*                                                                               
         GOTO1 MOBILE,DMCB,(12,QSTART),(0,BRDMTHS)                              
         GOTO1 MOBILE,DMCB,(53,QSTART),(4,BRDWKS)                               
*                                                                               
         B     EXIT                                                             
*                                                                               
         TITLE 'SPREPYT02 - TBS DATA TRACK INTERFACE FOR M - F-PB'              
***********************************************************************         
*                                                                     *         
*        PROCESS NEXT BUY RECORD                                      *         
*              ADD ACTIVITY TO FIRST TAPE                             *         
*              ADD AFFIDAVIT DATA TO SECOND TAPE                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PB       DS    0H                                                               
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         L     R8,SVFTBA           RESTORE POINTER TO FILE TABLE ENTRY          
         USING FILETABD,R8                                                      
*                                                                               
         LA    R4,RECOUT                                                        
         USING TBSBUYD,R4                                                       
*                                                                               
         MVC   TBSMED,=C'TV'                                                    
         TM    BUYKAM,X'01'        MEDIA T?                                     
         BO    *+10                                                             
         MVC   TBSMED,=C'RD'       NOPE - RADIO                                 
*                                                                               
         MVI   TBSTYP,C'B'         BUY DATA                                     
         MVC   TBSSTA,SPACES                                                    
         MVC   TBSCLT,SPACES                                                    
         MVC   TBSMKT,SPACES                                                    
         GOTO1 MSUNPK,DMCB,(X'80',BUYMSTA),TBSMKT,DUB                           
         MVC   TBSSTA(4),DUB                                                    
*                                                                               
         CLC   TBSMED,=C'RD'       IF - RADIO                                   
         BNE   *+14                                                             
         MVC   TBSSTA+4(1),DUB+4      BAND                                      
         MVI   TBSSTA+5,C'M'                                                    
*                                                                               
         MVC   TBSREF(3),=C'   '                                                
         CLI   BUYMSTA+2,X'E8'     TEST CABLE                                   
         BL    *+10                                                             
         MVC   TBSREF(3),DUB+5     MOVE CABLE NETWORK                           
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TBSREF+3(4),DUB                                                  
*                                                                               
         GOTO1 CLUNPK,DMCB,BUYKCLT,TBSCLT                                       
         MVC   TBSPRD,SPACES                                                    
         MVC   TBSPRD(3),PRD                                                    
         EDIT  (B1,BUYKEST),(4,TBSEST),FILL=0                                   
*                                                                               
         GOTO1 DATCON,DMCB,(3,BDSTART),(0,TBSSTART)                             
         GOTO1 DATCON,DMCB,(3,BDEND),(0,TBSEND)                                 
*                                                                               
         CLC   TBSSTART,QEND       DROP IF OUTSIDE REQUEST PERIOD               
         BH    PBPUTDN                                                          
         CLC   TBSEND,QSTART                                                    
         BL    PBPUTDN                                                          
*                                                                               
         MVC   TBSRQSTR,QSTART     PASS ON REQUEST DATES                        
         MVC   TBSRQEND,QEND                                                    
*                                                                               
         GOTO1 =V(DAYUNPK),DMCB,BDDAY,(X'07',DUB)                               
         LA    R0,7                                                             
         LA    R1,DUB                                                           
         LA    R2,TBSDAYS                                                       
         MVC   TBSDAYS,SPACES                                                   
*                                                                               
PB10     CLI   0(R1),C'.'                                                       
         BE    PB15                                                             
*                                                                               
         MVC   0(1,R2),0(R1)                                                    
*                                                                               
         CH    R0,=H'4'                                                         
         BNE   *+12                                                             
         MVI   1(R2),C'H'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         CH    R0,=H'1'                                                         
         BNE   *+12                                                             
         MVI   1(R2),C'U'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         MVI   1(R2),C','                                                       
*                                                                               
         LA    R2,2(R2)                                                         
*                                                                               
PB15     LA    R1,1(R1)                                                         
         BCT   R0,PB10                                                          
*                                                                               
         BCTR  R2,0                                                             
         CLI   0(R2),C','                                                       
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
*                                                                               
         OC    BDTIMST,BDTIMST                                                  
         BZ    PB20                                                             
         EDIT  (B2,BDTIMST),(4,TBSTMST),FILL=0                                  
*                                                                               
         CLC   =C'2400',TBSTMST    CHECK FOR MIDNIGHT                           
         BNE   *+10                                                             
         MVC   TBSTMST(4),=C'0000'                                              
*                                                                               
PB20     OC    BDTIMEND,BDTIMEND                                                
         BZ    PB25                                                             
         EDIT  (B2,BDTIMEND),(4,TBSTMEND),FILL=0                                
*                                                                               
         CLC   =C'2400',TBSTMEND   CHECK FOR MIDNIGHT                           
         BNE   *+10                                                             
         MVC   TBSTMEND(4),=C'0000'                                             
*                                                                               
PB25     EDIT  (B1,BDSEC),(3,TBSSPTLN),FILL=0                                   
         MVC   TBSDYPT,BDDAYPT                                                  
*                                                                               
         EDIT  (B2,BDNTAX),(4,TBSTAX),FILL=0                                    
*                                                                               
         XC    FULL,FULL                                                        
         EDIT  (B4,FULL),(7,TBSCOMM),FILL=0                                     
*                                                                               
         MVC   TBSPRD2(20),SPACES                                               
*                                                                               
         LA    R1,5                                                             
         LA    R2,TBSP2LN                                                       
         ZAP   DUB,=P'0'                                                        
         OI    DUB+7,X'0F'                                                      
*                                                                               
PB30     UNPK  0(L'TBSP2LN,R2),DUB                                              
         LA    R2,L'TBSP2LN(R2)                                                 
         BCT   R1,PB30                                                          
*                                                                               
         LA    R1,5                                                             
         LA    R2,TBSP2CST                                                      
         ZAP   DUB,=P'0'                                                        
         OI    DUB+7,X'0F'                                                      
*                                                                               
PB31     UNPK  0(L'TBSP2CST,R2),DUB                                             
         LA    R2,L'TBSP2CST(R2)                                                
         BCT   R1,PB31                                                          
*                                                                               
         MVC   TBSPROG,BDPROGRM                                                 
*                                                                               
         MVI   TBSPRGT,C' '        PROGRAM TYPE                                 
*                                                                               
         MVI   TBSSPC,C' '                                                      
         CLI   BDPROGRM+17,0       SPECIALS?                                    
         BNE   *+8                                                              
         MVI   TBSSPC,C'Y'         Y=USE RATINGS ON BUY AS OVERRIDES            
*                                                                               
         MVI   TBSBART,C' '                                                     
         TM    BDCIND2,X'02'      TRADE BUY?                                    
         BZ    *+8                                                              
         MVI   TBSBART,C'B'        YUP-SO ITS A BARTER SPOT                     
*                                                                               
         CLC   KEY+4(2),BUYMSTA    IF MARKETS POINTER AND RECORD MKTS           
         BNE   PBSPL                  EQUAL                                     
*                                                                               
         MVI   ELCODE,X'02'           LOOKING FOR IN MARKET DEMOS               
         BAS   RE,GETEL                                                         
         BNE   PB80                                                             
         B     PBSPLX                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
PBSPL    DS    0H                  ELSE FIND SPILL MKT DEMOS                    
*                                                                               
         MVI   ELCODE,X'03'        LOOKING FOR SPILL MARKET DEMOS               
         BAS   RE,GETEL                                                         
*                                                                               
PBSPLLP  DS    0H                                                               
*                                                                               
         BNE   PB80                                                             
*                                                                               
         CLC   KEY+4(2),4(R6)      MATCH POINTER MKT TO SPILL MKT               
         BE    PBSPLDN                                                          
*                                                                               
PBSPLCN  DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         B     PBSPLLP                                                          
*                                                                               
PBSPLDN  DS    0H                                                               
*                                                                               
         GOTO1 MSUNPK,DMCB,KEY+4,TBSMKT,TBSSTA  SPILL MARKET                    
*                                                                               
PBSPLX   DS    0H                                                               
*                                                                               
         USING NDELEM,R6                                                        
*                                                                               
         MVC   TBSDEMO(32),SPACES                                               
         MVC   TBSDEMOV(8),SPACES                                               
*                                                                               
         LA    R0,8                                                             
         LA    RF,TBSDEMRT         INIT DEMO RATINGS                            
         ZAP   DUB,=P'0'                                                        
         OI    DUB+7,X'0F'                                                      
*                                                                               
         UNPK  0(L'TBSDEMRT,RF),DUB                                             
         LA    RF,L'TBSDEMRT(RF)                                                
         BCT   R0,*-10                                                          
*                                                                               
         ZIC   RE,NDLEN            LENGTH OF ELEMENT INCLUDING DEMOS            
         SH    RE,=H'24'           LENGTH OF DEMOINFO ONLY                      
         BNP   PB80                                                             
*                                                                               
         CH    RE,=H'64'           MAX 8 DEMOS                                  
         BNH   *+8                                                              
         LA    RE,64                                                            
*                                                                               
         LA    R2,NDEMNO                                                        
         LA    R3,TBSDEMO                                                       
         LA    R5,TBSDEMOV                                                      
         LA    R7,TBSDEMRT                                                      
*                                                                               
PB40     DS    0H                                                               
*                                                                               
         MVC   0(1,R3),1(R2)       DEMO TYPE                                    
         ZIC   RF,2(R2)                                                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(L'TBSDEMO-1,R3),DUB                                            
*                                                                               
         TM    4(R2),X'80'       MANUAL OVERRIDE?                               
         BZ    *+8                                                              
         MVI   0(R5),C'*'                                                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,5(R2)                                                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(L'TBSDEMRT,R7),DUB                                             
*                                                                               
         LA    R5,L'TBSDEMOV(R5)   BUMP OUTPUT LIST OF OVERRIDE *S              
         LA    R3,L'TBSDEMO(R3)    BUMP OUTPUT LIST OF DEMOS                    
         LA    R2,8(R2)            BUMP INPUT LIST OF DEMO INFO                 
         LA    R7,L'TBSDEMRT(R7)   BUMP OUTPUT LIST OF RATINGS                  
         SH    RE,=H'8'            COUNT OF DEMOS,=H'8'                         
         BP    PB40                                                             
*                                                                               
         DROP  R6                                                               
PB80     L     R6,ADBUY                                                         
         MVC   P,SPACES                                                         
         MVI   ELCODE,X'66'        COMMENT ELEM                                 
         BAS   RE,GETEL                                                         
         BNE   PB90                                                             
         USING COMELEM,R6                                                       
*                                                                               
         ZIC   R1,CMLEN                                                         
         SH    R1,=H'4'            REST OF ELEM + 1                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),CMDATA                                                      
*                                                                               
PB90     DS    0H                                                               
         MVC   TBSCOMMT,P                                                       
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
*                                                                               
*        BUILD TABLE OF COSTS/BROADCAST DATES                                   
*                                                                               
         LA    R3,CDTTBL           START OF TABLE                               
         USING CDTTBLD,R3          ESTABLISH COST/DATE TABLE ENTRY              
*                                                                               
         XC    CDTENTRY(CDTENTL),CDTENTRY INIT FIRST TABLE ENTRY                
*                                                                               
         L     R6,ADBUY                                                         
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
         LA    R6,24(R6)           POINT TO FIRST ELEMENT                       
*                                                                               
PBCDTLP  DS    0H                                                               
*                                                                               
         BAS   RE,NEXTL                                                         
         BNE   PBCDTDN                                                          
*                                                                               
         USING REGELEM,R6                                                       
*                                                                               
         TM    RSTATUS,X'C0'       IGNORE MINUS SPOTS                           
         BNZ   PBCDTCN                                                          
*                                                                               
         GOTO1 GETRATE,DMCB,SPOTS,ADBUY,(R6)   GET RATES                        
*                                                                               
         L     RF,ADBUY                                                         
         USING BUYRECD,RF                                                       
*                                                                               
         CLC   KEY+4(2),BUYMSTA    IF SPILL RECORD DROP COST                    
         BNE   PBCDT20                AND SKIP AFFID RECORD                     
*                                                                               
         MVC   CDTCOST,GROSS       SAVE SPOT COST                               
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLI   QOPT1,C'Y'          SKIP IF AFF DATA NOT WANTED                  
         BNE   *+8                                                              
         BAS   RE,AFFID            PUT OUT AFFID RECORD                         
*                                                                               
PBCDT20  DS    0H                                                               
*                                                                               
         MVC   CDTDATE,RDATE       SAVE SPOT RUN DATE                           
*                                                                               
         LA    R3,CDTENTL(R3)      BUMP TO NEXT TABLE ENTRY                     
         XC    CDTENTRY(CDTENTL),CDTENTRY INIT NEXT TABLE ENTRY                 
*                                                                               
PBCDTCN  DS    0H                                                               
*                                                                               
         B     PBCDTLP                                                          
*                                                                               
PBCDTDN  DS    0H                                                               
*                                                                               
         LR    RF,R3                                                            
         LA    RE,CDTTBL                                                        
         SR    RF,RE                                                            
         BZ    PBPUTDN             NO SPOTS                                     
         SR    RE,RE                                                            
         D     RE,=A(CDTENTL)      NUMBER OF TABLE ENTRIES                      
*                                                                               
         GOTO1 XSORT,DMCB,CDTTBL,(RF),CDTENTL,CDTENTL,0    SORT TABLE           
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,BDSTART),(2,HALF)                                 
         LA    R2,BRDWKS                                                        
*                                                                               
         CLC   HALF,0(R2)          IF BUY STARTS BEFORE REQUEST PERIOD          
         BNL   *+10                                                             
         MVC   HALF,0(R2)             DEFAULT TO REQUEST START DATE             
*                                                                               
*                                  FIND FIRST BROADCAST WEEK FOR BUY            
         CLI   0(R2),X'FF'         END OF LIST REACHED?                         
         BE    PBPUTDN                                                          
         CLC   HALF,0(R2)                                                       
         BL    *+14                                                             
         CLC   HALF,2(R2)                                                       
         BNH   *+12                                                             
         LA    R2,4(R2)                                                         
         B     *-32                                                             
                                                                                
         GOTO1 DATCON,DMCB,(3,BDEND),(2,FULL)                                   
*                                                                               
         ST    R2,ABRDWKST         SAVE START OF BROADCAST WEEKS                
*                                                                               
         LA    R3,CDTTBL           START OF TABLE                               
         MVC   SVCDTENT,=6X'FF'    INIT SAVED COST                              
         ZAP   SQNCTR,=P'0'        INIT SEQUENCE COUNTER                        
*                                                                               
PBPUTLP  DS    0H                                                               
*                                                                               
         USING CDTTBLD,R3          ESTABLISH COST/DATE TABLE ENTRY              
*                                                                               
         OC    CDTENTRY(CDTENTL),CDTENTRY   IF END OF TABLE                     
         BZ    *+14                                                             
         CLC   CDTCOST,CDTCOST-CDTTBLD+SVCDTENT OR CHANGE IN COST               
         BE    PBPUT50                                                          
*                                                                               
*        PUT OUT RECORD WITH SPOTS FOR THIS COST                                
*                                                                               
         CLC   SVCDTENT,=6X'FF'    SKIP IF FIRST TIME                           
         BE    PBPUT10                                                          
*                                                                               
         LTR   R0,R0               SKIP IF NO SPOTS IN PERIOD                   
         BZ    PBPUT10                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,15,CDTCOST-CDTTBLD+SVCDTENT  COST FOR THESE SPOTS             
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  TBSCOST,DUB                                                      
*                                                                               
         SR    RF,RF                                                            
         CVB   RF,SQNCTR                                                        
         STC   RF,TBSREF+7         SET SEQ COUNTER IN REFERENCE                 
         TR    TBSREF+7(1),SEQTBL  TRANSLATE SEQUENCE NUMBER                    
         AP    SQNCTR,=P'1'        INCREMENT SEQUENCE COUNTER                   
*                                                                               
         L     R2,FTBBUYA          POINT TO BUY DCB                             
         PUT   (R2),(R4)                                                        
*                                                                               
PBPUT10 DS     0H                                                               
*                                                                               
*        CLEAR SPOT BUCKETS                                                     
*                                                                               
         LA    R0,16               16 ENTRIES IN TABLE                          
         LA    R5,TBSSPTNM         START OF SPOTS PER WEEK                      
         ZAP   DUB,=P'0'                                                        
         OI    DUB+7,X'0F'                                                      
*                                                                               
         UNPK  0(L'TBSSPTNM,R5),DUB CLEAR SPOT BICKETS                          
         LA    R5,L'TBSSPTNM(R5)                                                
         BCT   R0,*-10                                                          
*                                                                               
         SR    R0,R0               INIT DATA COUNTER                            
*                                                                               
         MVC   SVCDTENT,CDTENTRY   SAVE CURRENT TABLE ENTRY                     
*                                                                               
PBPUT50 DS     0H                                                               
*                                                                               
         OC    CDTENTRY(CDTENTL),CDTENTRY   DONE IF END OF TABLE                
         BZ    PBPUTDN                                                          
*                                                                               
         L     R2,ABRDWKST         POINT TO START OF BROADCAST WEEKS            
         LA    R5,TBSSPTNM         START OF SPOTS PER WEEK                      
*                                                                               
PBDATELP DS    0H                                                               
*                                                                               
         CLI   0(R2),X'FF'         DONE IF END OF LIST REACHED                  
         BE    PBDATEDN                                                         
*                                                                               
         CLC   CDTDATE,0(R2)       DOES DATE FIT IN BROADCAST WEEK?             
         BL    PBDATECN                                                         
         CLC   CDTDATE,2(R2)                                                    
         BH    PBDATECN                                                         
*                                                                               
         PACK  DUB,0(L'TBSSPTNM,R5)                                             
         AP    DUB,=P'1'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  0(L'TBSSPTNM,R5),DUB   BUMP SPOTS IN WEEK COUNTER                
*                                                                               
         LA    R0,1                INDICATE SPOT IN PERIOD                      
*                                                                               
         B     PBDATEDN                                                         
*                                                                               
PBDATECN DS    0H                                                               
*                                                                               
         LA    R2,4(R2)            NEXT BOADCAST WEEK                           
         LA    R5,L'TBSSPTNM(R5)   NEXT SPOTS IN WEEK COUNTER                   
         B     PBDATELP                                                         
*                                                                               
PBDATEDN DS    0H                                                               
*                                                                               
PBPUTCN  DS    0H                                                               
*                                                                               
         LA    R3,CDTENTL(R3)      NEXT TABLE ENTRY                             
*                                                                               
         B     PBPUTLP                                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
PBPUTDN  DS    0H                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R8                                                               
*                                                                               
         TITLE 'SPREPYT02 - TBS TAPE - AFFIDAVIT TAPE - STAB'                   
***********************************************************************         
*                                                                     *         
*        AT END OF STATION READ STATION BILLING RECORDS AND ADD       *         
*              RECORDS TO AFFIDAVIT TAPE FOR ALL BILLS ISSUED         *         
*              FOR STATION IN REQUEST PERIOD                          *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STAB     DS    0H                                                               
*                                                                               
         L     R8,SVFTBA           REATORE POINTER TO FILE TABLE ENTRY          
         USING FILETABD,R8                                                      
*                                                                               
         CLI   QOPT1,C'Y'          SKIP UNLESS AFFADAVIT DATA WANTED            
         BNE   STABX                                                            
*                                                                               
         L     RF,=A(RECAREA)                                                   
         ST    RF,AREC             SET I/O ADDRESS                              
*                                                                               
         LA    R3,RECOUT2          ESTABLISH TBS AFFIDAVIT RECORD               
         USING TBSAFFD,R3                                                       
*                                                                               
*        BUILD STATION BUCKET RECORD STARTING KEY                               
*                                                                               
         XC    KEY,KEY             ESTABLISH STATON BILLING KEY                 
         LA    R4,KEY                                                           
         USING STABUCKD,R4                                                      
*                                                                               
         MVC   STABKCOD,=X'0E01'   SET RECORD IDENTIFIER                        
         MVC   STABKAM,BAGYMD      SET AGENCY/MEDIA                             
         MVC   STABKCLT,BCLT       SET CLIENT CODE                              
         MVC   STABKPRD,BPRD       SET PRODUCT CODE                             
         MVC   STABKEST,BEST       SET ESTIMATE CODE - START                    
*                                                                               
         GOTO1 HIGH                READ FIRST STATION BUCKET                    
*                                                                               
STABLOOP DS    0H                                                               
*                                                                               
         CLC   KEY(STABKEST-STABUCKD),KEYSAVE DONE ON CHANGE IN PRODUCT         
         BNE   STABDONE                                                         
*                                                                               
         CLI   BEST,0              SKIP IF ESTIMATE 'NO'                        
         BE    STABLP10                                                         
*                                                                               
         CLC   STABKEST,BEST       ESTIMATE MUST BE IN RANGE                    
         BL    STABCONT                                                         
         CLC   STABKEST,BESTEND                                                 
         BH    STABCONT                                                         
*                                                                               
STABLP10 DS    0H                                                               
*                                                                               
         GOTO1 GET                 READ IN RECORD                               
*                                                                               
         L     R4,AREC             POINT TO FOUND RECORD                        
*                                                                               
         LA    R6,STABELEM         POINT TO FIRST ELEMENT                       
         USING STABELEM,R6         ESTABLISH BUCKET ELEMENT                     
*                                                                               
STABBKLP DS    0H                                                               
*                                                                               
         CLI   STABELEM,0          DONE AT END OF RECORD                        
         BE    STABBKDN                                                         
*                                                                               
         CLI   STABELEM,X'0E'      LOOKING FOR NORMAL ELEMENT                   
         BNE   STABBKCN                                                         
*                                                                               
         MVC   DUB(2),STABPER      COPY PERIOD - YM                             
         MVI   DUB+2,1             FIRST OF MONTH                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(0,WORK)    CONVERT TO YYMMDD                
*                                                                               
         GOTO1 GETBROAD,DMCB,WORK,WORK+12      FIND BROADCAST MONTH             
*                                                                               
         MVC   DATE,WORK+18        USE END OF BROADCAST MONTH                   
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+12),(2,FULL)   COMPRESS START DATE           
         GOTO1 DATCON,DMCB,(0,WORK+18),(2,FULL+2) COMPRESS END   DATE           
*                                                                               
         LA    R1,BRDMTHS          TABLE OF BROADCAST MONTH PERIODS             
*                                                                               
*        MAKE SURE ELEMENT FOR REQUESTED PERIOD                                 
*                                                                               
STABDTLP DS    0H                  FIND PERIOD THAT ADATE FALLS INTO            
*                                                                               
         OC    0(2,R1),0(R1)       IGNORE IF END OF LIST REACHED                
         BZ    STABDTDN                                                         
*                                                                               
         CLC   FULL+2(2),0(R1)     END AFTER START OF BRDCAST MNTH              
         BL    STABDTCN                                                         
*                                                                               
         CLC   FULL+2(2),2(R1)     AND BEFORE END OF MONTH                      
         BNH   STABDTFD                                                         
*                                  OR                                           
*                                                                               
         CLC   FULL(2),2(R1)       START BEFORE END   OF BRDCAST MNTH           
         BH    STABDTCN                                                         
*                                                                               
         CLC   FULL(2),0(R1)       AND   AFTER  START OF MONTH                  
         BNL   STABDTFD                                                         
*                                                                               
STABDTCN DS    0H                                                               
*                                                                               
         LA    R1,4(R1)            NEXT BROADCAST MONTH                         
         B     STABDTLP                                                         
*                                                                               
STABDTDN DS    0H                                                               
         B     STABBKCN            NEXT BUCK BUCKET                             
*                                                                               
STABDTFD DS    0H                                                               
*                                                                               
         MVC   TBSAREC(TBSARECL),SPACES      INIT AFFIDAVIT RECORD              
*                                                                               
         MVC   TBSAMED,=C'TV'      DEFAULT TO MEDIA - TV                        
*                                                                               
         TM    STABKAM,X'01'       MEDIA T?                                     
         BO    *+10                                                             
         MVC   TBSAMED,=C'RD'      NOPE - RADIO                                 
*                                                                               
         MVI   TBSATYP,C'I'        INVOICE RECORD                               
*                                                                               
         GOTO1 CLUNPK,DMCB,STABKCLT,TBSACLT  PASS CLIENT CODE                   
*                                                                               
         MVC   TBSAPRD(3),PRD                PASS PRODUCT CODE                  
*                                                                               
         EDIT  (B1,STABKEST),(4,TBSAEST),FILL=0                                 
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',STABKMKT),TBSAMKT,DUB                         
*                                                                               
         MVC   TBSASTA(4),DUB                                                   
*                                                                               
         CLC   TBSAMED,=C'RD'      IF - RADIO                                   
         BNE   *+14                                                             
         MVC   TBSASTA+4(1),DUB+4     BAND                                      
         MVI   TBSASTA+5,C'M'                                                   
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),STABPER     PERIOD IS YM BINARY                          
         MVI   FULL+2,X'01'        FORCE FIRST OF THE MONTH                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,FULL),(0,DATE)                                    
*                                                                               
         MVC   TBSAPD,DATE         AND USE YYMM PORTION                         
*                                                                               
*        CONSTRUCT INVOICE NUMBER                                               
*                                                                               
         MVC   TBSAINNM(1),MEDIA   SET MEDIA CODE                               
         MVI   TBSAINNM+1,C'-'                                                  
*                                                                               
         GOTO1 DATCON,DMCB,(2,STABBDT),(0,DATE)                                 
*                                                                               
         MVC   TBSAINNM+2(2),DATE+2    'MM'                                     
         MVI   TBSAINNM+4,C'-'                                                  
*                                                                               
         MVC   HALF,STABINV        INVOICE NUMBER                               
         NI    HALF,X'FF'-(STABINV_REVERSAL+STABINV_REVERSED)                   
*                                                                               
         EDIT  (B2,HALF),(4,TBSAINNM+5),FILL=0  INVOICE NUMBER                  
*                                                                               
         L     R2,FTBAFFA          POINT TO AFF DCB                             
         PUT   (R2),(R3)                                                        
*                                                                               
STABBKCN DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,STABELEM+1       ELEMENT LENGTH                               
         LA    R6,STABELEM(RF)     POINT TO NEXT ELEMENT                        
*                                                                               
         B     STABBKLP                                                         
*                                                                               
STABBKDN DS    0H                                                               
*                                                                               
STABCONT DS    0H                                                               
         LA    R4,KEY              RE-POINT TO KEY AREA                         
*                                                                               
         GOTO1 SEQ                 READ NEXT RECORD                             
*                                                                               
         B     STABLOOP                                                         
*                                                                               
STABDONE DS    0H                                                               
*                                                                               
STABX    DS    0H                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R3,R4,R8                                                         
*                                                                               
         TITLE 'SPREPYT02 - TBS TAPE - AFFIDAVIT TAPE - AFFID'                  
***********************************************************************         
*                                                                     *         
*        ROUTINE TO PUT OUT AFFID RECORD BASED ON POOL ELEMENT        *         
*                                                                     *         
*NTRY    R4==> TBSBUY   RECORDT                                       *         
*        R6==> POOL BUY ELEMENT                                       *         
*        R8==> FILETAB ENTRY                                          *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
AFFID    NTR1  LABEL=*                                                          
*                                                                               
         USING TBSBUYD,R4          ESTABLISH TBSBUY RECORD                      
*                                                                               
         USING REGELEM,R6          ESTABLISH BUY ELEMENT                        
*                                                                               
         USING FILETABD,R8                                                      
*                                                                               
         LA    R3,RECOUT2          ESTABLISH TBS AFFIDAVIT RECORD               
         USING TBSAFFD,R3                                                       
*                                                                               
         MVC   TBSAREC(TBSARECL),SPACES      INIT AFFIDAVIT RECORD              
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RLEN             BUMP TO NEXT BUY ELEMENT                     
         LA    R7,REGELEM(RF)                                                   
*                                                                               
         CLI   0(R7),X'10'         MUST BE AN AFFIDAVIT ELEMENT                 
         BNE   AFFIDX              IF NOT SKIP WRITING RECORD                   
*                                                                               
         USING AFFELEM,R7          ESTABLISH AFFID ELEMENT                      
*                                                                               
         MVC   TBSAMED,TBSMED                                                   
         MVI   TBSATYP,C'A'        AFFIDAVIT                                    
         MVC   TBSACLT,TBSCLT                                                   
         MVC   TBSAPRD,TBSPRD                                                   
         MVC   TBSAEST,TBSEST                                                   
         MVC   TBSAMKT,TBSMKT                                                   
         MVC   TBSASTA,TBSSTA                                                   
*                                                                               
         TM    RPSTAT2,X'80'       TRADE BUY?                                   
         BZ    *+8                                                              
         MVI   TBSABART,C'Y'       YUP-SO ITS A BARTER SPOT                     
*                                                                               
         PACK  DUB,TBSSPTLN        ADD LEADING 0 TO SPOT LENGTH                 
         UNPK  TBSASPLN,DUB                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(2,ADATE),(0,TBSADAT)                                
*                                                                               
         EDIT  (B4,GROSS),(12,TBSASPCS),FILL=0                                  
*                                                                               
         MVC   HALF,ATIME                                                       
         NI    HALF,X'FF'-X'F0'    TURN OFF BITS 0-3                            
         EDIT  (B2,HALF),(4,TBSATIME),FILL=0                                    
         MVI   TBSATIME+4,C' '                                                  
*                                                                               
         CLC   =C'2400',TBSATIME   CHECK FOR MIDNIGHT                           
         BNE   *+10                                                             
         MVC   TBSATIME(4),=C'0000'                                             
*                                                                               
         LA    R1,BRDMTHS          TABLE OF BROADCAST MONTH PERIODS             
*                                                                               
PB185    DS    0H                  FIND PERIOD THAT ADATE FALLS INTO            
*                                                                               
         OC    0(2,R1),0(R1)       END OF LIST                                  
         BL    PB190                                                            
*                                                                               
         CLC   ADATE,0(R1)                                                      
         BL    PB190              AFFID OUTSIDE REQUEST PERIOD                  
         CLC   ADATE,2(R1)                                                      
         BNH   PB195                                                            
         LA    R1,4(R1)                                                         
         B     PB185                                                            
*                                                                               
PB190    DS    0H                  AFFID OUTSIDE REQUEST PERIOD                 
*                                                                               
         GOTO1 DATCON,DMCB,(2,ADATE),(0,WORK)  CONVERT TO YYMMDD                
         GOTO1 GETBROAD,DMCB,WORK,WORK+12      FIND BROADCAST MONTH             
*                                                                               
         MVC   DATE,WORK+18        POINT TO END OF BROADCAST MONTH              
*                                                                               
         B     PB197                                                            
*                                                                               
PB195    DS    0H                                                               
*                                                                               
         MVC   HALF,2(R1)          CHANGE END DATE OF PERD TO YYMMDD            
         GOTO1 DATCON,DMCB,(2,HALF),(0,DATE)                                    
*                                                                               
PB197    DS    0H                                                               
*                                                                               
         MVC   TBSAPD,DATE         AND USE YYMM PORTION                         
*                                                                               
         OC    RPAY,RPAY           IF SPOT PAID                                 
         BZ    *+8                                                              
         MVI   TBSAAPP,C'Y'           SET AS APPROVED FOR PAYMENT               
*                                                                               
         L     R2,FTBAFFA          POINT TO AFF DCB                             
         PUT   (R2),(R3)                                                        
*                                                                               
AFFIDX   DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R3,R4,R6,R8                                                      
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
*        CLOSE ALL OPEN DATASETS                                                
*                                                                               
         LA    R8,FILETAB                                                       
         USING FILETABD,R8         ESTABLISH TABLE ENTRY                        
*                                                                               
RUNLCLLP DS    0H                                                               
*                                                                               
         CLI   0(R8),X'FF'         DONE IF END OF TABLE REACHED                 
         BE    RUNLCLDN                                                         
*                                                                               
         L     RF,FTBOPNBA         POINT TO BUY FILE DCB OPEN SWITCH            
*                                                                               
         CLI   0(RF),C'Y'          SKIP IF FILE NOT OPEN                        
         BNE   RUNLCLBX                                                         
*                                                                               
         L     R2,FTBBUYA          POINT TO BUY DCB                             
         CLOSE ((R2),) TPUT)       CLOSE FILE                                   
*                                                                               
         MVI   0(R3),C'N'          SET BUY OUTPUT DATASET IS CLOSED             
*                                                                               
RUNLCLBX DS    0H                                                               
*                                                                               
         L     RF,FTBOPNAA         POINT TO BUY FILE DCB OPEN SWITCH            
*                                                                               
         CLI   0(RF),C'Y'          SKIP IF FILE NOT OPEN                        
         BNE   RUNLCLAX                                                         
*                                                                               
         L     R2,FTBAFFA          POINT TO AFF DCB                             
         CLOSE ((R2),) TPUT)       CLOSE FILE                                   
*                                                                               
         MVI   0(R3),C'N'          SET BUY OUTPUT DATASET IS CLOSED             
*                                                                               
RUNLCLAX DS    0H                                                               
*                                                                               
RUNLCLCN DS    0H                                                               
*                                                                               
         LA    R8,FILETABL(R8)     BUMP TO NEXT ENTRY IN TABLE                  
         B     RUNLCLLP                                                         
*                                                                               
RUNLCLDN DS    0H                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R8                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
NEXTL    CLI   0(R6),0                                                          
         BE    NEXTLX                                                           
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTL2   CLI   0(R6),0                                                          
         BE    NEXTLX                                                           
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTL                                                            
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTL                                                            
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTLX   LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SPREPYT02 - TBS DATA TRACK INTERFACE FOR M-F-FILETAB'           
***********************************************************************         
*                                                                     *         
*        TABLE OF FILE PARAMETERS                                     *         
*              MEDIA,OPEN SWITCHES AND A(DCB)'S                       *         
*                                                                     *         
*        SEE FILETABD FOR DETAILS                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FILETAB  DS    0D                  FILE TABLE                                   
*                                                                               
         DC    CL1'R',XL3'00'      RADIO                                        
         DC    A(OPENBUYR),A(SYTBUYR)                                           
         DC    A(OPENAFFR),A(SYTAFFR)                                           
*                                                                               
         DC    CL1'T',XL3'00'      TV                                           
         DC    A(OPENBUYT),A(SYTBUYT)                                           
         DC    A(OPENAFFT),A(SYTAFFT)                                           
*                                                                               
         DC    CL1'X',XL3'00'      NETWORK RADIO                                
         DC    A(OPENBUYX),A(SYTBUYX)                                           
         DC    A(OPENAFFX),A(SYTAFFX)                                           
*                                                                               
         DC    XL1'FF'             EOT                                          
*                                                                               
         TITLE 'SPREPYT02 - TBS DATA TRACK INTERFACE FOR M-F-DCBS'              
***********************************************************************         
*                                                                     *         
*        DCBS FOR OUTPUT FILES                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYTBUYR  DCB   DDNAME=SYTBUYR,DSORG=PS,MACRF=(PM),                     +        
               RECFM=FB,LRECL=437,BUFNO=2,BLKSIZE=30590                         
*                                                                               
SYTBUYT  DCB   DDNAME=SYTBUYT,DSORG=PS,MACRF=(PM),                     +        
               RECFM=FB,LRECL=437,BUFNO=2,BLKSIZE=30590                         
*                                                                               
SYTBUYX  DCB   DDNAME=SYTBUYX,DSORG=PS,MACRF=(PM),                     +        
               RECFM=FB,LRECL=437,BUFNO=2,BLKSIZE=30590                         
*                                                                               
SYTAFFR  DCB   DDNAME=SYTAFFR,DSORG=PS,MACRF=(PM),                     +        
               RECFM=FB,LRECL=128,BUFNO=2,BLKSIZE=8192                          
*                                                                               
SYTAFFT  DCB   DDNAME=SYTAFFT,DSORG=PS,MACRF=(PM),                     +        
               RECFM=FB,LRECL=128,BUFNO=2,BLKSIZE=8192                          
*                                                                               
SYTAFFX  DCB   DDNAME=SYTAFFX,DSORG=PS,MACRF=(PM),                     +        
               RECFM=FB,LRECL=128,BUFNO=2,BLKSIZE=8192                          
*                                                                               
         TITLE 'SPREPYT02 - TBS DATA TRACK INTERFACE FOR M-F-WORK'              
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SEQTBL   DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ'                                    
*                                                                               
SQNCTR   DS    D                                                                
ELCODE   DS    X                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SVADDR   DS    A                                                                
SVFTBA   DS    A                                                                
SVCDTENT DS    XL(CDTENTL)         COST TABLE ENTRY SAVE                        
DATE     DS    CL6                                                              
OPENBUYR DS    C                   C'Y' - BUY OUTPUT OPEN - RADIO               
OPENBUYT DS    C                   C'Y' - BUY OUTPUT OPEN - TV                  
OPENBUYX DS    C                   C'Y' - BUY OUTPUT OPEN - NETWORK RD          
OPENAFFR DS    C                   C'Y' - AFF OUTPUT OPEN - RADIO               
OPENAFFT DS    C                   C'Y' - AFF OUTPUT OPEN - TV                  
OPENAFFX DS    C                   C'Y' - AFF OUTPUT OPEN - NETWORK RD          
ABRDWKST DS    A                   A(STARTING BROADCAST WEEK)                   
BRDWKS   DS    XL256               LIST OF BROADCAST WEEKS IN PERIOD            
BRDMTHS  DS    XL64                LIST OF BROADCAST MONTHS IN PERIOD           
SVB1XPRF DS    CL16                B1X PROFILE SAVEAREA                         
RECOUT2  DS    CL128                                                            
RECOUT   DS    CL500                                                            
CDTTBL   DS    200XL(CDTENTL)      COST/BROADCAST DATE TABLE                    
RECAREA  DS    4096C               I/O AREA                                     
*                                                                               
         TITLE 'SPREPYT02 - TBS DATA TRACK INTERFACE FOR M-F-FILETABD'          
***********************************************************************         
*                                                                     *         
*        TABLE OF FILE PARAMETERES DSECT                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FILETABD DSECT                     DSECT FOR FILE TABLE                         
FTBMED   DS    CL1                 MEDIA                                        
         DS    XL3                 SPARE                                        
FTBOPNBA DS    A                   A(BUY OUTPUT OPEN SWITCH)                    
FTBBUYA  DS    A                   A(BUY DATA DCB)                              
FTBOPNAA DS    A                   A(AFF OUTPUT OPEN SWITCH)                    
FTBAFFA  DS    A                   A(AFFADAVIT DATA DCB)                        
FILETABL EQU   *-FILETABD          LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'SPREPYT02 - TBS DATA TRACK INTERFACE FOR M-F-CDTTBLD'           
***********************************************************************         
*                                                                     *         
*        COST/DATE TABLE DSECT                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CDTTBLD  DSECT                     DSECT FOR COST/DATE TABLE                    
CDTENTRY DS    0X                  ENTRY IN TABLE                               
CDTCOST  DS    XL4                 SPOT COST                                    
CDTDATE  DS    XL2                 SPOT RUN DATE                                
CDTENTL  EQU   *-CDTENTRY          TABLE ENTRY LENGTH                           
         TITLE 'SPREPYT02 - TBS DATA TRACK INTERFACE FOR M-F-TBSBUYD'           
***********************************************************************         
*                                                                     *         
*        TBS RECORD DSECT                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TBSBUYD  DSECT                                                                  
         SPACE 2                                                                
TBSREC   DS    0C                                                               
TBSMED   DS    CL2       A/M       AGENCY/MEDIA                                 
TBSTYP   DS    CL1       A         RECORD TYPE B = BUY                          
TBSMKT   DS    CL6       A         MARKET                                       
TBSCLT   DS    CL8       CLT       CLIENT                                       
TBSPRD   DS    CL4       A         PRODUCT CODE                                 
TBSEST   DS    CL4       A         ESTIMATE NUMBER                              
TBSSTA   DS    CL6       A         STATION                                      
TBSREF   DS    CL8       A         LINE NUMBER                                  
*                                                                               
TBSSTART DS    CL6       A         BUY START DATE (YYMMDD)                      
TBSEND   DS    CL6       A         BUY END DATE (YYMMDD)                        
TBSDAYS  DS    CL17      A         MTWTHFSSU SEP BY COMMAS OR M-W,F,S           
TBSTMST  DS    CL4       N         MILITARY START TIME (0-2359)                 
TBSTMEND DS    CL4       N         MILITARY END TIME (0-2359)                   
TBSSPTLN DS    CL3       N         IN SECONDS                                   
TBSDYPT  DS    CL1       A         DAYPART                                      
TBSCOST  DS    CL11      C         SPOT COST                                    
TBSTAX   DS    CL4       C         TAX PERCENTAGE                               
TBSCOMM  DS    CL7       C         COMMISSION PERCENT                           
TBSSPTNM DS    16CL4     C         SPOTS PER WEEK                               
TBSPRD2  DS    5CL4      A         PIGGYBACK PRD                                
TBSP2LN  DS    5CL4      C         PIGGYBACK SPOT LENGTH                        
TBSP2CST DS    5CL11     C         PIGGYBACK COST                               
TBSDEMO  DS    8CL4      A         DEMOGRAPHIC CODES                            
TBSDEMRT DS    8CL9      C         DEMOGRAPHIC RATINGS                          
TBSDEMOV DS    8CL1      A         DEMOGRAPHIC OVERRIDE                         
TBSPROG  DS    CL16      A         PROGRAM NAME                                 
TBSPRGT  DS    CL1       A         PROGRAM TYPE                                 
TBSSPC   DS    CL1       A         Y=USE RATINGS ON BUY AS OVERRIDES            
TBSBART  DS    CL1       A         B=BARTER SPOT                                
TBSCOMMT DS    CL33      A         COMMENTS                                     
TBSRQSTR DS    CL6                 REQUEST START DATE YYMMDD                    
TBSRQEND DS    CL6                 REQUEST END   DATE YYMMDD                    
TBSRECX  EQU   *                                                                
TBSRECL  EQU   *-TBSREC            RECORD LENGTH                                
*                                                                               
         TITLE 'SPREPYT02 - TBS DATA TRACK INTERFACE FOR M-F'                   
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
TBSAFFD  DSECT                                                                  
       ++INCLUDE SPTBSAFFD                                                      
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049SPREPYT02 03/23/04'                                      
         END                                                                    
