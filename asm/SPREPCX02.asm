*          DATA SET SPREPCX02  AT LEVEL 051 AS OF 12/12/11                      
*PHASE SPCX02B                                                                  
                                                                                
*===================================================================            
*                                                                               
* QOPT1 =  Y TO PRINT ONLY 1 NON-MATCHING LOCAL LINE PER NETWORK BUY            
*                                                                               
* QOPT2 =  Y FOR RECOVERY FILE INPUT MODE (READS OUTPUT OF SCXGEN)              
*       =  P TO PRINT OUTPUT LIST OF RECORDS PROCESSED                          
*                                                                               
* QOPT3 =  Y TO FIX MISSING 68 ELEMENTS FOR CABLE BUYS ONLY                     
*                                                                               
* QOPT4 =  N TO SUPPRESS EMAIL MESSAGES                                         
*                                                                               
* TO START AT A PARTICULAR CLIENT CODE --                                       
* PUT ALL IN QCLT AND                                                           
* <CLT> IN QUESTOR, WHERE CLT IS THE STARTING CLIENT                            
*===================================================================            
                                                                                
         TITLE 'SPCX02 - CANADIAN NETWORK SPOT COUNT REPORT'                    
SPCX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPCX02                                                         
*                                                                               
         L     RC,=A(SPCX02WK)                                                  
         USING SPCX02WK,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BNE   *+12                                                             
         BRAS  RE,PROCB                                                         
         B     EXIT                                                             
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,CLTF                                                          
         B     EXIT                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,REQF                                                          
         B     EXIT                                                             
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BNE   *+12                                                             
         BRAS  RE,RUNL                                                          
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*=========================================                                      
* REQFRST                                                                       
*=========================================                                      
                                                                                
REQF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    GETBLK,GETBLK                                                    
         USING GETBUYD,GETBLK                                                   
         MVC   GBY1OR2,VGETBUY                                                  
         MVC   GBYCOMF,ACOMFACS                                                 
         LA    RE,DMWORK                                                        
         ST    RE,GBYDMWRK                                                      
*                                                                               
         CLC   QPRD,=C'POL'        ONLY RUN FOR POL !                           
         BE    *+10                                                             
         MVC   QPRD,=C'POL'                                                     
*                                                                               
         CLI   QOPT2,C'Y'          TEST T/A MODE                                
         BE    *+12                                                             
         CLI   QOPT2,C'P'          TEST T/A MODE AND PRINT                      
         JNE   EXIT                                                             
         MVI   TURNARND,C'Y'       SET TURNAROUND MODE FLAG                     
* OPEN SCXGEN OUTPUT FILE AND PROCESS BUYS FOR THIS AGENCY                      
         OPEN  (SCXGIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
REQF2    GET   SCXGIN,SCXGREC                                                   
*                                                                               
         CLC   SCXGAGY,QAGY        RIGHT AGENCY                                 
         BL    REQF2                                                            
         BH    REQFX                                                            
         AP    REQCOUNT,=P'1'                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),SCXGBUY        A-M/CLT/PRD/MKT/STA/EST                   
         MVC   KEY+11(2),SCXGBUY+10   MOVE 2-BYTE LINE NUMBER                   
*                                                                               
         MVI   GBYACT,GBYHIGH                                                   
         MVC   KEYSAVE,KEY                                                      
         LA    RE,KEYSAVE                                                       
         ST    RE,GBYKEYIN                                                      
         LA    RE,KEY                                                           
         ST    RE,GBYKEYOT                                                      
         GOTO1 VGETBUY,GETBLK                                                   
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   REQF2                                                            
*                                                                               
         MVI   GBYACT,GBYGET                                                    
         LA    RE,KEY+14                                                        
         ST    RE,GBYDA                                                         
         MVC   GBYIOA,ADBUY                                                     
         GOTO1 VGETBUY,GETBLK                                                   
*                                                                               
         BRAS  RE,PROCB                                                         
*                                                                               
         CLI   QOPT2,C'P'          TEST TO PRINT OUTPUT                         
         BNE   REQF2                                                            
*                                                                               
         CLI   BADBUYS,0           TEST ALREADY PRINTED ERROR                   
         BNE   REQF2                                                            
*                                                                               
         MVI   PRINTSW,C'X'        SET FLAG TO IGNORE COUNTS                    
         BRAS  RE,PRTBUY                                                        
*                                                                               
         B     REQF2                                                            
*                                                                               
REQFX    CLOSE (SCXGIN)                                                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
REQFX2   GOTO1 AENDREQ                                                          
         LTORG                                                                  
         EJECT                                                                  
*=======================================================                        
* RUNLAST - PRINT REPORT AND ERROR COUNTS                                       
*=======================================================                        
                                                                                
RUNL     NTR1  BASE=*,LABEL=*                                                   
         MVI   P,0                                                              
         GOTO1 REPORT                                                           
*                                                                               
         OI    REQCOUNT+3,X'0F'                                                 
         UNPK  P(5),REQCOUNT                                                    
         MVC   P+6(17),=C'RECORDS PROCESSED'                                    
*                                                                               
         MVI   P,0                                                              
         GOTO1 REPORT                                                           
*                                                                               
         OI    PCTERRS+3,X'0F'                                                  
         UNPK  P2(5),PCTERRS                                                    
         MVC   P2+6(13),=C'BAD PCT TOTAL'                                       
         GOTO1 REPORT                                                           
*                                                                               
         OI    COSTERRS+3,X'0F'                                                 
         UNPK  P2(5),COSTERRS                                                   
         MVC   P2+6(14),=C'BAD COST TOTAL'                                      
         GOTO1 REPORT                                                           
*                                                                               
         OI    LCLERRS+3,X'0F'                                                  
         UNPK  P2(5),LCLERRS                                                    
         MVC   P2+6(20),=C'BAD LOCAL SUM TOTAL'                                 
         GOTO1 REPORT                                                           
*                                                                               
         OI    ERRCOUNT+3,X'0F'                                                 
         UNPK  P2(5),ERRCOUNT                                                   
         MVC   P2+6(16),=C'RECORDS IN ERROR'                                    
         GOTO1 REPORT                                                           
*                                                                               
         CLI   QOPT4,C'N'          TEST SUPPRESS EMAIL                          
         BE    RUNL2                                                            
         CLI   TURNARND,C'Y'                                                    
         BNE   RUNL2               NO EMAIL IF REQUESTED REPORT                 
         CP    ERRCOUNT,=P'0'                                                   
         BE    *+8                                                              
         BRAS  RE,NOTEOUT                                                       
*                                                                               
RUNL2    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=========================================                                      
* CLTFRST                                                                       
*=========================================                                      
                                                                                
CLTF     NTR1  BASE=*,LABEL=*                                                   
         CLI   QUESTOR,C'<'        TEST FOR START-UP CLIENT                     
         BNE   CLTF2                                                            
         CLC   CLT,QUESTOR+1       REACHED IT YET ?                             
         BNL   CLTF2                                                            
         MVC   P(15),=C'SKIPPING CLIENT'                                        
         MVC   P+16(3),CLT                                                      
         GOTO1 REPORT                                                           
         MVI   MODE,CLTLAST                                                     
         J     EXIT                                                             
*                                                                               
CLTF2    MVC   P(17),=C'PROCESSING CLIENT'                                      
         MVC   P+20(3),CLIENT                                                   
         GOTO1 REPORT                                                           
         J     EXIT                                                             
         EJECT                                                                  
*=========================================                                      
* PROCBUY                                                                       
*=========================================                                      
                                                                                
PROCB    NTR1  BASE=*,LABEL=*                                                   
*                                  ALWAYS BUILD 2-BYTE LINE NUM                 
         MVC   SVNETKEY,KEY        SAVE NETWORK BUY KEY                         
         CLI   QOPT2,C'Y'          TEST INPUT FROM TAPE                         
         BE    PROCB2              YES - LINE NUM IS IN 2-BYTE FORMAT           
*                                                                               
         AP    REQCOUNT,=P'1'      IF READING FILE, COUNT NOW                   
         LLC   R0,KEY+11           GET 1-BYTE LINE NUMBER                       
         TM    KEY+13,BUYRLN2      KEY HAS 2-BYTE LINE NUM                      
         BO    *+8                 NO                                           
         STCM  R0,3,SVNETKEY+11    SET 2-BYTE LINE NUM                          
* HAVE TO REREAD BUY NOW TO GET IN 2-BYTE FORMAT                                
         MVI   GBYACT,GBYGET                                                    
         LA    RE,KEY+14                                                        
         ST    RE,GBYDA                                                         
         MVC   GBYIOA,ADBUY                                                     
         GOTO1 VGETBUY,GETBLK                                                   
*                                                                               
PROCB2   MVI   BADBUYS,0           RESET COUNTER                                
         MVI   PRINTSW,0                                                        
*                                                                               
         L     R8,ADBUY            POINT TO BUY RECORD                          
         USING BUYRECD,R8                                                       
* BUILD DATE TABLE                                                              
         GOTO1 DATCON,DMCB,(3,BDSTART),WORK   GET 6 BYTE START                  
         GOTO1 DATCON,DMCB,(3,BDEND),WORK+6   GET 6-BYTE END                    
         XC    DTTAB,DTTAB                                                      
         GOTO1 MOBILE,DMCB,(53,WORK),(5,DTTAB)                                  
*                                                                               
         BAS   RE,CNTELS           COUNT 0B/0C ELEMENTS BY DATE                 
         MVC   NETCNTS,COUNTS      SAVE NETWORK COUNTS                          
         MVC   NPDCNTS,PDCOUNTS     AND PAID COUNTS                             
                                                                                
* MAKE SURE NETWORK PERCENTS SUM TO 100%                                        
                                                                                
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         SR    RF,RF               CLEAR ACCUM                                  
         DROP  R8                                                               
*                                                                               
PB8A     BRAS  RE,NEXTEL                                                        
         BNE   PB8B                                                             
         ICM   R0,15,NTWKSHR-NTWKELEM(R6)                                       
         AR    RF,R0                                                            
         B     PB8A                                                             
*                                                                               
PB8B     C     RF,=F'100000'                                                    
         BE    PB8X                                                             
*                                                                               
         L     RE,ADBUY                                                         
         MVC   IOAREA(24),0(RE)    MOVE BUY KEY TO PRINT                        
         MVC   PCNTS(17),=C'BAD PERCENT TOTAL'                                  
         GOTO1 HEXOUT,DMCB,IOAREA,PCNTS+20,13,=C'TOG'                           
         MVI   PRINTSW,C'P'                                                     
         BAS   RE,PRTBUY           PRINT NETWORK BUY                            
         GOTO1 REPORT                                                           
         AP    PCTERRS,=P'1'       BUMP BAD PCT COUNTER                         
         J     EXIT                                                             
                                                                                
* SAVE NETWORK BUY                                                              
                                                                                
PB8X     L     RE,ADBUY            SET 'FROM' ADDR                              
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)         SET FROM LEN                                 
         L     R0,=A(IOAREA)       SET 'TO' ADDRESS                             
         LA    R1,2(RF)            SET TO CLEAR 2 EXTRA BYTES                   
         MVCL  R0,RE                                                            
*                                                                               
         L     R8,ADBUY            NOW GET NETWORK BUY DOLLARS                  
         USING BUYRECD,R8                                                       
*                                                                               
**NOP    IC    R0,BDPURP           SAVE PURPOSE CODE                            
**NOP    MVI   BDPURP,X'FD'        FORCE GETRATE TO RETURN COST                 
**NOP    GOTO1 GETRATE,DMCB,(0,SPOTS),ADBUY,0,0                                 
**NOP    STC   R0,BDPURP           RESTORE PURPOSE CODE                         
**NOP    MVC   NTWKDOLS,GROSS      SAVE NETWORK DOLLARS                         
*                                                                               
         SR    R0,R0               WORK IN BUY DOLLARS MHER 09DEC11             
         ICM   R0,7,BDCOST                                                      
         TM    BDCIND2,BDCRATPQ                                                 
         BO    *+8                                                              
         MHI   R0,100              CONVERT TO PENNIES                           
         ST    R0,NTWKDOLS                                                      
         XC    LCLDOLS,LCLDOLS     CLEAR LOCAL DOLLARS SUM                      
*                                                                               
         LA    R8,IOAREA                                                        
         USING BUYRECD,R8                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         ST    R6,LAST68EL                                                      
*                                                                               
PB10     L     R6,LAST68EL                                                      
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    PB11                                                             
         CLC   NTWKDOLS,LCLDOLS    SUM OF LOCALS=NETWORK COST                   
         JE    EXIT                                                             
*                                                                               
         AP    LCLERRS,=P'1'                                                    
         MVI   PRINTSW,C'T'        SET BAD TOTAL COST FLAG                      
         BRAS  RE,PRTBUY                                                        
         J     EXIT                                                             
*                                                                               
PB11     ST    R6,LAST68EL                                                      
         DROP  R8                                                               
*                                                                               
         USING NTWKELEM,R6                                                      
         XC    KEY,KEY             GET LOCAL BUY                                
         MVC   KEY(13),SVNETKEY                                                 
         MVC   KEY+4(5),NTWKMKST   MKT/STA FROM RECORD                          
*                                                                               
         MVI   GBYACT,GBYHIGH                                                   
         MVC   KEYSAVE,KEY                                                      
         LA    RE,KEYSAVE                                                       
         ST    RE,GBYKEYIN                                                      
         LA    RE,KEY                                                           
         ST    RE,GBYKEYOT                                                      
         GOTO1 VGETBUY,GETBLK                                                   
*                                                                               
         CLC   KEY(13),KEYSAVE     IS THERE REC W/ THIS KEY?                    
         BE    PB12                GOOD                                         
                                                                                
* MISSING LOCAL BUY                                                             
                                                                                
         MVI   PRINTSW,0                                                        
         BAS   RE,PRTBUY           PRINT NETWORK BUY IF NEEDED                  
         GOTO1 MSUNPK,DMCB,(X'80',NTWKMKST),WORK,PSTA                           
         MVC   PCNTS(9),=C'NOT FOUND'                                           
         GOTO1 HEXOUT,DMCB,KEYSAVE,PCNTS+10,13,=C'TOG'                          
         GOTO1 REPORT                                                           
         AP    ERRCOUNT,=P'1'      BUMP BAD NTWK BUY COUNTER                    
         B     PB20                IF NOT, FIND NXT 68 ELEM                     
*                                                                               
PB12     MVI   GBYACT,GBYGET                                                    
         LA    RE,KEY+14                                                        
         ST    RE,GBYDA                                                         
         MVC   GBYIOA,ADBUY                                                     
         GOTO1 VGETBUY,GETBLK                                                   
*                                                                               
         MVI   PRINTSW,0           CLEAR ERROR FLAG                             
         BRAS  RE,CHKCOS           CHECK LOCAL COST                             
         CLI   PRINTSW,C'$'        TEST HAD COST ERROR                          
         JE    EXIT                YES- ONLY PRINT ONE ERROR                    
*                                                                               
         L     R8,ADBUY                                                         
         BAS   RE,CNTELS                                                        
         CLC   NETCNTS,COUNTS      SAME COUNTERS                                
         BNE   PB14                                                             
         CLC   NPDCNTS,PDCOUNTS    AND PAID COUNTERS                            
         BE    PB20                                                             
*                                                                               
PB14     CLI   BADBUYS,0           TEST FIRST ERROR                             
         BNE   *+10                                                             
         AP    ERRCOUNT,=P'1'      BUMP BAD NTWK BUY COUNTER                    
*                                                                               
         IC    R0,BADBUYS          BUMP ERROR COUNT                             
         AHI   R0,1                                                             
         STC   R0,BADBUYS                                                       
*                                                                               
         CLI   QOPT1,C'Y'          ONLY PRINT 1 MISMATCH                        
         BNE   PB15                                                             
         CLI   BADBUYS,1                                                        
         BH    PB20                                                             
*                                                                               
PB15     MVI   PRINTSW,0                                                        
         BRAS  RE,PRTBUY                                                        
         GOTO1 MSUNPK,DMCB,(X'80',NTWKMKST),WORK,PSTA                           
         LA    R1,COUNTS                                                        
         BAS   RE,FMTCNT                                                        
         GOTO1 REPORT                                                           
                                                                                
*=========================================================                      
* CHECK FOR 68 ELEM IN EXPLODED BUY                                             
*=========================================================                      
                                                                                
PB20     L     R8,ADBUY                                                         
         LA    R6,BDELEM-BUYRECD(R8)                                            
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    PB10                                                             
                                                                                
* MISSING 68 ELEMENT                                                            
                                                                                
         CLI   BADBUYS,0           TEST FIRST ERROR                             
         BNE   *+10                                                             
         AP    ERRCOUNT,=P'1'      BUMP BAD NTWK BUY COUNTER                    
*                                                                               
         IC    R0,BADBUYS          BUMP ERROR COUNT                             
         AHI   R0,1                                                             
         STC   R0,BADBUYS                                                       
*                                                                               
         CLI   QOPT1,C'Y'          ONLY PRINT 1 MISMATCH                        
         BNE   PB25                                                             
         CLI   BADBUYS,1                                                        
         BH    PB30                                                             
*                                                                               
PB25     MVI   PRINTSW,C'Z'                                                     
         BRAS  RE,PRTBUY                                                        
         GOTO1 MSUNPK,DMCB,(X'80',4(R8)),WORK,PSTA                              
         MVC   PCNTS(15),=C'MISSING 68 ELEM'                                    
         GOTO1 REPORT                                                           
*                                                                               
PB30     CLI   QOPT3,C'Y'          TEST FIX MISSING 68                          
         BNE   PB40                                                             
*                                                                               
         LA    RE,BUYKSTAC-BUYREC(R8) POINT TO STATION                          
         CLI   2(RE),X'B0'            TEST CABLE SUFFIX                         
         BNL   PB32                                                             
* FOR NON-CABLE USE NETWORK BUY STATION                                         
         GOTO1 MSUNPK,DMCB,(X'80',IOAREA+4),WORK,WORK+10                        
         B     PB34                                                             
*                                                                               
PB32     GOTO1 MSUNPK,DMCB,(X'80',4(R8)),WORK,WORK+10   GET NTWK AGAIN          
*                                                                               
PB34     XC    DUB,DUB                                                          
         MVI   DUB,X'68'                                                        
         MVI   DUB+1,6                                                          
         MVC   DUB+2(4),WORK+10                                                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,13(R8)                                                      
         AR    R0,R8                                                            
         GOTO1 RECUP,DMCB,ADBUY,DUB,(R0)                                        
         GOTO1 PUTBUY                                                           
*                                                                               
PB40     B     PB10                                                             
         EJECT                                                                  
*================================================================               
* COUNT SPOTS BY DATE                                                           
*================================================================               
*                                                                               
         USING BUYRECD,R8                                                       
CNTELS   NTR1  BASE=*,LABEL=*                                                   
         XC    COUNTS,COUNTS                                                    
         XC    PDCOUNTS,PDCOUNTS                                                
*                                                                               
         LA    R4,DTTAB                                                         
         LA    R5,COUNTS                                                        
         XC    ELDATE,ELDATE                                                    
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
CNTELS2  BRAS  RE,NEXTEL                                                        
         BNE   CNTELSX                                                          
*                                                                               
         CLC   ELDATE,2(R6)        MAKE SURE DATES DO NOT DESCEND               
         BNH   CNTELS3                                                          
         MVI   COUNTS,X'FF'        SET BAD RECORD FLAG                          
         B     CNTELSX                                                          
*                                                                               
CNTELS3  CLI   1(R6),18            TEST PIGGY ELEM                              
         BNE   CNTELS4                                                          
         CLC   10(1,R6),14(R6)     TEST IF SAME PRODUCT                         
         BNE   CNTELS4                                                          
         OC    BUYKMKT,BUYKMKT     ONLY SET AT NET LEVEL                        
         BNZ   CNTELSX                                                          
         MVI   COUNTS,X'FE'        SET BAD PIGGIES FLAG                         
         B     CNTELSX                                                          
*                                                                               
CNTELS4  MVC   ELDATE,2(R6)        SAVE THIS DATE                               
*                                                                               
CNTELS6  CLC   2(2,R6),2(R4)       SPOT AFTER END OF CURRENT WEEK               
         BH    CNTELS20            YES - NEXT WEEK                              
*                                                                               
         CLC   2(2,R6),0(R4)       SPOT BEFORE START OF WEEK                    
         BL    CNTELS20            NO - SPOT IS IN CURRENT WEEK                 
* SPOT IS IN THIS WEEK                                                          
         SR    R0,R0                                                            
         TM    6(R6),X'80'         TEST MINUS                                   
         BO    CNTELS10                                                         
         IC    R0,0(R5)            BUMP + COUNTER                               
         AHI   R0,1                                                             
         STC   R0,0(R5)                                                         
         OC    4(2,R6),4(R6)       TEST SPOT PAID                               
         BZ    CNTELS8                                                          
         IC    R0,128(R5)                                                       
         AHI   R0,1                                                             
         STC   R0,128(R5)                                                       
*                                                                               
CNTELS8  B     CNTELS2                                                          
*                                                                               
CNTELS10 IC    R0,1(R5)            GET - COUNTER                                
         AHI   R0,1                                                             
         STC   R0,1(R5)                                                         
         OC    4(2,R6),4(R6)       TEST SPOT PAID                               
         BZ    CNTELS12                                                         
         IC    R0,129(R5)                                                       
         AHI   R0,1                                                             
         STC   R0,129(R5)                                                       
*                                                                               
CNTELS12 B     CNTELS2                                                          
*                                                                               
CNTELS20 LA    R5,2(R5)                                                         
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   CNTELS6                                                          
*                                                                               
CNTELSX  J     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
*=========================================================                      
* CHECK COST IN LOCAL BUY IS CORRECT                                            
* NOW WORKS IN BUY DOLLARS, NOT GROSS!  MHER 09DEC11                            
*=========================================================                      
                                                                                
CHKCOS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
*                                                                               
**NOP**  GOTO1 GETRATE,DMCB,(0,SPOTS),ADBUY,0,0                                 
**NOP**  L     R0,GROSS                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,BDCOST                                                      
         TM    BDCIND2,BDCNBRDQ    IS THE LOCAL BUY IN DOLLARS                  
         BZ    *+8                 NO                                           
         MHI   R0,100              CONVERT TO PENNIES                           
         ST    R0,GROSS            SAVE IN GROSS (BUT COULD BE NET)             
         A     R0,LCLDOLS          ADD TO LOCAL DOLLARS TOTAL                   
         ST    R0,LCLDOLS                                                       
*                                                                               
         L     R1,NTWKDOLS                                                      
         L     RE,LAST68EL                                                      
         ICM   R0,15,NTWKSHR-NTWKELEM(RE)   GET COST SHARE                      
         AR    R0,R0                        X 2                                 
         MR    R0,R0                                                            
         D     R0,=F'100000'                                                    
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                         ROUND                               
         SRA   R1,1                                                             
         ST    R1,FULL             SAVE COST                                    
*                                                                               
         S     R1,GROSS                                                         
         LPR   R1,R1                                                            
         CHI   R1,100              CHECK WITHIN A DOLLAR                        
         BH    BADCOST                                                          
         J     EXIT                                                             
         DROP  R8                                                               
                                                                                
* PRINT COST ERROR                                                              
                                                                                
BADCOST  L     R8,ADBUY            PRINT BAD LOCAL BUY                          
         MVI   PRINTSW,C'$'        SET PRINTING DOLLAR ERROR                    
*                                                                               
         LA    R6,BDELEM-BUYRECD(R8)                                            
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PCNTS(4),=C'NET='                                                
         MVC   PCNTS+4(4),2(R6)                                                 
         MVC   PCNTS+10(14),=C'BAD LOCAL COST'                                  
         L     R0,FULL             GET CALCULATED COST                          
         EDIT  (R0),(10,PCNTS+26),2                                             
         AP    COSTERRS,=P'1'                                                   
         AP    ERRCOUNT,=P'1'                                                   
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',4(R8)),WORK,PSTA                              
         BRAS  RE,PRTBUY                                                        
         J     EXIT                                                             
         EJECT                                                                  
*========================================================                       
* PRINT BUY                                                                     
*========================================================                       
                                                                                
PRTBUY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PRINTSW,C'$'        COST ERRORS PASS A(BUYREC)                   
         BE    *+8                                                              
         LA    R8,IOAREA                                                        
         USING BUYRECD,R8                                                       
*                                                                               
         CLC   PRTBUYSV,0(R8)                                                   
         JE    EXIT                                                             
         MVC   PRTBUYSV,0(R8)                                                   
*                                                                               
PRTB2    MVC   PAGY,BUYALPHA                                                    
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,WORK,PSTA                                    
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
*                                                                               
         ICM   R0,3,BUYKBUY                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
PRTB3    CLI   PRINTSW,C'X'        TEST PRINTING OUTPUT TRACE                   
         BNE   PRTB5                                                            
         MVC   PCNTS(6),=C'BUY OK'                                              
         B     PRTB20                                                           
*                                                                               
PRTB5    CLI   PRINTSW,C'Z'        TEST MISSING 68 ELEM                         
         BE    PRTB20                                                           
*                                                                               
         CLI   PRINTSW,C'P'        TEST BAD PCT TOTAL                           
         BE    PRTBX                                                            
*                                                                               
         CLI   PRINTSW,C'$'                                                     
         BE    PRTBX                                                            
*                                                                               
         CLI   PRINTSW,C'T'        TEST DOLLAR TOTAL ERR                        
         BNE   PRTB10                                                           
*                                                                               
         MVC   PCNTS+00(9),=C'NETWORK $'                                        
         L     R0,NTWKDOLS                                                      
         EDIT  (R0),(10,PCNTS+10),2,ALIGN=LEFT                                  
         MVC   PCNTS+21(9),=C'SUM LCL $'                                        
         L     R0,LCLDOLS                                                       
         EDIT  (R0),(10,PCNTS+31),2,ALIGN=LEFT                                  
         GOTO1 DATCON,DMCB,(3,BDSTART),(5,PCNTS+42)                             
         B     PRTBX                                                            
*                                                                               
PRTB10   LA    R1,NETCNTS                                                       
         BAS   RE,FMTCNT                                                        
*                                                                               
* MOVE PRINT LINES DOWN TO SKIP A LINE                                          
*                                                                               
PRTB20   MVC   P3,P2                                                            
         MVC   P2,P1                                                            
         MVC   P1,SPACES                                                        
         MVI   P1,0                                                             
*                                                                               
         CLI   PRINTSW,C'X'                                                     
         BNL   PRTBX                                                            
*                                                                               
PRTB22   LA    R0,PEST-PAGY+P3        PUT BUY START DATE BELOW EST              
         GOTO1 DATCON,DMCB,(3,BDSTART),(5,(R0))                                 
*                                                                               
PRTBX    GOTO1 REPORT                                                           
         J     EXIT                                                             
*                                                                               
PRTBUYSV DS    XL13                                                             
         LTORG                                                                  
         EJECT                                                                  
*==========================================================                     
* FORMAT COUNTERS AT 0(R1) TO PRINT                                             
*==========================================================                     
                                                                                
FMTCNT   NTR1  BASE=*,LABEL=*                                                   
         MVC   PCNTTYPE,=C'OR'                                                  
         MVC   PCNTTYPE+132(2),=C'PD'                                           
         LA    R5,PCNTS                                                         
         LHI   R6,L'PCNTS/2                                                     
         CLI   0(R1),X'FF'         TEST BAD BUY                                 
         BNE   *+14                                                             
         MVC   PCNTS(28),=C'BUY ELEMENTS OUT OF SEQUENCE'                       
         J     EXIT                                                             
         CLI   0(R1),X'FE'         TEST BAD PIGGIES                             
         BNE   FMTCNT2                                                          
         MVC   PCNTS(32),=C'BUY ELEMENTS HAVE DOUBLE PIGGIES'                   
         J     EXIT                                                             
*                                                                               
FMTCNT2  MVI   0(R5),C'+'          SET TOO MANY TO PRINT                        
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         CHI   R0,9                                                             
         BH    FMTCNT4                                                          
         UNPK  0(1,R5),DUB                                                      
         LTR   R0,R0                                                            
         BNZ   FMTCNT4                                                          
         MVI   0(R5),C'.'                                                       
                                                                                
* FORMAT PAID COUNTS TO P2.COUNTERS ARE +128 FROM ORD COUNTERS                  
                                                                                
FMTCNT4  MVI   132(R5),C'+'        SET TOO MANY TO PRINT                        
         SR    R0,R0                                                            
         IC    R0,128(R1)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         CHI   R0,9                                                             
         BH    FMTCNT6                                                          
         UNPK  132(1,R5),DUB                                                    
         LTR   R0,R0                                                            
         BNZ   FMTCNT6                                                          
         MVI   132(R5),C'.'                                                     
*                                                                               
FMTCNT6  LA    R1,1(R1)                                                         
         LA    R5,2(R5)                                                         
         BCT   R6,FMTCNT2                                                       
         J     EXIT                                                             
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* ROUTINE TO SEND AN EMAIL ERROR NOTIFICATION                                   
*=====================================================================          
                                                                                
NOTEOUT  NTR1  BASE=*,LABEL=*                                                   
         WTO   'SCX - NON-ZERO ERROR COUNT'                                     
*                                                                               
         L     R6,ADCONLST                                                      
         USING SPADCONS,R6                                                      
         GOTOR VSMTP,DMCB,('SMTPAINI',JESMAIL)                                  
         MVC   SUBJECT+3(2),AGY                                                 
         GOTOR VSMTP,DMCB,('SMTPAPRS',TOWHO),(L'SUBJECT,SUBJECT)                
         MVC   P,SPACES                                                         
         MVC   ERRLINE+27(2),AGY                                                
         MVC   P(L'ERRLINE),ERRLINE                                             
***                                                                             
* GET JOB/STEP NAMES                                                            
***                                                                             
         LA    R4,FULL                                                          
         EXTRACT (R4),FIELDS=TIOT                                               
*                                                                               
         L     R4,FULL                                                          
         USING TIOTD,R4                                                         
         MVC   P(8),TIOCNJOB                                                    
         MVC   P+9(8),TIOCPSTN     PROC STEP NAME                               
         DROP  R4                                                               
*                                                                               
         GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
         GOTOR VSMTP,DMCB,('SMTPASND',0)                                        
         GOTOR VSMTP,DMCB,('SMTPAEND',0) DETACH SMTP                            
         DROP  R6                                                               
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
JESMAIL  DC    CL8'JESMAIL '                                                    
TOWHO    DC    C'AKAT,ABEA,MHER,WHOA,US-MEDIA_CONTROL:'                         
SUBJECT  DC    C'SCXAA NETWORK BUY ERRORS FOUND'                                
ERRLINE  DC    CL50'JOBNAMEX/STEPNAME CHECK SCXAA OUTPUT FOR ERRORS'            
*                                                                               
         DS    0D                                                               
         DC    CL8'SPCX02WK'                                                    
SPCX02WK DS    0D                                                               
LAST68EL DS    A                                                                
*                                                                               
GETBLK   DS    XL64                                                             
*                                                                               
SVNETKEY DS    XL13                                                             
         DS    XL1                 SPARE                                        
TURNARND DS    C                   TURNAROUND MODE FLAG                         
PRINTSW  DS    X                                                                
BADBUYS  DS    X                                                                
ELDATE   DS    XL2                                                              
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
NTWKDOLS DS    F                                                                
LCLDOLS  DS    F                                                                
REQCOUNT DC    PL4'0'                                                           
ERRCOUNT DC    PL4'0'                                                           
PCTERRS  DC    PL4'0'                                                           
COSTERRS DC    PL4'0'                                                           
LCLERRS  DC    PL4'0'                                                           
*                                                                               
         DS    0D                                                               
COUNTS   DS    XL128                                                            
PDCOUNTS DS    XL128               PAID COUNTERS AT COUNTERS+128                
NETCNTS  DS    XL128                                                            
NPDCNTS  DS    XL128                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'**DTTAB*'                                                    
DTTAB    DS    XL256                                                            
*                                                                               
SCXGIN   DCB   DDNAME=SCXGIN,DSORG=PS,RECFM=FB,LRECL=16,               X        
               BLKSIZE=3200,MACRF=GM,EODAD=REQFX                                
*                                                                               
         DS    0D                                                               
SCXGREC  DS    CL16                                                             
         ORG   SCXGREC                                                          
SCXGAGY  DS    CL2                                                              
SCXGBUY  DS    XL13                                                             
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    CL8'*IOAREA*'                                                    
IOAREA   DS    6000C                                                            
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
         ORG   P                                                                
PAGY     DS    CL2                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PSTA     DS    CL7                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL1                                                              
PCNTTYPE DS    CL2                                                              
         DS    CL1                                                              
PCNTS    DS    CL87                                                             
PPDCNTS  EQU   PCNTS+132                                                        
         ORG                                                                    
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPMODES                                                     
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE DDMASTD                                                        
         EJECT                                                                  
       ++INCLUDE DDREMOTED                                                      
         EJECT                                                                  
       ++INCLUDE SPGETBUYD                                                      
         EJECT                                                                  
       ++INCLUDE DDSMTPD                                                        
         EJECT                                                                  
         PRINT ON                                                               
TIOTD    DSECT                                                                  
         IEFTIOT1                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051SPREPCX02 12/12/11'                                      
         END                                                                    
