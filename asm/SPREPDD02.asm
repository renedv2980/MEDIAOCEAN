*          DATA SET SPREPDD02  AT LEVEL 085 AS OF 05/24/06                      
*PHASE SPDD02B                                                                  
*INCLUDE MEDBDESC                                                               
*INCLUDE MEDAPRNT                                                               
         TITLE 'SPREPDD02 - DEMO COMPARISON REPORT'                             
         PRINT NOGEN                                                            
SPDD02   CSECT                                                                  
         NMOD1 0,SPDD02,R9                                                      
*                                                                               
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BNE   DD010                                                            
         STM   R9,RC,HDHKR9                                                     
         LA    RE,HEADHK                                                        
         ST    RE,HEADHOOK                                                      
*                                                                               
         L     RE,MEDBUFF          CLEAR THE MEDIA BUFFER                       
         USING MEDBLOCK,RE                                                      
         LA    RF,1272                                                          
         XCEF  ,                                                                
         L     RE,MEDBUFF                                                       
         LA    RF,MEDBY9-MEDGLD    SET THE ENTRY LENGTH                         
         ST    RF,MEDLCHNK                                                      
         XC    MEDNUMWK,MEDNUMWK                                                
         XC    MEDNUMMO,MEDNUMMO                                                
         XC    MEDNUMQT,MEDNUMQT                                                
         MVC   MEDNUMPE,=F'1'      PERIOD ONLY                                  
         MVI   RQEQUIV,C'Y'        READ EQUIVALENCE HDR                         
         MVI   RQDAYPT,C'Y'        READ DAYPART HDR                             
         DROP  RE                                                               
*                                                                               
         L     R1,VMASTC           LOAD SPGETDEMF                               
         USING DDMASTD,R1                                                       
         MVC   MCDUB,=C'T00A21  '                                               
         L     RF,MCVLOADM                                                      
         DROP  R1                                                               
         GOTO1 (RF),DMCB                                                        
         ICM   RF,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,VSPGETDM                                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
DD010    CLI   MODE,REQFRST        REQUEST FIRST                                
         BNE   DD015                                                            
         CLC   QBOOK1,SPACES       DEFAULT BOOK IS ACT                          
         BH    *+10                                                             
         MVC   QBOOK1(3),=C'ACT'                                                
*                                                                               
         XC    BQBOOK1,BQBOOK1                                                  
         CLC   QBOOK1(3),=C'ACT'                                                
         BE    DD012                                                            
         MVC   WORK(4),QBOOK1                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,WORK,(3,DUB)                                         
         MVC   BQBOOK1(2),DUB                                                   
*                                                                               
DD012    MVI   NDEMOS,4            4 DEMOS                                      
         MVI   RCSUBPRG,1                                                       
         MVI   QRERATE,C'I'        DEFAULT TO AFFID                             
         CLI   QOPT2,C'Y'          TEST RERATE REPORT                           
         BNE   *+16                                                             
         MVI   NDEMOS,8            YES-8 DEMOS                                  
         MVI   RCSUBPRG,2                                                       
         MVI   QRERATE,C' '                                                     
*                                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDEXTDM,NDEMOS     MOVE N'DEMOS TO MEDBLOCK                     
         DROP  RE                                                               
*                                                                               
         MVC   PAGE,=X'0001'                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   ESTSW,C'N'                                                       
*                                                                               
         GOTO1 DATCON,DMCB,QSTART,(2,BQSTARTP)                                  
         GOTO1 DATCON,DMCB,QSTART,(2,BQSTARTP)                                  
         GOTO1 DATCON,DMCB,QEND,(2,BQENDP)                                      
         GOTO1 DATCON,DMCB,QSTART,(3,BQSTART)                                   
         GOTO1 DATCON,DMCB,QEND,(3,BQEND)                                       
         L     RF,ADCONLST                                                      
         L     RF,VACTBKSR-SPADCONS(RF)                                         
         GOTO1 (RF),DMCB,(RA),0           INITIALIZE FOR MEDACTBK               
         B     EXIT                                                             
*                                                                               
DD015    CLI   MODE,CLTFRST        CLIENT FIRST                                 
         BNE   DD020                                                            
         MVI   SVSELSRC,C'N'                                                    
*                                                                               
         L     RE,ADCLT                                                         
         USING CLTHDRD,RE                                                       
*                                                                               
         CLI   CPROF+3,C'0'        0=NSI, 1=ARB                                 
         BE    *+8                                                              
         MVI   SVSELSRC,C'A'                                                    
*                                                                               
         XC    WORK,WORK           SET UP 1W PROFILE KEY                        
         MVC   WORK(4),=C'S01W'                                                 
         MVC   WORK+4(3),SVAGY                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  RE                                                               
         GOTO1 GETPROF,DMCB,WORK,A(SP1WPROF),DATAMGR                            
         L     RF,=A(SP1WPROF)                                                  
         ST    RF,ASV1W                                                         
         B     EXIT                                                             
         EJECT                                                                  
DD020    CLI   MODE,PRDFRST        PRODUCT FIRST                                
         BNE   DD030                                                            
         B     EXIT                                                             
         EJECT                                                                  
DD030    CLI   MODE,ESTFRST        ESTIMATE FIRST                               
         BNE   DD035                                                            
         L     R6,ADEST                                                         
         USING ESTHDR,R6                                                        
         MVC   ESTSVI,EHUTADJ      SAVE ESTIMATE'S SVI ADJUSTMENT MONTH         
*                                                                               
         GOTO1 MEDPRDRD,DMCB,(RA)  BUILD LIST OF PRODUCT/EST INFO               
         ZIC   R3,BPRD                                                          
         CLI   BPRD,X'FF'                                                       
         BNE   *+8                                                              
         LA    R3,220                                                           
         BCTR  R3,0                                                             
         MH    R3,PRDBUFLN                                                      
         L     RE,PRDBUFF                                                       
         LA    R3,28(R3,RE)                                                     
         MVC   DEMOS,0(R3)         EXTRACT DEMO CODE LIST                       
         ZIC   RE,NDEMOS                                                        
         MH    RE,=H'3'                                                         
         LA    RE,DEMOS(RE)                                                     
         MVI   0(RE),X'FF'         MARK END OF LIST                             
         XC    DNAMES,DNAMES                                                    
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBSELCLI,CLT                                                     
         MVC   DBSELSRC,SVSELSRC                                                
         LA    RE,DBWORK                                                        
         ST    RE,DBAREC           I/O AREA                                     
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBTPTT,C'T'         TYPICAL TIME                                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         LA    R8,EUSRNMS                                                       
         GOTO1 DEMOCON,DMCB,(NDEMOS,(R3)),(2,DNAMES),(C'S',DBLOCK),    X        
               (SPOTPROF+9,(R8))                                                
*                                                                               
         GOTO1 MEDDATE,DMCB,(RA)   BUILD MEDBLOCK                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
DD035    CLI   MODE,STAFRST        STATION FIRST                                
         BNE   DD040                                                            
         MVI   FORCEHED,C'Y'                                                    
         XC    ESTTOTS,ESTTOTS                                                  
         XC    AVGTOTS,AVGTOTS                                                  
         XC    WKYTOTS,WKYTOTS                                                  
         XC    ACTBKTAB,ACTBKTAB                                                
* PASS ENOUGH INFO FOR SPILL MARKETS                                            
*        MVC   DBSELMK,SPILL       DON'T KNOW SPILL                             
         MVC   DBSELSTA(4),BIGSTA                                               
         MVI   DBSELSTA+4,C'T'                                                  
         L     RF,ADBLOCK                                                       
         MVC   0(255,RF),DBLOCK    SET DBLOCK IN STANDARD AREA                  
*                                                                               
         L     RF,ADCONLST                                                      
         L     RF,VACTBKSR-SPADCONS(RF)                                         
         GOTO1 (RF),DMCB,(0,(RA)),BQSTARTP,ACTBKTAB   GET BOOKS                 
*                                                                               
         CLC   QBOOK1(3),=C'ACT'   TEST ACTUAL BOOK LOOKUP                      
         BNE   EXIT                                                             
         GOTO1 VMDAPRNT,DMCB,(RA),ACTBKTAB,0   YES-SAVE THE ACTUAL BKS          
         B     EXIT                                                             
         EJECT                                                                  
DD040    CLI   MODE,PROCBUY        PROCESS BUY RECORD                           
         BNE   DD200                                                            
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
*                                                                               
         CLC   BDSTART,BQEND       FILTER BUY ON REQUEST PERIOD                 
         BH    EXIT                                                             
         CLC   BDEND,BQSTART                                                    
         BL    EXIT                                                             
*                                                                               
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         MVC   MEDBRAND,BPRD                                                    
         MVC   MEDSPTLN,BDSEC                                                   
         GOTO1 MEDGETBY,DMCB,(RA),2     CALL MEDGETBY                           
         CLI   MEDSPILL,C'Y'       BYPASS SPILL                                 
         BE    EXIT                                                             
         OC    MEDBYD(12),MEDBYD   ANY SPOTS OR DOLLARS                         
         BZ    EXIT                                                             
         LA    R4,MEDBY1                                                        
         ZIC   RF,NDEMOS                                                        
         LA    R1,ESTTOTS                                                       
*                                                                               
DD042    L     RE,0(R1)            ACCUMULATE ESTIMATED DEMO TOTALS             
         A     RE,0(R4)                                                         
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    R4,8(R4)                                                         
         BCT   RF,DD042                                                         
         DROP  R7                                                               
*                                                                               
         GOTO1 =V(VMDBDESC),DMCB,(RA),BDAREA    EXTRACT BUY DESCRIPTION         
*                                                                               
         MVI   BKTYPE,0                                                         
         LA    R8,BDELEM           LOOK FOR DEMO LOOKUP OVERRIDE ELEM           
         MVI   ELCDLO,X'24'                                                     
         MVI   ELCDHI,X'24'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING DLUELEM,R8                                                       
         MVC   BKTYPE,DLUBKTYP     EXTRACT SPECIAL BOOK TYPE                    
*                                                                               
         LA    R8,BDELEM                                                        
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         BAS   RE,GETEL            GET DEMO ELEMENT                             
         BNE   DD090                                                            
         XC    ESTVALS,ESTVALS                                                  
         USING NDELEM,R8                                                        
         ZIC   R3,NDLEN                                                         
         AHI   R3,-24                                                           
         BNP   DD090                                                            
         SRL   R3,3                R3 = NO OF DEMOS IN ELEMENT                  
         LA    RF,NDEMNO                                                        
*                                                                               
DD050    ZIC   R0,NDEMOS                                                        
         SR    R1,R1                                                            
*                                                                               
DD060    LR    R4,R1                                                            
         MHI   R4,3                                                             
         LA    R4,DEMOS(R4)        LOOK FOR ONE OF OUR DEMO NOS                 
         CLC   0(3,RF),0(R4)                                                    
         BE    DD070                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,DD060                                                         
         B     DD080               NOT FOUND                                    
*                                                                               
DD070    LR    R5,R1               SAVE ESTIMATED DEMO VALUE                    
         SLL   R5,3                                                             
         LA    R5,ESTVALS(R5)                                                   
         MVC   0(4,R5),4(RF)                                                    
         LR    R5,R1               FORMAT DEMO VALUE                            
         MVC   HALF,=H'17'                                                      
         CLI   QOPT2,C'Y'                                                       
         BNE   *+10                                                             
         MVC   HALF,=H'8'                                                       
         MH    R5,HALF                                                          
         LA    R5,P+66(R5)                                                      
                                                                                
         L     R1,4(RF)                                                         
         N     R1,=X'3FFFFFFF'     DROP OVERRIDE FLAGS                          
         CLI   1(RF),C'R'          TEST RATING                                  
         BE    *+12                                                             
         CLI   1(RF),C'E'          OR EXTENDED RATING                           
         BNE   DD074                                                            
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DECIMAL REQUEST                       
         BO    DD072               YES                                          
         TM    4(RF),X'40'         1-DEC REQUEST, TEST 1-DEC VALUE              
         BZ    DD074               YES - DISPLAY                                
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         B     DD074                                                            
*                                                                               
DD072    TM    4(RF),X'40'         2-DEC REQ, TEST 2-DEC VALUE                  
         BO    *+8                                                              
         MHI   R1,10                                                            
         EDIT  (R1),(7,(R5)),2                                                  
         B     DD076                                                            
*                                                                               
DD074    EDIT  (R1),(7,(R5)),1     EDIT DEMO VALUE                              
*                                                                               
DD076    TM    4(RF),X'80'         TEST FOR MANUAL OVERRIDE                     
         BZ    DD080                                                            
         MVI   7(R5),C'*'                                                       
*                                                                               
DD080    LA    RF,8(RF)            NEXT DEMO IN ELEMENT                         
         BCT   R3,DD050                                                         
*                                                                               
DD090    LA   R7,BDAREA                                                         
         USING BDEXTD,R7                                                        
         MVC  P(3),BDPEST          EST                                          
         MVI  P+3,C'-'                                                          
         MVC  P+4(3),BDPLIN        LINE                                         
         MVC  P+8(11),BDPSDTE      BUY DATES                                    
         MVC  P+20(8),BDPDAY       DAY(S)                                       
         MVC  P+29(11),BDPTIME     TIME                                         
         MVC  P+41(18),BDPPROG     PROGRAMMING                                  
         MVC  P+60(5),=C'*EST*'                                                 
         DROP R7                                                                
*                                                                               
         LA   R4,P2                                                             
         LA   R8,BDELEM                                                         
         MVI  ELCDLO,X'10'         LOOK FOR AFFIDAVIT ELEMENTS                  
         MVI  ELCDHI,X'10'                                                      
         CLI  QOPT2,C'Y'           UNLESS RERATE REPORT, IN WHICH CASE          
         BNE  DD092                LOOK FOR SPOT ELEMENTS                       
         MVI  ELCDLO,6                                                          
         MVI  ELCDHI,8                                                          
         CLI  BUYKPRD,X'FF'                                                     
         BNE  DD092                                                             
         MVI  ELCDLO,11                                                         
         MVI  ELCDHI,13                                                         
*                                                                               
DD092    BAS   RE,GETEL                                                         
         BE    DD105                                                            
         CLI   QOPT2,C'Y'                                                       
         BE    *+14                                                             
         MVC   P2(19),=C'** NO AFFIDAVITS **'                                   
         B     *+10                                                             
         MVC   P2(14),=C'** NO SPOTS **'                                        
         MVI   ALLOWLIN,2                                                       
         GOTO1 REPORT                                                           
         B     DD130                                                            
*                                                                               
DD100    BAS   RE,NEXTEL           NEXT AFFIDAVIT OR SPOT ELEMENT               
         BNE   DD130                                                            
*                                                                               
DD105    CLI   QOPT2,C'Y'                                                       
         BE    DD106                                                            
         USING AFFELEM,R8                                                       
         CLC   ADATE,BQSTARTP      FILTER AFFIDAVIT ON REQUEST PERIOD           
         BL    DD100                                                            
         CLC   ADATE,BQENDP                                                     
         BH    DD100                                                            
         GOTO1 DATCON,DMCB,(2,ADATE),(4,11(R4))                                 
         GOTO1 (RF),(R1),(2,ADATE),(0,WORK)                                     
         LA    R5,20(R4)                                                        
         GOTO1 GETDAY,DMCB,WORK,(R5)                                            
         XC    FULL,FULL                                                        
         MVC   FULL(2),ATIME                                                    
         NI    FULL,X'0F'          DROP HOB FROM AFFID TIME                     
         LA    R5,29(R4)                                                        
         GOTO1 UNTIME,DMCB,FULL,(R5)                                            
         MVC   34(6,R4),SPACES                                                  
         B     DD108                                                            
         DROP  R8                                                               
*                                                                               
         USING REGELEM,R8                                                       
DD106    TM    RSTATUS,X'04'       DROP HIATUSES                                
         BO    DD100                                                            
         TM    RSTATUS,X'C0'       AND MINUS/MINUSED                            
         BNZ   DD100                                                            
         CLC   RDATE,BQSTARTP      FILTER SPOT ON REQUEST PERIOD                
         BL    DD100                                                            
         CLC   RDATE,BQENDP                                                     
         BH    DD100                                                            
         GOTO1 DATCON,DMCB,(2,RDATE),(4,11(R4))                                 
*                                                                               
DD108    BAS   RE,DEMOLOOK         LOOK UP THE DEMOS                            
*                                                                               
         MVC   41(L'SPLKPRG,R4),SPLKPRG   PROGRAM NAME(S)                       
*                                                                               
         MVI   DEMINDX,0           PRINT AVERAGE DATA                           
         LA    R2,AVGTOTS                                                       
         LA    R3,AVGVALS                                                       
         LA    R7,66(R4)                                                        
         LA    RE,DEMOS            POINT TO LIST OF DEMO CODES                  
*                                                                               
DD110    L     R5,0(R3)                                                         
         N     R5,=X'3FFFFFFF'     REMOVE FLAGS                                 
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DECIMAL REQUEST                       
         BZ    DD112               NO                                           
         CLI   1(RE),C'R'          TEST RATING                                  
         BE    *+12                                                             
         CLI   1(RE),C'E'                                                       
         BNE   DD112                                                            
         EDIT  (R5),(7,0(R7)),2                                                 
         B     DD114                                                            
*                                                                               
DD112    EDIT  (R5),(7,0(R7)),1                                                 
*                                                                               
DD114    TM    0(R3),X'80'         TEST MANUAL OVERRIDE                         
         BZ    *+8                                                              
         MVI   7(R7),C'*'                                                       
*                                                                               
         L     RF,0(R2)            ACCUMULATE TO TOTALS                         
         AR    RF,R5                                                            
         ST    RF,0(R2)                                                         
*                                                                               
         LA    R2,4(R2)                                                         
         LA    R3,8(R3)                                                         
         LA    RE,3(RE)            NEXT DEMO                                    
*                                                                               
         LHI   R0,8                                                             
         CLI   QOPT2,C'Y'                                                       
         BE    *+8                                                              
         LHI   R0,17                                                            
         AR    R7,R0                                                            
*                                                                               
         IC    R0,DEMINDX                                                       
         AHI   R0,1                                                             
         STC   R0,DEMINDX                                                       
         CLC   DEMINDX,NDEMOS                                                   
         BL    DD110                                                            
*                                                                               
         CLI   QOPT2,C'Y'          UNLESS RERATE REPORT,                        
         BE    DD128                                                            
*                                                                               
         MVI   DEMINDX,0           PRINT WEEKLY DATA                            
         LA    R2,WKYTOTS                                                       
         LA    R3,WKYVALS                                                       
         CLI   SPLKWKLY,C'Y'       TEST FOR WEEKLY DATA                         
         BE    *+8                                                              
         LA    R3,AVGVALS          NO - ACCUM AVG VALS TO WEEKLY TOTALS         
         LA    R7,74(R4)                                                        
         LA    RE,DEMOS                                                         
*                                                                               
DD123    L     R5,0(R3)                                                         
         N     R5,=X'3FFFFFFF'     DROP FLAGS                                   
         CLI   SPLKWKLY,C'Y'                                                    
         BNE   DD125                                                            
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DEC REQUEST                           
         BZ    DD124               NO                                           
         CLI   1(RE),C'R'          TEST RATING                                  
         BE    *+12                                                             
         CLI   1(RE),C'E'                                                       
         BNE   DD124                                                            
         EDIT  (R5),(7,0(R7)),2    ONLY PRINT REAL WEEKLY DATA                  
         B     DD125                                                            
*                                                                               
DD124    EDIT  (R5),(7,0(R7)),1    ONLY PRINT REAL WEEKLY DATA                  
*                                                                               
DD125    L     RF,0(R2)            ACCUMULATE TO TOTALS                         
         AR    RF,R5                                                            
         ST    RF,0(R2)                                                         
*                                                                               
         LA    R2,4(R2)                                                         
         LA    R3,8(R3)                                                         
         LA    R7,17(R7)                                                        
         LA    RE,3(RE)                                                         
         IC    R0,DEMINDX                                                       
         AHI   R0,1                                                             
         STC   R0,DEMINDX                                                       
         CLI   DEMINDX,4                                                        
         BL    DD123                                                            
*                                                                               
DD128    LA    RE,P2               NOW START PRINTING                           
         CR    R4,RE                                                            
         BNE   *+8                                                              
         MVI   ALLOWLIN,2                                                       
         GOTO1 REPORT                                                           
         LA    R4,P                                                             
         B     DD100                                                            
*                                                                               
DD130    MVI   P,0                                                              
         GOTO1 REPORT              SPACING LINE                                 
         B     EXIT                                                             
         EJECT                                                                  
DD200    CLI   MODE,STALAST        STATION LAST                                 
         BNE   DD300                                                            
         CLC   QBOOK1(3),=C'ACT'   TEST ACTUAL BOOKS                            
         BNE   DD202                                                            
         OC    ESTTOTS,ESTTOTS     YES - TEST NO DATA FOR STATION               
         BNZ   DD205                                                            
         L     R4,REPORT           YES - DUMMY CALL TO MEDAPRNT TO              
         MVC   HALF,0(R4)          CLEAR ITS TABLE.                             
         MVC   0(2,R4),=X'07FE'                                                 
         GOTO1 VMDAPRNT,DMCB,(RA),ACTBKTAB,1                                    
         MVC   0(2,R4),HALF                                                     
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
*                                                                               
DD202    OC    ESTTOTS,ESTTOTS     TEST DATA FOR THIS STATION                   
         BZ    EXIT                                                             
*                                                                               
DD205    MVI   P,0                 YES - PRINT                                  
         GOTO1 REPORT              SPACING LINE                                 
         MVC   P+52(8),BIGSTA                                                   
         MVC   P+61(5),=C'TOTAL'                                                
         MVC   P2+61(5),=C'*EST*'                                               
*                                                                               
         LA    R3,ESTTOTS          PRINT EST DEMO TOTS                          
         LA    R7,P2+66                                                         
         BAS   RE,EDITOTS                                                       
*                                                                               
         LA    R3,AVGTOTS          PRINT AVG DEMO TOTS                          
         LA    R7,P3+66                                                         
         BAS   RE,EDITOTS                                                       
*                                                                               
         CLI   QOPT2,C'Y'          UNLESS RERATE REPORT,                        
         BE    DD210                                                            
         OC    WKYTOTS,WKYTOTS     TEST FOR WEEKLY DATA                         
         BZ    DD210                                                            
         LA    R3,WKYTOTS          YES - PRINT WEEKLY DEMO TOTS                 
         LA    R7,P3+74                                                         
         BAS   RE,EDITOTS                                                       
*                                                                               
DD210    MVI   ALLOWLIN,4                                                       
         GOTO1 REPORT                                                           
*                                                                               
         CLC   QBOOK1(3),=C'ACT'                                                
         BNE   EXIT                                                             
         GOTO1 VMDAPRNT,DMCB,(RA),ACTBKTAB,1     PRINT THE ACTUAL BOOKS         
         B     EXIT                                                             
         EJECT                                                                  
DD300    B     EXIT                                                             
*                                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* MISC ROUTINES                                                                 
*                                                                               
         SPACE 1                                                                
GETEL    CLI   0(R8),0                                                          
         BNE   *+8                                                              
         LTR   RE,RE                                                            
         BR    RE                                                               
         CLC   0(1,R8),ELCDLO                                                   
         BL    NEXTEL                                                           
         CLC   0(1,R8),ELCDHI                                                   
         BH    NEXTEL                                                           
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
NEXTEL   SR    R0,R0                                                            
         ICM   R0,1,1(R8)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R8,R0                                                            
         B     GETEL                                                            
         SPACE 2                                                                
EDITOTS  NTR1                      ROUTINE TO EDIT TOTALS                       
*                                                                               
         LA    RE,DEMOS            POINT TO DEMO CODES                          
         SR    RF,RF                                                            
         IC    RF,NDEMOS                                                        
*                                                                               
ED010    L     R5,0(R3)                                                         
         N     R5,=X'3FFFFFFF'                                                  
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DECIMAL REQUEST                       
         BZ    ED020               NO                                           
         CLI   1(RE),C'R'                                                       
         BE    *+12                                                             
         CLI   1(RE),C'E'                                                       
         BNE   ED020                                                            
* ROUND TO 1-DECIMAL                                                            
         M     R4,=F'2'            X 2                                          
         D     R4,=F'10'                                                        
         AHI   R5,1                                                             
         SRL   R5,1                                                             
*                                                                               
ED020    EDIT  (R5),(7,(R7)),1                                                  
*                                                                               
ED022    LA    R3,4(R3)                                                         
         AHI   RE,3                                                             
*                                                                               
         LHI   R0,8                                                             
         CLI   QOPT2,C'Y'                                                       
         BE    *+8                                                              
         LHI   R0,17                                                            
         AR    R7,R0                                                            
         BCT   RF,ED010                                                         
         B     EXIT                                                             
         EJECT                                                                  
DEMOLOOK NTR1                                                                   
*                                                                               
*        ROUTINE TO LOOK UP ACTUAL DEMO VALUES                                  
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         XC    SPDEMLK,SPDEMLK                                                  
         CLI   BDPROGRM+17,0       TEST SPECIAL                                 
         BNE   *+12                                                             
         CLI   QOPT3,C'Y'          AND SUPPRESS WEEKLY LOOKUP                   
         BE    DL50                YES-USE BUYER'S DEMOS                        
         MVC   SPLKAREC,=A(LKWORK) WORKAREA                                     
         MVC   SPLKAFAC,ACOMFACS   COMFACS                                      
         MVC   SPLKALST,=A(DEMOS)  DEMO LIST                                    
         MVI   SPLKFIL,C'T'        FILE CODE                                    
         MVI   SPLKMED,C'T'        MEDIA CODE                                   
         MVC   SPLKUMK,BUYMSTA     AGENCY MARKET NUMBER                         
         MVC   SPLKBTYP,BKTYPE     SPECIAL BOOK TYPE                            
*                                                                               
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DECIMAL REQUEST                       
         BZ    *+8                                                              
         OI    SPLKOPT,SPLKOP2D                                                 
*                                                                               
         TM    BDCANAD,X'80'       TEST FOR CANADIAN                            
         BZ    *+8                                                              
         MVI   SPLKMED,C'C'        YES - MEDIA=C                                
         MVC   SPLKSRC,SVSELSRC    SOURCE CODE                                  
         MVC   SPLKAGY,BUYALPHA    AGENCY                                       
         MVC   SPLKCLI,CLT         CLIENT                                       
         ST    R6,SPLKABUY         PASS A(BUY RECORD)                           
         CLI   QOPT2,C'Y'          TEST RERATE REPORT                           
         BE    DL2                                                              
         ST    R8,SPLKAAFD         NO-PASS A(AFFIDAVIT ELEMENT)                 
         USING AFFELEM,R8                                                       
         MVC   HALF,ADATE                                                       
         B     DL4                                                              
         DROP  R8                                                               
*                                                                               
DL2      MVC   SPLKDAY,BDDAY       RERATE                                       
         MVC   SPLKTIM,BDTIMST                                                  
         USING REGELEM,R8                                                       
         MVC   HALF,RDATE                                                       
         DROP  R8                                                               
*                                                                               
DL4      MVC   SPLKDBK,BQBOOK1    MOVE REQUESTED BOOK IF ANY                    
         CLC   QBOOK1(3),=C'ACT'  TEST ACTUAL BOOK REQUEST                      
         BNE   DL32                                                             
*                                                                               
         LA    R4,ACTBKTAB         FIND ACTUAL BOOK FROM TABLE                  
*                                                                               
DL10     CLI   0(R4),0                                                          
         BE    DL30                                                             
         CLC   HALF,0(R4)                                                       
         BL    DL15                                                             
         CLC   HALF,2(R4)                                                       
         BH    DL15                                                             
         MVC   SPLKDBK,4(R4)                                                    
         B     DL30                                                             
DL15     AHI   R4,9                                                             
         B     DL10                                                             
*                                                                               
DL30     OC    SPLKDBK,SPLKDBK     TEST BOOK FOUND                              
         BZ    DL50                NO-USE BUYER'S DEMOS                         
*                                                                               
DL32     MVC   SPLKSTA(4),BIGSTA   STATION                                      
         MVI   SPLKSTA+4,C'T'                                                   
*                                                                               
         MVC   SPLKSVI,ESTSVI      DEFAULT SVI MONTH = ESTIMATE'S               
         CLC   QHUT1,SPACES                                                     
         BNH   DL41                                                             
         MVI   SPLKSVI,X'FF'       NO SVI ADJUSTMENTS                           
         CLC   QHUT1,=C'NO'                                                     
         BE    DL42                                                             
         PACK  DUB,QHUT1           SVI MONTH                                    
         CVB   R1,DUB                                                           
         STC   R1,SPLKSVI                                                       
*                                                                               
DL41     CLI   SPLKSVI,0           TEST AUTO-SVI                                
         BNE   DL42                                                             
         MVC   SPLKAUTF,BDWKIND    YES-SET FREQUENCY, START AND END             
         GOTO1 DATCON,DMCB,(3,BDSTART),(2,SPLKAUST)                             
         GOTO1 (RF),(R1),(3,BDEND),(2,SPLKAUND)                                 
*                                                                               
DL42     LA    R1,DEMLKXT                                                       
         ST    R1,SPLKXTND                                                      
         USING SPLKXTD,R1                                                       
         XC    SPXTHEAD,SPXTHEAD   CLEAR THIS!                                  
         CLI   SPLKSTA,C'0'        CABLE?                                       
         BL    DL43                NO                                           
         MVC   SPXTHEAD,SPLKSTA    SET THE HEADEND                              
         DROP  R1                                                               
         XC    SPLKSTA,SPLKSTA                                                  
         GOTO1 MSUNPK,DMCB,(X'80',BUYMSTA),WORK,WORK+4                          
         MVC   SPLKSTA(3),WORK+9   REPLACE THE STATION WITH THE NETWORK         
*                                                                               
DL43     XC    AVGVALS,AVGVALS     CALL GETDEMO FOR AVERAGE DATA                
         MVC   SPLKAVAL,=A(AVGVALS)                                             
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         MVI   WORK+5,C'I'         FORCE IMPRESSION BASED                       
         ST    R1,SPLKA1W                                                       
         GOTO1 VSPGETDM,DMCB,(X'FF',SPDEMLK)                                    
*                                                                               
         XC    WKYVALS,WKYVALS     CALL GETDEMO FOR WEEKLY DATA                 
         CLI   QOPT2,C'Y'          EXCEPT FOR RERATE REPORT                     
         BE    DLX                                                              
         MVC   SPLKAVAL,=A(WKYVALS)                                             
         L     R1,=A(SP1WPROF)                                                  
         ST    R1,SPLKA1W                                                       
         GOTO1 VSPGETDM,DMCB,(X'FF',SPDEMLK)                                    
         B     DLX                                                              
*                                                                               
DL50     MVC   AVGVALS,ESTVALS     FORCE BUYER'S DEMOS                          
         MVI   SPLKWKLY,C'N'                                                    
*                                                                               
DLX      B     EXIT                                                             
         SPACE 2                                                                
DAYTBL   DC    X'40201008040201'   DAY TABLE FOR GETDEMO CALLS                  
         EJECT                                                                  
*                                                                               
*        HEADLINE HOOK ROUTINE                                                  
*                                                                               
         DROP  RB,R9                                                            
         CNOP  0,4                                                              
         USING *,RF                                                             
HEADHK   NTR1                                                                   
*                                                                               
         LM    R9,RC,HDHKR9                                                     
         B     HH000                                                            
         DROP  RF                                                               
HDHKR9   DC    4F'0'                                                            
*                                                                               
         USING SPDD02,RB,R9                                                     
*                                                                               
HH000    LA    R4,H9+66                                                         
         CLI   QOPT2,C'Y'                                                       
         BNE   *+8                                                              
         LA    R4,H10+66                                                        
         LA    R5,DNAMES                                                        
         ZIC   R7,NDEMOS                                                        
*                                                                               
HH010    MVC   0(7,R4),0(R5)                                                    
         CLI   QOPT2,C'Y'                                                       
         BE    HH030                                                            
         GOTO1 CENTER,DMCB,(R4),15                                              
         LA    R3,15                                                            
         LR    R2,R4                                                            
*                                                                               
HH020    CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         MVI   0(R2),C'-'                                                       
         LA    R2,1(R2)                                                         
         BCT   R3,HH020                                                         
*                                                                               
         LA    R4,17(R4)                                                        
         B     HH040                                                            
*                                                                               
HH030    LA    R4,8(R4)                                                         
*                                                                               
HH040    LA    R5,7(R5)                                                         
         BCT   R7,HH010                                                         
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MAXDEMS  EQU   8                   MAXIMUM N'DEMOS                              
*                                                                               
         DS    0D                                                               
VSPGETDM DS    V                                                                
VMDAPRNT DC    V(VMDAPRNT)                                                      
*                                                                               
PARAS    DS    6F                                                               
ESTTOTS  DS    0CL(MAXDEMS*4)                                                   
         DS    (MAXDEMS)F                                                       
AVGTOTS  DS    0CL(MAXDEMS*4)                                                   
         DS    (MAXDEMS)F                                                       
WKYTOTS  DS    0CL(MAXDEMS*4)                                                   
         DS    (MAXDEMS)F                                                       
AVGVALS  DS    0CL(MAXDEMS*8)                                                   
         DS    (MAXDEMS*2)F                                                     
WKYVALS  DS    0CL(MAXDEMS*8)                                                   
         DS    (MAXDEMS*2)F                                                     
ESTVALS  DS    0CL(MAXDEMS*8)                                                   
         DS    (MAXDEMS*2)F                                                     
BDAREA   DS    CL132               BUY DESCRIPTION AREA                         
NDEMOS   DS    X                   N'DEMOS                                      
DNAMES   DS    CL(MAXDEMS*7)       DEMO NAMES                                   
DEMOS    DS    XL(MAXDEMS*3)       DEMO CODES                                   
         DS    X                   END OF DEMO LIST INDICATOR X'FF'             
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SVSELSRC DS    X                                                                
BKTYPE   DS    C                                                                
BQBOOK1  DS    XL2                                                              
ESTSVI   DC    X'00'                                                            
SP1WPROF DC    16C'Y'                                                           
ACTBKTAB DS    XL144               ACTUAL BOOK TABLE                            
DEMLKXT  DS    XL32                SAVE AREA FOR SPDEMLK                        
         EJECT                                                                  
       ++INCLUDE SPDEMLK                                                        
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         SPACE 2                                                                
LKWORK   DS    1000X               I/O AREA FOR DEMO LOOK-UP                    
DBWORK   DS    1500X               I/O AREA FOR DEMOCON                         
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
         PRINT ON                                                               
       ++INCLUDE SPMEDBDESD                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENBUY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDMASTC                                                                       
*                                                                               
DDMASTD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE SPDEMLKXTD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'085SPREPDD02 05/24/06'                                      
         END                                                                    
