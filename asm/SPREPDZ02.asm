*          DATA SET SPREPDZ02  AT LEVEL 006 AS OF 01/12/17                      
*PHASE SPDZ02B                                                                  
SPDZ02   TITLE 'SPDZ02 - FIX DEMO LOOKUP ELEMENTS FOR CANADIAN BUYS'            
                                                                                
*=================================================================              
* QOPT5=Y TO SEARCH FOR MISSING ALPHA MARKETS                                   
*=================================================================              
                                                                                
SPDZ02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPDZ02                                                         
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPDZ02+4096,RC                                                   
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
NO       LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
YES      CR    RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
REQF     ZAP   BUYCOUNT,=P'1'                                                   
         ZAP   CHGCOUNT,=P'0'                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),QAGY                                                    
         GOTO1 HIGHMKT                                                          
         L     R8,ADMARKET                                                      
         CLC   KEY(8),0(R8)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    BQSTART,BQSTART                                                  
         XC    BQEND,BQEND                                                      
         CLI   QSTART,C' '         TEST DATE ENTERED                            
         BNH   REQF10                                                           
         GOTO1 DATCON,DMCB,QSTART,(3,BQSTART)                                   
*                                                                               
REQF10   CLI   QEST,C'0'           TEST ESTIMATE ENTERED                        
         JL    REQFX               IF NOT NUMERIC, IGNORE                       
         PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STC   R0,BEST                                                          
                                                                                
REQFX    B     EXIT                                                             
*                                                                               
REQL     OI    BUYCOUNT+3,X'0F'                                                 
         UNPK  P(8),BUYCOUNT                                                    
         MVC   P+9(10),=C'TOTAL BUYS'                                           
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
CLTF     ZAP   BUYCOUNT,=P'0'                                                   
         XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(3),1(R6)                                                     
         MVI   KEY+3,X'FF'                                                      
         PACK  DUB,QMKT                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,KEY+4                                                       
         GOTO1 HIGH                                                             
         B     CLTF4                                                            
*                                                                               
CLTF2    GOTO1 SEQ                                                              
*                                                                               
CLTF4    CLC   KEY(6),KEYSAVE     SAME A-M/CLT/PRD/MKT                          
         BNE   CLTFX                                                            
         CLC   KEY(9),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA                      
         BE    CLTF10                                                           
*                                                                               
         MVC   SAVEKEY,KEY        SAVE BUY KEY                                  
         BAS   RE,GETSTA          READ STATION MASTER RECORD                    
*                                                                               
         MVC   KEY,SAVEKEY        RESTORE KEY                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CLTF10   CLI   BEST,0              TEST ANY ESTIMATE REQUESTED                  
         JE    CLTF10X                                                          
         CLC   KEY+9(1),BEST                                                    
         BNE   CLTF2                                                            
*                                                                               
CLTF10X  MVI   DMOUTBTS,X'FD'      DO NOT TEST DELETED                          
         MVI   DMINBTS,X'08'       REQUEST DELETED RECORDS                      
         GOTO1 GETBUY                                                           
*                                                                               
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
*                                                                               
         TM    15(R8),X'80'        TEST DELETED                                 
         BO    CLTF2                                                            
*                                                                               
         LA    R6,BDELEM           FIX STATION IN SPILL ELEMENTS                
         MVI   ELCDLO,3            SO NO FALSE POSITIVES ON COMPARES            
         MVI   ELCDHI,3                                                         
*                                                                               
CLTF12   BRAS  RE,NEXTEL                                                        
         BNE   CLTF14                                                           
         USING NDELEM,R6                                                        
         CLI   NDSTA,C'A'                                                       
         BNL   *+10                                                             
         XC    NDSTA,NDSTA                                                      
*                                                                               
         CLI   QOPT5,C'Y'          TEST MISSING ALPHA MKT SEARCH                
         BNE   CLTF12                                                           
         CLI   NDMKTALF,C'A'                                                    
         BNL   CLTF12                                                           
         MVC   PSPL(13),=C'BAD ALPHA MKT'                                       
         BAS   RE,PRTBUY                                                        
         B     CLTF2                                                            
*                                                                               
CLTF14   CLI   QOPT5,C'Y'          LOOKING FOR BAD ALPHA MKTS                   
         BE    CLTF2               YES - STOP PROCESSING NOW                    
*                                                                               
         L     RE,ADBUY                                                         
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         L     R0,=A(SAVEBUY)                                                   
         LA    R1,2(RF)                                                         
         MVCL  R0,RE               SAVE BUY RECORD                              
*                                                                               
         CLI   QSTART,C' '         TEST START DATE ENTERED                      
         BNH   CLTF20                                                           
         CLC   BDEND,BQSTART       IF BUY ENDS BEFORE START DATE                
         BL    CLTF2               DO NOT CONVERT                               
*                                                                               
CLTF20   CLC   KEY+4(2),BUYKEY+4    ARE WE DOING A SPILL POINTER                
         BE    CLTF22                                                           
         BAS   RE,FXSPL                                                         
         B     CLTF50                                                           
*                                                                               
CLTF22   CLI   BUYKSTA+2,X'B0'     TEST CABLE STATION                           
         BL    CLTF24              YES                                          
         BAS   RE,FXCBL                                                         
         BNZ   CLTF2               NON-ZERO CC MEANS DON'T CONVERT              
*                                                                               
E        USING DLUELEM,ELEM                                                     
*                                                                               
CLTF24   XC    ELEM,ELEM                                                        
*                                  FIND/SAVE EXISTING DLUELEM                   
         MVI   ELCDLO,X'24'                                                     
         MVI   ELCDHI,X'24'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   CLTF30                                                           
*                                                                               
         MVC   ELEM(11),0(R6)      MOVE EXISTING DLU ELEM                       
         GOTO1 RECUP,DMCB,ADBUY,(R6)    AND THEN DELETE IT                      
*                                                                               
CLTF30   MVI   E.DLUELEM,X'24'                                                  
         MVI   E.DLULEN,11         CHANGE LENGTH IF NEEDED                      
*                                                                               
         L     RE,ADMARKET                                                      
         USING MKTRECD,RE                                                       
*                                                                               
         L     RF,ADSTAT                                                        
         USING STARECD,RF                                                       
*                                                                               
         MVC   E.DLUBAMKT,MKTALF                                                
         CLI   BUYKSTA+2,X'B0'     FOR CABLE USE NDEF ALPHA MKT                 
         BL    *+10                                                             
         MVC   E.DLUBAMKT,SVAMKT                                                
*                                                                               
         LHI   R1,SQNORS2I                                                      
         MVI   E.DLUBFLGS,X'01'    SET BBM MARKET                               
         CLI   MKTRSVC,C'1'                                                     
         BE    *+12                                                             
         LHI   R1,SQNORS1I                                                      
         MVI   E.DLUBFLGS,X'02'    ELSE SET NSI MKT                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    SFLAG1,0  *EXECUTED*                                             
         BZ    *+8                                                              
         OI    E.DLUBFLGS,X'80'    SET TO SUPPRESS IMPS                         
*                                                                               
         MVC   E.DLUBSTOV,SRS2CALL USE BBM CALL LETTERS                         
         CLI   MKTRSVC,C'1'        TEST BBM                                     
         BE    *+10                YES                                          
         MVC   E.DLUBSTOV,SRS1CALL USE NSI CALL LETTERS                         
         DROP  RE,RF                                                            
*                                                                               
         GOTO1 RECUP,DMCB,ADBUY,ELEM,(R6)    INSERT NEW DLUELEM                 
*                                                                               
CLTF40   MVC   PRTGSVC,=C'BBM'     SET UP ELEMENT PRINTING                      
         TM    E.DLUBFLGS,X'01'                                                 
         BO    CLTF42                                                           
         MVC   PRTGSVC,=C'NSI'                                                  
         TM    E.DLUBFLGS,X'02'                                                 
         BO    CLTF42                                                           
         MVC   PRTGSVC,=C'...'                                                  
*                                                                               
CLTF42   MVI   PRTGSVC+3,C'/'                                                   
         MVC   PALPHA,E.DLUBAMKT                                                
         MVI   PALPHA+3,C'/'                                                    
         MVC   PSTOV,E.DLUBSTOV                                                 
         OC    PSTOV,=C'    '                                                   
         DROP  E                                                                
*                                                                               
CLTF50   AP    BUYCOUNT,=P'1'                                                   
*                                                                               
         L     RE,ADBUY                                                         
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         L     R4,=A(SAVEBUY)                                                   
         SR    R5,R5                                                            
         ICM   R5,3,13(R4)                                                      
         CLCL  R4,RE                                                            
         BE    CLTF52                                                           
*                                                                               
         GOTO1 PUTBUY              WRITE RECORD BACK IF CHANGED                 
         BAS   RE,PRTBUY                                                        
         AP    CHGCOUNT,=P'1'                                                   
*                                                                               
CLTF52   B     CLTF2                                                            
*                                                                               
CLTFX    MVC   P(3),CLIENT                                                      
         OI    BUYCOUNT+3,X'0F'                                                 
         UNPK  P+4(6),BUYCOUNT                                                  
         OI    CHGCOUNT+3,X'0F'                                                 
         UNPK  P+12(5),CHGCOUNT                                                 
         GOTO1 REPORT                                                           
         MVI   MODE,CLTLAST                                                     
         B     EXIT                                                             
         EJECT                                                                  
*==================================================================             
* READ STATION MASTER RECORD                                                    
*==================================================================             
                                                                                
GETSTA   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         GOTO1 MSUNPK,DMCB,SAVEKEY+4,WORK,WORK+4                                
         MVC   KEY+2(4),WORK+4                                                  
         MVC   STA(5),KEY+2        SAVE STATION CALL LETTERS HERE               
         MVC   KEY+6(1),QMED                                                    
         MVC   KEY+7(2),QAGY                                                    
         MVC   KEY+9(3),CLT                                                     
         MVC   KEY+12(3),=C'000'                                                
         GOTO1 READSTA                                                          
         B     EXIT                                                             
         EJECT                                                                  
*==================================================================             
* FIX SPILL DEMO ELEMENT                                                        
*==================================================================             
                                                                                
FXSPL    NTR1                                                                   
*                                                                               
         MVI   ELCDLO,3                                                         
         MVI   ELCDHI,3                                                         
         LA    R6,BDELEM                                                        
*                                                                               
FXSPL2   BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NDELEM,R6                                                        
         CLC   KEY+4(2),NDAGYMKT                                                
         BNE   FXSPL2                                                           
*                                                                               
         L     RE,ADMARKET                                                      
         USING MKTRECD,RE                                                       
*                                                                               
         L     RF,ADSTAT                                                        
         USING STARECD,RF                                                       
*                                                                               
         MVC   NDMKTALF,MKTALF                                                  
*                                                                               
         MVC   NDRTGSVC,MKTRSVC                                                 
*                                                                               
         LHI   R1,SQNORS2I                                                      
         CLI   MKTRSVC,C'1'        TEST BBM MARKET                              
         BE    *+8                                                              
         LHI   R1,SQNORS1I                                                      
*                                                                               
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    SFLAG1,0  *EXECUTED*                                             
         BZ    *+8                                                              
         OI    NDFLGS,X'80'        SET TO SUPPRESS IMPS                         
*                                                                               
         MVC   NDSTA,SRS2CALL      USE BBM CALL LETTERS                         
         CLI   MKTRSVC,C'1'        TEST BBM                                     
         BE    *+10                YES                                          
         MVC   NDSTA,SRS1CALL      USE NSI CALL LETTERS                         
*                                                                               
         MVC   PRTGSVC,=C'BBM'     SET UP ELEMENT PRINTING                      
         CLI   NDRTGSVC,C'1'                                                    
         BE    FXSPL10                                                          
         MVC   PRTGSVC,=C'NSI'                                                  
         CLI   NDRTGSVC,C'0'                                                    
         BE    FXSPL10                                                          
         MVC   PRTGSVC,=C'...'                                                  
*                                                                               
FXSPL10  MVI   PRTGSVC+3,C'/'                                                   
         MVC   PALPHA,NDMKTALF                                                  
         MVI   PALPHA+3,C'/'                                                    
         MVC   PSTOV,NDSTA                                                      
         OC    PSTOV,=C'    '                                                   
         MVC   PSPL,=C'SPILL'                                                   
         DROP  RE,RF                                                            
         B     EXIT                                                             
         EJECT                                                                  
*==================================================================             
* FIX CABLE                                                                     
*==================================================================             
                                                                                
FXCBL    NTR1                                                                   
         CLC   KEY(9),SVCBLKEY     TEST SAME CBLNET                             
         BE    FXCBL10                                                          
*                                                                               
         MVC   SVCBLKEY,KEY        SAVE CURRENT BUY KEY                         
*                                                                               
         XC    KEY,KEY                                                          
K        USING NDEFKEY,KEY                                                      
         MVC   K.NDEFKTYP,=X'0D11'                                              
         MVC   K.NDEFKAGY,QAGY                                                  
*                                                                               
         L     RE,ADSTAT                                                        
         MVC   K.NDEFKNET,STAKCALL-STARECD(RE)                                  
         MVC   K.NDEFKCLT,SAVEKEY+1                                             
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    FXCBL2                                                           
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    K.NDEFKCLT,K.NDEFKCLT DROP CLIENT                                
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  K                                                                
*                                                                               
FXCBL2   LA    R7,MYCBLDEF                                                      
         ST    R7,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         MVC   KEY,SVCBLKEY        RESTORE SPTDIR KEY                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETBUY                                                           
                                                                                
* FIND SUFFIX FOR THIS CODE                                                     
                                                                                
FXCBL10  BRAS  RE,SETSFX2          POINT R1 TO SUFFIX TABLE                     
         SR    RE,RE                                                            
         IC    RE,BUYKSTA+2                                                     
         SHI   RE,X'B0'                                                         
         MHI   RE,L'SUFXTAB2                                                    
         AR    RE,R1               POINT TO ENTRY                               
         MVC   CBLSUFX,0(RE)       SAVE 2 CHAR SUFFIX                           
*                                                                               
         LA    R7,MYCBLDEF                                                      
         AHI   R7,NDEFEL-NDEFRECD                                               
         USING NDEFEL01,R7                                                      
         SR    R0,R0                                                            
*                                                                               
FXCBL12  CLI   0(R7),1                                                          
         BNE   FXCBL14                                                          
*                                                                               
         CLC   CBLSUFX,NDEFMSUF    MATCH SUFFIX                                 
         BE    FXCBL16                                                          
*                                                                               
FXCBL14  IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BNE   FXCBL12                                                          
         B     NO                  IF NOT IN REC, DO NOT CONVERT                
*                                                                               
FXCBL16  OC    NDEFAMKT,NDEFAMKT   TEST ALPHA MARKET PRESENT                    
         BZ    NO                  IF NOT, DO NOT ADD DLUELEM                   
         MVC   SVAMKT,NDEFAMKT     SAVE THE ALPHA MARKET                        
         B     YES                                                              
         EJECT                                                                  
         EJECT                                                                  
         USING BUYRECD,R8                                                       
PRTBUY   NTR1                                                                   
         MVC   PAGY,BUYALPHA                                                    
         MVI   PMED,C'T'                                                        
         IC    R0,BUYKAM                                                        
         N     R0,=X'0000000F'                                                  
         CHI   R0,1                                                             
         BE    *+8                                                              
         MVI   PMED,C'N'                                                        
         MVC   PCLT,CLT                                                         
         MVC   PMKT,QMKT                                                        
         MVC   PSTA(5),STA                                                      
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
*                                                                               
         MVI   PEST+3,C'-'                                                      
         LLC   R0,BUYKBUY                                                       
         TM    BUYRCNTL,BUYRLN2    TEST 2-BYTE LINE NUMBER                      
         BZ    *+8                                                              
         ICM   R0,3,BUYKBUY                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         CLI   BUYKSTA+2,X'B0'     TEST CABLE                                   
         BL    PRTBUY10                                                         
         MVI   PSTA+4,C'/'                                                      
         MVC   PSTA+5(2),CBLSUFX                                                
         B     PRTBUY20                                                         
*                                                                               
PRTBUY10 SR    R0,R0                                                            
         IC    R0,BUYKAM                                                        
         N     R0,=X'0000000F'                                                  
         CHI   R0,3                TEST NETWORK                                 
         BNE   PRTBUY20                                                         
         OC    BUYKMKT,BUYKMKT     TEST MARKET 0                                
         JZ    PRTBUY20                                                         
*                                                                               
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
PRTBUY12 IC    R0,1(R6)            FIND NETWORK ELEM                            
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    PRTBUY20                                                         
         CLI   0(R6),X'68'                                                      
         BNE   PRTBUY12                                                         
         MVI   PSTA+4,C'/'                                                      
         MVC   PSTA+5(4),2(R6)                                                  
*                                                                               
PRTBUY20 GOTO1 REPORT                                                           
         J     EXIT                                                             
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         LLC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
         DS    0D                                                               
CBLSUFX  DS    H                                                                
CHGCOUNT DC    PL4'0'                                                           
BUYCOUNT DC    PL4'0'                                                           
SVAMKT   DS    CL4                                                              
MYSAVE   DS    XL32                                                             
TOTCOUNT DC    PL4'0'                                                           
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
         DS    0D                                                               
SAVEKEY  DS    XL24                                                             
SVCBLKEY DS    XL24                                                             
         DS    0D                                                               
ELEM     DS    XL64                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'MYCBLDEF'                                                    
MYCBLDEF DS    2000X                                                            
         LTORG                                                                  
*                                                                               
       ++INCLUDE SPCNCBLTAB                                                     
*                                                                               
         DS    0D                                                               
         DC    CL8'*SAVEBUY'                                                    
SAVEBUY  DS    6000C                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
* DSECT FOR PRINT LINE                                                          
         ORG   P                                                                
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL9                                                              
         DS    CL2                                                              
PERR     DS    CL10                                                             
         DS    CL1                                                              
PMSG     DS    CL20                                                             
         ORG   PERR+2                                                           
PRTGSVC  DS    CL3                                                              
         DS    CL1                                                              
PALPHA   DS    CL3                                                              
         DS    CL1                                                              
PSTOV    DS    CL4                                                              
         DS    CL1                                                              
PSPL     DS    CL5                                                              
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENNDEF                                                      
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPREPDZ02 01/12/17'                                      
         END                                                                    
