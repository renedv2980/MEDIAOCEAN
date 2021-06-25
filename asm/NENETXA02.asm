*          DATA SET NENETXA02  AT LEVEL 072 AS OF 05/01/02                      
*PHASE SPNX02A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'SPNX02 - ATT FILE DATA TRANSFER'                                
SPNX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPNX02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPNX02,RB,RC                                                     
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    IN100                                                            
         CLI   MODE,CLTFRST                                                     
         BE    RT100                                                            
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* RUNFRST PROCESSING                                                            
* ASK OPERATOR IF LOADS HAVE BEEN COMPLETED                                     
         SPACE                                                                  
IN100    DS    0H                                                               
         B     IN200    ******NOP OPERATOR COMMUNICATION ******                 
*                                                                               
         GOTO1 LOGIO,DMCB,1,(L'WARNING,WARNING)                                 
         B     IN140                                                            
IN120    GOTO1 LOGIO,DMCB,1,(L'EOJORGO,EOJORGO)                                 
*                                                                               
IN140    GOTO1 LOGIO,DMCB,0,(8,DUB)                                             
*                                                                               
         CLC   =C'GO',DUB                                                       
         BE    IN200                                                            
         CLC   =C'EOJ',DUB                                                      
         BNE   IN120                                                            
         GOTO1 AENDREQ                                                          
*                                                                               
WARNING  DC    C'****WARNING**** BE SURE THIS JOB SHOULD RUN. REPLY GO X        
               OR EOJ'                                                          
EOJORGO  DC    C'********** ENTER EOJ OR GO **********'                         
*                                                                               
IN200    DS    0H                                                               
*                                                                               
         OPEN  (FILEINS,(INPUT))                                                
         OPEN  (FILEOUTS,(OUTPUT))                                              
         OPEN  (FILEINN,(INPUT))                                                
         OPEN  (FILEOUTN,(OUTPUT))                                              
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         B     EXIT                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2004'                                  
         SPACE                                                                  
         EJECT                                                                  
* CLTFRST PROCESSSING                                                           
         SPACE                                                                  
RT100    DS    0H                                                               
         MVI   TOTIND,C'-'         SET FLAG FOR DELETING RECORDS                
*                                                                               
         BAS   RE,BLDEST           BUILD ATT ESTIMATE LIST                      
*                                                                               
RT120    DS    0H                                                               
         L     R0,ADBUY                                                         
         SH    R0,=H'4'                                                         
         GET   FILEINN,(R0)                                                     
         AP    INCNT,=P'1'                                                      
         SPACE                                                                  
* SET 2X'00' AT EOR                                                             
         L     RE,ADBUY                                                         
         SH    RE,=H'4'            POINT TO RECLEN                              
         AH    RE,0(RE)            POINT TO END OF REC                          
         XC    0(2,RE),0(RE)       CLEAR ELCODE AND LEN                         
*                                                                               
         L     R2,ADBUY                                                         
         CLI   0(R2),2             TEST PACKAGE RECORD                          
         BNE   RT200                                                            
         CLC   11(1,R2),BAGYMD     TEST AGENCY ATT                              
         BNE   RT220               NO - SAVE IT                                 
         AP    DELCNTP,=P'1'                                                    
         B     RT120                                                            
         SPACE 2                                                                
         USING NURECD,R2                                                        
RT200    CLI   0(R2),4             TEST AGENCY ATT UNIT                         
         BE    RT300               YES                                          
         CLC   =X'FFFFFFFF',0(R2)  TEST FILE TRAILER REC                        
         BE    RT120               YES - SKIP                                   
*                                                                               
RT220    L     R0,ADBUY                                                         
         SH    R0,=H'4'                                                         
         PUT   FILEOUTN,(R0)       PUT ALL NON-ATT RECS TO OUTPUT               
         AP    OUTCNT,=P'1'                                                     
         B     RT120               AND CONTINUE                                 
         SPACE 2                                                                
* THE FOLLOWING TEST DELETES ALL RECORDS UNDER CLIENT CODE ATT                  
* AND ALL RECORDS COPIED FROM OTHER SPTFILES                                    
         SPACE                                                                  
RT300    DS    0H                                                               
         CLC   1(1,R2),BAGYMD      TEST AGENCY ATT                              
         BNE   RT220               NO - SAVE IT                                 
         CLI   NUXFRAGY,0          TEST A COPIED RECORD                         
         BE    RT220               NO - KEEP IT                                 
         BAS   RE,GETCLTOT         EXTRACT DOLLAR VALUE                         
*                                                                               
         AP    DELCNTU,=P'1'       BUMP DELETE COUNTER                          
         B     RT120               AND READ NEXT RECORD                         
*                                                                               
RT400    CLOSE FILEINN                                                          
*                                                                               
* DELETE NETWORK GOAL RECORDS FROM SPOT TAPE                                    
RT500    DS    0H                                                               
         L     R0,ADBUY                                                         
         SH    R0,=H'4'                                                         
         GET   FILEINS,(R0)                                                     
         AP    INCNTS,=P'1'                                                     
* SET 2X'00' AT EOR                                                             
         L     RE,ADBUY                                                         
         SH    RE,=H'4'            POINT TO RECLEN                              
         AH    RE,0(RE)            POINT TO END OF REC                          
         XC    0(2,RE),0(RE)       CLEAR ELCODE AND LEN                         
*                                                                               
         L     R2,ADBUY                                                         
         CLI   0(R2),2             TEST GOAL  RECORD                            
         BNE   RT520                                                            
         CLC   1(1,R2),BAGYMD      TEST AGENCY ATT                              
         BNE   RT520               YES - DELETE IT                              
         AP    DELCNTG,=P'1'       BUMP DELETE COUNTER                          
         B     RT500               AND READ NEXT RECORD                         
*                                                                               
RT520    CLC   =X'FFFFFFFF',0(R2)  TEST FILE TRAILER REC                        
         BE    RT500               YES - SKIP                                   
         L     R0,ADBUY                                                         
         SH    R0,=H'4'                                                         
         PUT   FILEOUTS,(R0)       PUT ALL NON-ATT RECS TO OUTPUT               
         AP    OUTCNTG,=P'1'                                                    
         B     RT500               AND CONTINUE                                 
         SPACE                                                                  
         EJECT                                                                  
* NOW PROCESS DATA FROM OTHER SPTFILES                                          
* EACH BUY RECORD WILL GENERATE TWO RECORDS ON THE ATT FILE                     
* ONE UNDER CLIENT ATT AND ONE UNDER THE CLIENT=AGENCY                          
         SPACE                                                                  
PD100    DS    0H                                                               
         CLOSE FILEINS              AND CLOSE INPUT TAPE                        
*                                                                               
         L     R2,ADBUY                                                         
         MVI   0(R2),X'FF'         SET EOF REC                                  
         BAS   RE,GETCLTOT         FORCE LAST CLT TO BUFFER                     
         SPACE                                                                  
* MUST READ CLIENT HEADERS FOR DDS ATT AGENCIES NOW                             
* WHILE STILL ON THE ATT SPTFILE AND SAVE THEM                                  
         SPACE                                                                  
PD200    XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         LA    R4,ATAGYTAB                                                      
         USING AGYTABD,R4                                                       
*                                                                               
PD300    DS    0H                                                               
         GOTO1 CLPACK,DMCB,AGYCLT,AGYCLTP    GET PACKED CLIENT CODE             
         MVC   KEY+2(2),AGYCLTP              AND MOVE TO KEY                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AREC,AGYADCLT       SET CLTHDR SAVE ADDRESS                      
         GOTO1 GET                                                              
*                                                                               
         LA    R4,AGYTABL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         BNE   PD300                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
* FILL IN BINARY PRODUCT CODES IN AORTAB AND SORT *                             
* BINARY PRODUCT CODES ARE THE ATT/AGY VALUES     *                             
         SPACE                                                                  
         LA    R1,AORTAB           POINT TO TABLE                               
         LA    R0,AORTABCT         NUMBER OF ENTRIES                            
*                                                                               
PD400    BAS   RE,SETAOR                                                        
         LA    R1,AORTABL(R1)                                                   
         BCT   R0,PD400                                                         
         SPACE                                                                  
* SORT ON BINARY PRD CODE                                                       
         SPACE                                                                  
         GOTO1 XSORT,DMCB,AORTAB,AORTABCT,AORTABL,1,3                           
*                                                                               
         BAS   RE,BLDNET           READ ALL ATT NETWORKS AND SAVE               
*                                                                               
         L     RE,=A(PRDTABSV)                                                  
         ST    RE,SVAGYPRD                                                      
         LA    RE,ATAGYTAB                                                      
         B     RG520                                                            
         EJECT                                                                  
**************************************************************                  
*              OPEN SPOT FILES AND PROCESS RECORDS           *                  
**************************************************************                  
         SPACE 2                                                                
SF100    DS    0H                  CLOSE PREVIOUS SPOT SYSTEM                   
         GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'SPOT'                                 
*                                                                               
         XC    CNDATA,CNDATA                                                    
         LA    R4,CNDATA                                                        
         USING CND,R4                                                           
         MVC   CNAGY(2),SVAGYA     MOVE ALPHA AGENCY CODE                       
*                                                                               
         GOTO1 CONFID,DMCB,(R4),(1,FULL)                                        
         OC    FULL,FULL           TEST ID FOUND                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,UTL                                                           
         MVC   4(1,RE),CNNSE       MOVE SPOT SYSTEM NUMBER                      
         MVC   SVAGYB,CNNCD        AND SAVE SPOT AGENCY NUMBER                  
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',FLIST,ADBUY                     
*                                                                               
         XC    KEY,KEY                                                          
         IC    R0,SVAGYB                                                        
         STC   R0,KEY+1            SET AGENCY IN KEY (LEFT ALIGN)               
         OI    KEY+1,X'03'         SET MEDIA = SPOT                             
         MVC   KEY+2(2),BCLT                                                    
         GOTO1 HIGH                READ ATT CLTHDR                              
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R8,=A(AGCLTHDR)                                                  
         ST    R8,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         BAS   RE,BLDPRD           BUILD PRODUCT TRANSLATE TABLE                
*                                                                               
         L     RE,SVAGYPRD         SAVE CONVERTED PRODUCTS FOR NET PASS         
         L     RF,=A(PRDTAB)                                                    
         MVC   0(255,RE),0(RF)                                                  
         LA    RE,256(RE)                                                       
         ST    RE,SVAGYPRD                                                      
*                                                                               
         ZAP   AGYCNT,=P'0'        RESET AGENCY RECORD COUNTER                  
         EJECT                                                                  
* READ ALL CLIENT ATT GOAL RECORDS *                                            
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+1(3),1(R8)      SET A-M/CLT                                  
         GOTO1 HIGH                                                             
         B     RG200                                                            
*                                                                               
RG100    GOTO1 SEQ                                                              
*                                                                               
RG200    CLC   KEY(4),KEYSAVE      SAME A-M/CLT                                 
         BNE   RG500                                                            
*                                                                               
         L     R2,ADGOAL                                                        
         USING GOALRECD,R2                                                      
*                                                                               
         ST    R2,AREC                                                          
         GOTO1 GET                                                              
*--FOR Y&N AGENCY ALTER ESTIMATE NUMBERS BETWEEN                                
*--4-10 BY 160.                                                                 
         CLC   SVAGYA(2),=C'YN'                                                 
         BNE   RG300                                                            
         ZIC   RE,GKEYEST                                                       
         LA    RF,160                                                           
         CLI   GKEYEST,4                                                        
         BL    RG300                                                            
         CLI   GKEYEST,10                                                       
         BH    RG300                                                            
         AR    RE,RF                                                            
         STC   RE,GKEYEST                                                       
*                                                                               
RG300    ZIC   R0,GKEYEST          GET ESTIMATE NUMBER                          
         BAS   RE,XAEST            VALIDATE POL EST OPEN                        
         BNE   RG100               IF EST NOT OPEN FOR ATT, SKIP REC            
*                                                                               
         MVC   GKEYMKT,=H'7777'    MAKE ALL ATT GOAL MKT 7777                   
*                                                                               
         MVI   PRDERRSW,0          RESET FATAL ERROR SWITCH                     
         MVI   AORSW,C'N'          SET NOT AOR SWITCH                           
         MVC   APRDTAB,=A(PRDTAB)  SET PRDTAB TABLE ADDRESS                     
*                                                                               
         LA    R7,GKEYPRD          POINT TO PRD 1                               
         BAS   RE,FINDPRD                                                       
         CLI   PRDERRSW,C'Y'       TEST ERROR                                   
         BE    RG100               YES - SKIP RECORD                            
*                                                                               
         LA    R7,GKEYPRD2         POINT TO PRD 2                               
         CLI   0(R7),0                                                          
         BE    *+8                                                              
         BAS   RE,FINDPRD                                                       
         CLI   PRDERRSW,C'Y'       TEST ERROR                                   
         BE    RG100               YES - SKIP RECORD                            
         EJECT                                                                  
* GENERATE RECORD FOR CLIENT ATT *                                              
         SPACE                                                                  
         MVC   1(1,R2),BAGYMD        MOVE ATT AGY/MED TO REC                    
         MVC   2(2,R2),BCLT          SET CLIENT = ATT                           
         MVC   GOALREC+20(2),=C'AT'  AGYALPHA BECOMES ATT                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,13(R2)         GET REC LENGTH                               
         AH    R0,=H'4'                                                         
         L     R4,ADGOAL                                                        
         SH    R4,=H'4'                                                         
         SLL   R0,16               LEFT ALIGN                                   
         ST    R0,0(R4)            SET REC LENGTH FOR SORT                      
*                                                                               
         AP    AGYCNT,=P'1'                                                     
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)    AND PUT RECORD 1                 
         SPACE                                                                  
* NOW GENERATE ATT/AGY RECORD *                                                 
         SPACE                                                                  
         MVI   AORSW,C'Y'          INDICATE AOR SEARCH                          
         LA    R7,GKEYPRD                                                       
         BAS   RE,FINDPRD                                                       
*                                                                               
         L     RE,SVAORPRD                 POINT TO PRD1 AORTAB ENTRY           
         ICM   RE,15,AORADAGY-AORTABD(RE)  POINT TO AGYTAB ENTRY                
         MVC   GOALREC+2(2),AGYCLTP-AGYTABD(RE)  AND MOVE CLT CODE              
*                                                                               
         LA    R7,GKEYPRD2                                                      
         CLI   0(R7),0             TEST PIGGYBACK                               
         BE    *+8                 NO                                           
         BAS   RE,FINDPRD                                                       
*                                                                               
         AP    AGYCNT,=P'1'                                                     
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)                                     
         CLI   QOPT1,C'Y'          SEE IF TEST RUN                              
         BNE   RG100                                                            
         CP    AGYCNT,=P'50'                                                    
         BL    RG100                                                            
*                                                                               
RG500    L     RE,SVATTAGY                                                      
         LA    RE,AGYTABL(RE)   NEXT ENTRY                                      
         USING AGYTABD,RE                                                       
*                                                                               
RG520    CLI   0(RE),X'FF'         TEST EOL                                     
         BE    SG100                                                            
         ST    RE,SVATTAGY                                                      
         MVC   SVAGYA,AGYAGY       SET NEW AGYALPHA                             
         MVI   SVAGYA+2,C' '                                                    
         MVC   WORK(3),AGYCLT      MOVE EBCDIC CLIENT CODE                      
         MVC   SVAGYCLT,AGYCLTP    AND PACKED CODE                              
         B     SF100                                                            
         DROP  R2,RE                                                            
         EJECT                                                                  
* ALL AGENCY FILES HAVE BEEN PROCESSED - SORT RECORDS AND WRITE TO TAPE         
SG100    MVI   TOTIND,C'+'         INDICATE ADDING RECORDS                      
*                                                                               
SG120    DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R0,15,4(R1)                                                      
         BZ    SG300                                                            
         LR    R4,R0                                                            
         PUT   FILEOUTS,(R0)                                                    
*        LA    R4,4(R4)                                                         
*        SR    R5,R5                                                            
*        ICM   R5,3,13(R4)                                                      
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R5),=C'2D'                
         AP    OUTCNT,=P'1'                                                     
         AP    ADDCNTG,=P'1'                                                    
         B     SG120                                                            
*                                                                               
SG300    DS    0H                  PRINT FINAL TOTALS                           
         CLOSE FILEOUTS                                                         
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         EJECT                                                                  
**************************************************************                  
*             OPEN NET FILES AND PROCESS DATA                *                  
**************************************************************                  
         SPACE 2                                                                
NF100    L     RE,=A(PRDTABSV)                                                  
         ST    RE,SVAGYPRD                                                      
         BAS   RE,XAAGYTOT         1ST TIME INITIALIZE                          
         LA    RE,ATAGYTAB                                                      
         B     RU520                                                            
*                                                                               
NF200    DS    0H                  CLOSE PREVIOUS SPOT SYSTEM                   
         GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'SPOT'                                 
*                                                                               
         XC    CNDATA,CNDATA                                                    
         LA    R4,CNDATA                                                        
         USING CND,R4                                                           
         MVC   CNAGY(2),SVAGYA     MOVE ALPHA AGENCY CODE                       
*                                                                               
         GOTO1 CONFID,DMCB,(R4),(1,FULL)                                        
         OC    FULL,FULL           TEST ID FOUND                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,UTL                                                           
         MVC   4(1,RE),CNNSE       MOVE NET SYSTEM NUMBER                       
         MVC   SVAGYB,CNNCD        AND SAVE NET AGENCY NUMBER                   
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',FLIST2,ADBUY                    
*                                                                               
         L     RE,SVAGYPRD         SAVE CONVERTED PRODUCTS FOR NET PASS         
         L     RF,=A(PRDTAB)                                                    
         MVC   0(256,RF),0(RE)                                                  
         LA    RE,256(RE)                                                       
         ST    RE,SVAGYPRD                                                      
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* READ ALL CLIENT ATT PACKAGES & TRANSLATE                            *         
*                                                                     *         
* ON PACAGE RECORD CHECK FOR ACCEPTABLE NETWORK/ESTIMATE/PACKAGE      *         
* READ PACKAGES FOR NETWORK/ESTIMATE - BUILD A TABLE OF GOOD PACKAGES *         
* ONLY 25 ALLOWED - BREAK AT NETWORK/ESTIMATE AND READ UNITS          *         
*                                                                     *         
***********************************************************************         
RP100    XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         IC    R0,SVAGYB                                                        
         STC   R0,KEY+11           SET AGENCY IN KEY (LEFT ALIGN)               
         OI    KEY+11,X'03'        SET MEDIA = NETWORK                          
         MVC   KEY+12(2),BCLT                                                   
RP120    MVI   ENDPK,C'0'                                                       
         BAS   RE,UNHIGH                                                        
         MVC   PKKEY,KEY                                                        
         MVI   LSTPKNUM,0          RESET LAST PACKAGE #                         
         B     RP220               SKIP IF NO BUYS                              
*                                                                               
RP200    BAS   RE,UNSEQ                                                         
*                                                                               
RP220    CLC   KEY(14),KEYSAVE     SAME A-M/CLT                                 
         BE    *+12                                                             
         MVI   ENDPK,C'Y'                                                       
         B     RU100                                                            
*                                                                               
         CLC   KEY(19),PKKEY       SAME KEY THRU ESTIMATE                       
         BNE   RU100                                                            
*                                                                               
         L     R2,ADBUY                                                         
         USING NPRECD,R2                                                        
         ST    R2,AREC                                                          
         BAS   RE,UNGET                                                         
         MVC   SVPKCLT,NPKCLT                                                   
         MVC   SVPKEST,NPKEST                                                   
         MVC   SVPKNET,NPKNET                                                   
*                                                                               
         LA    R4,NPKNET                                                        
         BAS   RE,CHKNET                                                        
         BE    RP240                                                            
         ZIC   RE,KEY+17                                                        
         LA    RE,1(RE)                                                         
         STC   RE,KEY+17                                                        
         XC    KEY+18(2),KEY+18                                                 
         B     RP120                                                            
*--FOR Y&N AGENCY ALTER ESTIMATE NUMBERS BETWEEN                                
*--4-10 BY 160.                                                                 
RP240    CLC   SVAGYA(2),=C'YN'                                                 
         BNE   RP250                                                            
         ZIC   RE,NPKEST                                                        
         LA    RF,160                                                           
         CLI   NPKEST,4                                                         
         BL    RP250                                                            
         CLI   NPKEST,10                                                        
         BH    RP250                                                            
         AR    RE,RF                                                            
         STC   RE,NPKEST                                                        
*                                                                               
RP250    ZIC   R0,NPKEST            GET ESTIMATE NUMBER                         
         BAS   RE,XAEST             VALIDATE POL EST OPEN                       
         BE    RP260                                                            
         SR    RE,RE                                                            
         MVC   DUB(2),KEY+17                                                    
         LH    RE,DUB                                                           
         LA    RE,1(RE)                                                         
         STH   RE,DUB                                                           
         MVC   KEY+17(2),DUB                                                    
         MVI   KEY+19,0                                                         
         B     RP120                                                            
*                                                                               
RP260    CLI   ENDPK,C'N'                                                       
         TM    NPAKSTAT,X'20'      LOCKED NEXT RECORD                           
         BO    RP200                                                            
*                                                                               
         CLI   NPKPACK,25                                                       
         BNH   RP300                                                            
         CLI   LSTPKNUM,25                                                      
         BNL   RP280                                                            
         ZIC   RE,LSTPKNUM                                                      
         LA    RE,1(RE)                                                         
         STC   RE,NPKPACK                                                       
         B     RP300                                                            
*                                                                               
RP280    MVC   ERRKEY(3),SVAGYA                                                 
         MVC   ERRKEY+4(4),NPKNET                                               
         ZIC   R0,NPKEST           EST                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ERRKEY+9(3),DUB                                                  
         ZIC   R0,NPKPACK                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ERRMSG(3),DUB                                                    
         MVC   ERRMSG+4(28),=C'** ERROR ** PACKAGE OVER 25'                     
         GOTO1 REPORT                                                           
         B     RP200                                                            
*                                                                               
RP300    LA    R0,24               TABLE OF EXCEPTABLE PAKAGES                  
         LA    R1,GDPKGE                                                        
RP320    CLI   0(R1),0                                                          
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,RP320                                                         
         MVC   0(1,R1),NPKPACK                                                  
*                                                                               
         MVC   LSTPKNUM,NPKPACK                                                 
         ZIC   RE,NPKPACK           GET ESTIMATE NUMBER                         
         MH    RE,=H'10'                                                        
         L     RF,SVATTAGY                                                      
         USING AGYTABD,RF                                                       
         ZIC   R0,AGYPKGN                                                       
         AR    RE,R0                                                            
         STC   RE,NPKPACK                                                       
         DROP  RF                                                               
         SPACE                                                                  
* GENERATE RECORD FOR CLIENT ATT *                                              
         MVC   11(1,R2),BAGYMD     MOVE ATT AGY/MED TO REC                      
         MVC   12(2,R2),BCLT       SET CLIENT = ATT                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,NPKRLEN        GET REC LENGTH                               
         AH    R0,=H'4'                                                         
         L     R4,ADBUY                                                         
         SH    R4,=H'4'                                                         
         SLL   R0,16               LEFT ALIGN                                   
         ST    R0,0(R4)            SET REC LENGTH FOR SORT                      
*                                                                               
         AP    AGYCNT,=P'1'                                                     
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)    AND PUT RECORD 1                 
         SPACE                                                                  
* NOW GENERATE ATT/AGY RECORD *                                                 
*        L     RE,SVAORPRD                 GET AORTAB ENTRY                     
*        ICM   RE,15,AORADAGY-AORTABD(RE)  POINT TO AGYTAB ENTRY                
*        MVC   12(2,R2),AGYCLTP-AGYTABD(RE) MOVE CLIENT CODE                    
*                                                                               
*        AP    AGYCNT,=P'1'                                                     
*        GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)                                     
         B     RP200                                                            
         DROP  R2                                                               
         EJECT                                                                  
* READ ALL CLIENT ATT UNIT RECORDS *                                            
RU100    MVC   PKKEY,KEY           SAVE LAST PACKAGE KEY                        
         CLI   GDPKGE,0                                                         
         BE    RU240                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'94'                                                        
         IC    R0,SVAGYB                                                        
         STC   R0,KEY+1            SET AGENCY IN KEY (LEFT ALIGN)               
         OI    KEY+1,X'03'         SET MEDIA = NETWORK                          
         MVC   KEY+2(2),SVPKCLT                                                 
         MVC   KEY+4(1),SVPKEST                                                 
         MVC   KEY+5(4),SVPKNET                                                 
RU120    BAS   RE,UNHIGH                                                        
         B     RU220               SKIP IF NO BUYS                              
*                                                                               
RU200    BAS   RE,UNSEQ                                                         
*                                                                               
RU220    CLC   KEY(4),KEYSAVE      SAME A-M/CLT                                 
         BNE   RU500                                                            
*                                                                               
         CLC   KEY(9),KEYSAVE      SAME NETWORK ESTIMATE                        
         BE    RU300                                                            
RU240    XC    GDPKGE,GDPKGE                                                    
         MVC   KEY,PKKEY                                                        
         CLI   ENDPK,C'Y'                                                       
         BE    RU500                                                            
         B     RP120                                                            
*                                                                               
RU300    CLC   =X'AD3C',KEY+(NUKDDATE-NUKDKEY)                                  
         BH    RU200               BEFORE SEP28/86 - REJECT                     
*                                                                               
         L     R2,ADBUY                                                         
         USING NURECD,R2                                                        
         ST    R2,AREC                                                          
         BAS   RE,UNGET                                                         
*--FOR Y&N AGENCY ALTER ESTIMATE NUMBERS BETWEEN                                
*--4-10 BY 160.                                                                 
         CLC   SVAGYA(2),=C'YN'                                                 
         BNE   RU310                                                            
         ZIC   RE,NUKEST                                                        
         LA    RF,160                                                           
         CLI   NUKEST,4                                                         
         BL    RU310                                                            
         CLI   NUKEST,10                                                        
         BH    RU310                                                            
         AR    RE,RF                                                            
         STC   RE,NUKEST                                                        
*                                                                               
RU310    CLI   NUPRD,0             NO PRODUCT REJECT                            
         BE    RU200                                                            
         ICM   RF,15,NUACTUAL      NO ACTUAL DOLLARS REJECT                     
         BNZ   *+12                                                             
         TM    NUUNITST,X'20'                                                   
         BNO   RU200                                                            
*                                                                               
         LA    R0,25               MAKE SURE PAKAGE WAS GOOD                    
         LA    R1,GDPKGE                                                        
RU320    CLC   0(1,R1),NUPACK                                                   
         BE    RU340                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,RU320                                                         
         B     RU200                                                            
*                                                                               
RU340    ZIC   RE,NUPACK           GET PACKAGE NUMBER                           
         MH    RE,=H'10'                                                        
         L     RF,SVATTAGY                                                      
         USING AGYTABD,RF                                                       
         ZIC   R0,AGYPKGN                                                       
         AR    RE,R0                                                            
         STC   RE,NUPACK                                                        
         DROP  RF                                                               
*                                                                               
         LA    R4,NUKNET                                                        
         BAS   RE,CHKNET                                                        
*                                                                               
         MVI   PRDERRSW,0          RESET FATAL ERROR SWITCH                     
         MVI   AORSW,C'N'          INDICATE NOT AOR SEARCH                      
         MVC   APRDTAB,=A(PRDTAB)  SET PRDTAB TABLE ADDRESS                     
         BAS   RE,XAPRD            TRANSLATE PRD CODES                          
         CLI   PRDERRSW,C'Y'       TEST ERROR                                   
         BE    RU200               YES - SKIP RECORD                            
*                                                                               
         MVC   NUXFRAGY,XFRAGY     SET TRANSFER FLAG IN BUYREC                  
         MVC   NUALPHA,=C'AT'      AGYALPHA BECOMES ATT                         
         SPACE                                                                  
* GENERATE RECORD FOR CLIENT ATT *                                              
         MVC   1(1,R2),BAGYMD      MOVE ATT AGY/MED TO REC                      
         MVC   2(2,R2),BCLT        SET CLIENT = ATT                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,NURLEN         GET REC LENGTH                               
         AH    R0,=H'4'                                                         
         L     R4,ADBUY                                                         
         SH    R4,=H'4'                                                         
         SLL   R0,16               LEFT ALIGN                                   
         ST    R0,0(R4)            SET REC LENGTH FOR SORT                      
*                                                                               
         AP    AGYCNT,=P'1'                                                     
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)    AND PUT RECORD 1                 
         SPACE                                                                  
* NOW GENERATE ATT/AGY RECORD *                                                 
         MVI   AORSW,C'Y'          INDICATE AOR SEARCH                          
         BAS   RE,XAPRD            TRANSLATE PRD CODES                          
         CLI   PRDERRSW,C'Y'       TEST ERROR                                   
         BE    RU200               YES - SKIP RECORD                            
*                                                                               
         SPACE                                                                  
* SINCE BRAND POL, LAST PRODUCT GIVES AOR *                                     
         L     RE,SVAORPRD                 GET AORTAB ENTRY                     
         ICM   RE,15,AORADAGY-AORTABD(RE)  POINT TO AGYTAB ENTRY                
         MVC   2(2,R2),AGYCLTP-AGYTABD(RE) MOVE CLIENT CODE                     
*                                                                               
         AP    AGYCNT,=P'1'                                                     
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)                                     
         CLI   QOPT1,C'Y'          SEE IF TEST RUN                              
         BNE   RU200                                                            
         CP    AGYCNT,=P'200'                                                   
         BL    RU200                                                            
*                                                                               
RU500    DS    0H                                                               
         BAS   RE,XAAGYTOT                                                      
*                                                                               
         L     RE,SVATTAGY                                                      
         LA    RE,AGYTABL(RE)   NEXT ENTRY                                      
         USING AGYTABD,RE                                                       
*                                                                               
RU520    CLI   0(RE),X'FF'         TEST EOL                                     
         BE    SR100                                                            
         ST    RE,SVATTAGY                                                      
         MVC   SVAGYA,AGYAGY       SET NEW AGYALPHA                             
         MVI   SVAGYA+2,C' '                                                    
         MVC   WORK(3),AGYCLT      MOVE EBCDIC CLIENT CODE                      
         MVC   SVAGYCLT,AGYCLTP    AND PACKED CODE                              
         B     NF200                                                            
         DROP  R2,RE                                                            
         EJECT                                                                  
* ALL AGENCY FILES HAVE BEEN PROCESSED - SORT RECORDS AND WRITE TO TAPE         
SR100    MVI   TOTIND,C'+'         INDICATE ADDING RECORDS                      
*                                                                               
SR120    DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R0,15,4(R1)                                                      
         BZ    SR300                                                            
         LR    R2,R0                                                            
         PUT   FILEOUTN,(R0)                                                    
         AP    OUTCNT,=P'1'                                                     
* MOVE RECORD *                                                                 
         L     R1,ADBUY                                                         
         SH    R1,=H'4'                                                         
         LH    RE,0(R2)            GET LENGTH                                   
SR200    CH    RE,=H'256'                                                       
         BNH   SR220                                                            
         MVC   0(256,R1),0(R2)                                                  
         LA    R1,256(R1)                                                       
         LA    R2,256(R2)                                                       
         SH    RE,=H'256'                                                       
         B     SR200                                                            
SR220    BCTR  RE,0                                                             
         EX    RE,MVCREC                                                        
*                                                                               
         LA    R1,1(RE,R1)         POINT TO END OF REC                          
         XC    0(2,R1),0(R1)       CLEAR NEXT ELEM CODE/LENGTH                  
*                                                                               
         L     R1,ADBUY                                                         
         CLI   0(R1),4                                                          
         BNE   SR240                                                            
         AP    ADDCNTU,=P'1'                                                    
         BAS   RE,GETCLTOT                                                      
*        L     R4,ADBUY                                                         
*        SR    R5,R5                                                            
*        ICM   R5,3,NURLEN-NUKEY(R4)                                            
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R5),=C'2D'                
         B     SR120                                                            
*                                                                               
SR240    AP    ADDCNTP,=P'1'                                                    
*                                                                               
         B     SR120                                                            
MVCREC   MVC   0(0,R1),0(R2)  *EXECUTED*                                        
*                                                                               
SR300    DS    0H                  PRINT FINAL TOTALS                           
         CLOSE FILEOUTN                                                         
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
         L     R2,ADBUY                                                         
         MVI   0(R2),X'FF'         PASS EOF TO TOT ROUTINES                     
         BAS   RE,GETCLTOT                                                      
*                                                                               
         BAS   RE,XATOTPRT                                                      
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,CTRS                                                          
*                                                                               
SR400    MVC   P(20),4(R4)                                                      
         EDIT  (P4,0(R4)),(8,P+22)                                              
         GOTO1 REPORT                                                           
         LA    R4,L'CTRS(R4)                                                    
         LA    R0,CTRX                                                          
         CR    R4,R0                                                            
         BL    SR400                                                            
         B     EXIT                                                             
         EJECT                                                                  
* BUILD TABLE OF ALL CLIENT ATT ESTIMATE HEADERS                                
* ATESTTAB HAS NON-ZERO VALUE FOR EACH OPEN ESTIMATE                            
* ATPOLTAB HAS 4 BYTE PACKED DATE OF POL ESTIMATE                               
BLDEST NTR1                                                                     
*                                                                               
         L     R1,=A(ATESTTAB)                                                  
         L     R0,=A(ATESTTBX)                                                  
         BAS   RE,CLEAR                                                         
*                                                                               
         L     R1,=A(ATPOLTAB)                                                  
         L     R0,=A(ATPOLTBX)                                                  
         BAS   RE,CLEAR                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         GOTO1 HIGH                SKIP CLTHDR                                  
*                                                                               
BLDEST2  GOTO1 SEQ                                                              
*                                                                               
BLDEST4  CLC   KEY(4),KEYSAVE      SAME A-M/CLT                                 
         BNE   EXIT                                                             
         CLI   KEY+7,0             TEST ESTIMATE HEADER                         
         BE    BLDEST2             NO                                           
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   BLDEST2                                                          
*                                                                               
         CLC   =C'POL',KEY+4       TEST POL ESTIMATE                            
         BE    BLDEST10                                                         
*                                                                               
         L     R1,ADCLT                                                         
         LA    R1,CLIST-CLTHDRD(R1)                                             
*                                                                               
BLDEST6  CLC   KEY+4(3),0(R1)      TRANSLATE PRODUCT CODE                       
         BE    BLDEST8                                                          
         LA    R1,4(R1)                                                         
         CLI   0(R1),C'A'                                                       
         BNL   BLDEST6                                                          
         DC    H'0'                                                             
*                                                                               
BLDEST8  ZIC   RE,3(R1)            GET PRD NUMBER                               
         SLL   RE,8                X 256                                        
         A     RE,=A(ATESTTAB)                                                  
         ZIC   R0,KEY+7            GET EST NUMBER                               
         AR    RE,R0               INDEX TO CORRECT BYTE                        
         MVC   0(1,RE),KEY+7       MOVE EST NUMBER TO TABLE                     
         B     BLDEST2                                                          
         EJECT                                                                  
* READ POL ESTHDR TO GET DATES *                                                
BLDEST10 MVC   AREC,ADEST                                                       
         GOTO1 GET                                                              
*                                                                               
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
         GOTO1 DATCON,DMCB,ESTART,(2,FULL)                                      
         GOTO1 (RF),(R1),EEND,(2,FULL+2)                                        
         ZIC   RE,KEY+7            GET ESTIMATE NUMBER                          
         SLL   RE,2                X 4                                          
         A     RE,=A(ATPOLTAB)                                                  
         MVC   0(4,RE),FULL        MOVE DATES TO TABLE                          
         B     BLDEST2                                                          
         DROP  R6                                                               
         EJECT                                                                  
* SET UP EQUIVALENCE TABLE BETWEEN PRODUCT CODES FROM                           
* AGY/ATT RECORDS (ON AGENCY FILE) AND ATT/ATT RECORDS (ON ATT FILE)            
BLDPRD   NTR1                                                                   
         L     R4,=A(AGCLTHDR)     POINT TO NEW CLTHDR                          
         LA    R4,CLIST-CLTHDRD(R4)                                             
         L     RF,=A(PRDTAB)                                                    
         XC    0(256,RF),0(RF)                                                  
*                                                                               
BLDPRD2  L     R5,ADCLT            POINT TO ATT CLTHDR                          
         LA    R5,CLIST-CLTHDRD(R5)                                             
*                                                                               
BLDPRD4  CLC   0(3,R4),0(R5)       MATCH ALPHA PRD CODE                         
         BE    BLDPRD6                                                          
         LA    R5,4(R5)                                                         
         CLI   0(R5),C'A'                                                       
         BNL   BLDPRD4                                                          
         B     BLDPRD10            IGNORE PRD NOT FOUND                         
*                                                                               
BLDPRD6  ZIC   RE,3(R4)            GET PRD NUM                                  
         AR    RE,RF               POINT TO PROPER SLOT                         
         MVC   0(1,RE),3(R5)       MOVE ATT PRD CODE TO SLOT                    
*                                                                               
BLDPRD10 LA    R4,4(R4)                                                         
         CLI   0(R4),C'A'                                                       
         BNL   BLDPRD2                                                          
         B     EXIT                                                             
         EJECT                                                                  
* AGY/ATT RECORDS (ON AGENCY FILE) AND ATT/ATT RECORDS (ON ATT FILE)            
BLDNET   NTR1                                                                   
         L     R4,=A(ATNETTAB)                                                  
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=C'SN'                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,ADSTAT                   
*                                                                               
         L     R6,ADSTAT                                                        
         B     BN120                                                            
*                                                                               
BN100    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'STATION'                              
*                                                                               
BN120    CLC   =C'SN',0(R6)        NETWORK BREAK                                
         BNE   BN300                                                            
         CLC   =C'AT',7(R6)        AT ?                                         
         BNE   BN100               NEXT                                         
         L     R0,=A(ATNETTBX)                                                  
         CR    R4,R0               END OF TABLE                                 
         BL    BN200                                                            
         MVC   ERRKEY(4),2(R6)                                                  
         MVC   ERRMSG(25),=C'** NETWORK TABLE ERROR **'                         
         GOTO1 REPORT                                                           
         B     BN100                                                            
*                                                                               
BN200    MVC   0(4,R4),2(R6)       MOVE NETWORK                                 
         LA    R4,4(R4)                                                         
         BCTR  R5,0                COUNTER                                      
         B     BN100                                                            
*                                                                               
BN300    LPR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO NETWORKS ON AT FILE                       
         ST    R5,ATNETCNT                                                      
BNEXT    B     EXIT                                                             
         EJECT                                                                  
* CHECK NETWORKS ON ATT FILE                                                    
CHKNET   NTR1                                                                   
         L     R1,=A(ATNETTAB)                                                  
         L     R5,ATNETCNT                                                      
CN100    CLC   0(4,R4),0(R1)                                                    
         BE    EQXIT                                                            
         LA    R1,4(R1)                                                         
         BCT   R5,CN100                                                         
*                                                                               
* NOW CHECK TABLE OF DIFFERENT CODES                                            
         LA    R1,MTNETTBL                                                      
CN200    CLI   0(R1),X'FF'                                                      
         BE    CN400                                                            
         ZIC   R5,2(R1)                                                         
         CLC   0(2,R1),SVAGYA      FIND CORRECT AGENCY TABLE                    
         BE    CN300                                                            
         MH    R5,=H'8'                                                         
         AR    R1,R5                                                            
         LA    R1,3(R1)                                                         
         B     CN200                                                            
*                                                                               
CN300    LA    R1,3(R1)                                                         
CN320    CLC   0(4,R4),0(R1)                                                    
         BE    CN500                                                            
         LA    R1,8(R1)                                                         
         BCT   R5,CN320                                                         
CN400    MVC   ERRKEY(2),SVAGYA                                                 
         MVC   ERRKEY+3(4),0(R4)                                                
         MVC   ERRMSG(24),=C'** NETWORK NOT FOUND **'                           
         GOTO1 REPORT                                                           
         B     NEQXIT                                                           
*                                                                               
CN500    MVC   0(4,R4),4(R1)                                                    
         B     EQXIT                                                            
         EJECT                                                                  
* TRANSLATE ALL PRODUCT CODES IN BUYREC TO ATT PRD CODES                        
* APRDTAB CONTAINS APPROPRIATE PRDTAB ADDRESS                                   
XAPRD    NTR1                                                                   
         L     R6,ADBUY                                                         
         USING NURECD,R6                                                        
         MVI   NOAORSW,C'N'                                                     
*                                                                               
         LA    R7,NUPRD                                                         
         CLI   0(R7),0                                                          
         BE    *+8                                                              
PC100    BAS   RE,FINDPRD                                                       
*                                                                               
         CLC   NUKDATE,POLESDTS    TEST ELEM PRIOR TO ATT EST START             
         BL    XAPERERR                                                         
         CLC   NUKDATE,POLESDTS+2    OR AFTER ATT EST END                       
         BH    XAPERERR                                                         
*                                                                               
         CLI   APRDTAB,0           TEST TO CHECK FOR ERRORS                     
         BNE   PC200               NO                                           
         CLI   0(R7),0             NO PRODUCT IN UNIT                           
         BE    PC300                                                            
         SLL   RF,8                PRD CODE X 256                               
         ZIC   RE,NUKEST           ESTIMATE NUMBER                              
         AR    RF,RE                                                            
         A     RF,=A(ATESTTAB)     POINT TO EST TAB ENTRY                       
         CLI   0(R7),0             TEST ATT EST OPEN                            
         BE    XAESTERR                                                         
*                                                                               
PC200    LA    RE,NUPRD2           NEXT PRD CODE                                
         CLI   0(RE),0                                                          
         BE    PC300                                                            
         CR    RE,R7                                                            
         BE    PC300                                                            
         LR    R7,RE                                                            
         B     PC100                                                            
*                                                                               
PC300    MVI   NOAORSW,C'Y'        DON'T SET AOR INFO                           
         LA    R6,27(R6)           POINT TO 1ST ELEMENT                         
PC320    CLI   0(R6),0                                                          
         BE    EXIT                                                             
         CLI   0(R6),X'10'                                                      
         BE    PC400                                                            
         CLI   0(R6),X'24'                                                      
         BE    PC500                                                            
PC340    ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     PC320                                                            
*                                                                               
PC400    LA    R7,5(R6)                                                         
         B     *+8                                                              
*                                                                               
PC500    LA    R7,2(R6)                                                         
         CLI   0(R7),0                                                          
         BE    PC340                                                            
         BAS   RE,FINDPRD                                                       
         B     PC340                                                            
         DROP  R6                                                               
         EJECT                                                                  
FINDPRD  CLI   AORSW,C'Y'                                                       
         BE    FNDAORPR                                                         
         ZIC   RF,0(R7)            GET PRD CODE                                 
         A     RF,APRDTAB                                                       
         CLI   0(RF),0                                                          
         BE    FINDPRD2                                                         
         MVC   0(1,R7),0(RF)                                                    
         BR    RE                                                               
*                                                                               
FINDPRD2 NTR1  ,                    SET UP TO 'ENTER' ERROR ROUTINES            
         B     XAPRDERR                                                         
         SPACE                                                                  
* SUBROUTINE TRANSLATES ATT PRD CODE TO AOR PRD CODE VIA AORTAB *               
         USING AORTABD,R4                                                       
FNDAORPR NTR1                                                                   
         ICM   R4,15,SVAORPRD                                                   
         BZ    *+14                                                             
         CLC   0(1,R7),AORPRDAT    TEST SAME ATT PRD CODE AS PREVIOUS           
         BE    FNDAORPX                                                         
*                                                                               
         ZIC   R4,0(R7)            GET PRD CODE                                 
         ST    R4,FULL             SET KEYARG FOR SEARCH                        
         GOTO1 BINSRCH,AORPARMS,FULL                                            
         CLI   0(R1),1                                                          
         BNE   FNDAORP2            NO - CONTINUE                                
         XC    SVAORPRD,SVAORPRD                                                
         MVI   0(R7),0             SET ERROR FLAGS                              
         MVI   PRDERRSW,C'Y'                                                    
         B     NEQXIT                                                           
*                                                                               
FNDAORP2 CLI   NOAORSW,C'Y'                                                     
         BE    *+16                                                             
         MVC   SVAORPRD,0(R1)      SAVE ENTRY ADDRESS                           
         MVC   SVAORPRD(1),0(R7)   AND PRODUCT CODE                             
         L     R4,SVAORPRD                                                      
*                                                                               
FNDAORPX MVC   0(1,R7),AORPRDAG    SET AGENCY PRD CODE                          
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE 2                                                                
* TEST AGENCY ESTIMATE NUMBER IS VALID ON CLIENT ATT                            
* ON ENTRY R0 CONTAINS EST NUMBER                                               
XAEST    NTR1                                                                   
         LR    RE,R0                                                            
         SLL   RE,2                X 4                                          
         A     RE,=A(ATPOLTAB)                                                  
         MVC   POLESDTS,0(RE)      MOVE TO SAVE AREA                            
         OC    0(4,RE),0(RE)       TEST POL EST OPEN                            
         BNZ   EQXIT                                                            
         B     NEQXIT                                                           
         EJECT                                                                  
NETERR   MVC   ERRMSG(33),=C'** ERROR ** ATT NETWORK NOT FOUND'                 
         B     XAERR                                                            
*                                                                               
XAPERERR MVC   ERRMSG(38),=C'** ERROR ** UNIT NOT IN ATT EST PERIOD'            
         MVI   PRDERRSW,C'Y'       INDICATE FATAL ERROR                         
         B     XAERR                                                            
*                                                                               
XAPRDERR MVC   ERRMSG(36),=C'** ERROR ** MISSING ATT PRODUCT CODE'              
         MVI   PRDERRSW,C'Y'       INDICATE FATAL ERROR                         
*                                                                               
* NEED TO PUT ALPHA CODE IN MESSAGE                                             
*                                                                               
XAPRERR0 L     RE,=A(AGCLTHDR)                                                  
         LA    RE,CLIST-CLTHDRD(RE)                                             
*                                                                               
XAPRERR2 CLC   0(1,R7),3(RE)       MATCH PRD NUMBERS                            
         BE    XAPRERR4                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   XAPRERR2                                                         
         ZIC   R0,0(R7)            ANYTHING IS BETTER THAN BLOWING UP           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL(3),DUB                                                      
         LA    RE,FULL                                                          
XAPRERR4 MVC   ERRMSG+38(3),0(RE)                                               
         B     XAERR                                                            
*                                                                               
XAESTERR MVC   ERRMSG(37),=C'** ERROR ** NO ATT BRAND ESTIMATE PRD='            
         L     RE,ADCLT            POINT TO ATT CLTHDR                          
         LA    RE,CLIST-CLTHDR(RE)                                              
         B     XAPRERR2                                                         
         EJECT                                                                  
XAERR    LA    R4,ERRKEY                                                        
         L     R2,ADBUY                                                         
         USING NURECD,R2                                                        
         CLC   SVUNITER,NUKEY                                                   
         BNE   XAERR2                                                           
         MVC   ERRMSG,SPACES                                                    
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
XAERR2   L     R2,ADBUY                                                         
         CLI   0(R2),X'04'                                                      
         BNE   EXIT                                                             
         USING NURECD,R2                                                        
         MVC   SVUNITER,NUKEY                                                   
         MVC   0(3,R4),SVAGYA                                                   
         MVC   4(4,R4),NUKNET                                                   
         MVC   9(6,R4),NUKPROG                                                  
         ZIC   R0,NUKEST           EST                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  16(3,R4),DUB                                                     
         ZIC   R0,NUPACK           PACKAGE                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  20(3,R4),DUB                                                     
         GOTO1 DATCON,DMCB,(2,NUKDATE),(5,24(R4))                               
         MVI   32(R4),C'-'                                                      
         ZIC   R0,NUKSUB           LINE                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  33(3,R4),DUB                                                     
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
* PRINT TOTALS FOR AN AGENCY                                                    
XAAGYTOT NTR1                                                                   
         CLI   XFRAGY,0            TEST FIRST TIME                              
         BE    AGYTOTX                                                          
*                                                                               
         MVC   P(3),SVAGYA         AGENCY ALPHA                                 
         MVI   P+3,C'='                                                         
         ZIC   R0,XFRAGY                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+4(2),DUB                                                       
         OI    AGYCNT+3,X'0F'                                                   
         UNPK  P+10(7),AGYCNT                                                   
         MVC   P+18(7),=C'RECORDS'                                              
         GOTO1 REPORT                                                           
*                                                                               
AGYTOTX  IC    RE,XFRAGY           BUMP XFR AGENCY NUMBER                       
         LA    RE,1(RE)                                                         
         STC   RE,XFRAGY                                                        
         ZAP   AGYCNT,=P'0'        RESET AGENCY RECORD COUNTER                  
         B     EXIT                                                             
         EJECT                                                                  
* SUBROUTINE TO CLEAR FROM R1 TO R0. RF IS DESTROYED.                           
CLEAR    LA    RF,256                                                           
*                                                                               
CLEAR2   SR    R0,R1               GIVES LENGTH TO CLEAR                        
         CR    R0,RF                                                            
         BNH   CLEAR4                                                           
         XC    0(256,R1),0(R1)                                                  
         AR    R1,RF                                                            
         SR    R0,RF                                                            
         BZR   RE                                                               
         B     CLEAR2                                                           
CLEAR4   LR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,CLEARXC                                                       
         BR    RE                                                               
CLEARXC  XC    0(0,R1),0(R1)  ** EXECUTED **                                    
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
**************************************************                              
* ACCUMULATE ASS/ACT/INTEG DOLLARS - UNIT RECS   *                              
* R2 POINTS TO UNIT RECORD                       *                              
* TOTIND = C'-' TO POST TO REMOVED               *                              
* TOTIND = C'+' TO POST TO ADDED                 *                              
**************************************************                              
         SPACE                                                                  
GETCLTOT NTR1                                                                   
*                                                                               
         L     R2,ADBUY                                                         
         USING NURECD,R2                                                        
*                                                                               
         CLI   0(R2),X'FF'         TEST EOF                                     
         BE    GC500                                                            
*                                                                               
         TM    NURSTAT,X'80'       TEST DELETED                                 
         BNZ   EXIT                                                             
*                                                                               
         CLI   OLDCLT,0            TEST FIRST TIME                              
         BE    GC100                                                            
*                                                                               
         CLC   OLDCLT,NUKCLT       TEST SAME CLIENT                             
         BE    GC200               YES                                          
         L     R5,NEXTBUF          NO - MOVE CLT TOT TO BUFFER                  
         MVC   0(1,R5),TOTIND      MOVE +/-                                     
         MVC   1(2,R5),OLDCLT                                                   
         MVC   4(24,R5),CTOTS                                                   
         LA    R5,28(R5)                                                        
         ST    R5,NEXTBUF                                                       
         MVI   0(R5),0             SET END OF BUFFER FLAG                       
*                                                                               
GC100    LA    R1,CTOTS                                                         
         BAS   RE,CLRCLTTL         CLEAR CTOTS                                  
*                                                                               
         MVC   OLDCLT,2(R2)        MOVE CLIENT CODE                             
GC200    ICM   RE,15,NUASSIGN                                                   
         CVD   RE,DUB                                                           
         AP    CTOTS(8),DUB                                                     
         ICM   RE,15,NUACTUAL                                                   
         CVD   RE,DUB                                                           
         AP    CTOTS+8(8),DUB                                                   
         ICM   RE,15,NUINTEG                                                    
         CVD   RE,DUB                                                           
         AP    CTOTS+16(8),DUB                                                  
         B     EXIT                                                             
*                                                                               
GC500    L     R5,NEXTBUF          MOVE LAST CLT TOT TO BUFFER                  
         MVC   0(1,R5),TOTIND                                                   
         MVC   1(2,R5),OLDCLT                                                   
         MVC   4(24,R5),CTOTS                                                   
         MVI   28(R5),0            SET END OF BUFFER FLAG                       
         LA    R5,28(R5)           AND ADVANCE POINTER                          
         ST    R5,NEXTBUF                                                       
         XC    OLDCLT,OLDCLT       SET FIRST TIME FLAG                          
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
* PRINT DOLLAR TOTALS FROM BUFFER *                                             
XATOTPRT NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   P(21),=C'** DELETED RECORDS **'                                  
         GOTO1 REPORT                                                           
*                                                                               
         L     R5,=A(TOTBUFF)                                                   
         ST    R5,NEXTBUF                                                       
*                                                                               
         CLI   0(R5),C'-'                                                       
         BNE   XATP8                                                            
*                                                                               
XATP2    DS    0H                                                               
         GOTO1 CLUNPK,DMCB,1(R5),P+27                                           
*                                                                               
XATP4    LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,4(R5)                                                         
         LA    R6,AGYTOTS                                                       
*                                                                               
XATP6    AP    0(8,R6),0(8,R5)     BUMP AGY TOTALS                              
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XATP6                                                         
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         CLI   0(R5),C'-'                                                       
         BE    XATP2                                                            
         ST    R5,NEXTBUF          SAVE BUFFER POINTER                          
*                                                                               
         GOTO1 REPORT              SKIP A LINE                                  
         SPACE                                                                  
* PRINT THE - TOTALS *                                                          
XATP8    MVC   P+27(5),=C'TOTAL'                                                
         LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,AGYTOTS                                                       
         LA    R6,FILTOTS                                                       
*                                                                               
XATP10   SP    0(8,R6),0(8,R5)     SUBTRACT FROM AATTUMS                        
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XATP10                                                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R1,AGYTOTS                                                       
         BAS   RE,CLRCLTTL                                                      
         SPACE 2                                                                
* PRINT THE TOTALS OF INSERTED RECORDS *                                        
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         L     R5,NEXTBUF          RESTORE BUFFER POINTER                       
*                                                                               
         MVC   P(21),=C'** INSERTED RECORDS **'                                 
         GOTO1 REPORT                                                           
*                                                                               
         CLI   0(R5),0             TEST END OF BUFFER                           
         BE    XATP20                                                           
*                                                                               
XATP12   DS    0H                                                               
         GOTO1 CLUNPK,DMCB,1(R5),P+27                                           
*                                                                               
XATP14   LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,4(R5)                                                         
         LA    R6,AGYTOTS                                                       
*                                                                               
XATP16   AP    0(8,R6),0(8,R5)     BUMP AGY TOTALS                              
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XATP16                                                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         CLI   0(R5),0                                                          
         BNE   XATP12                                                           
*                                                                               
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
         MVC   P+27(5),=C'TOTAL'                                                
         LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,AGYTOTS                                                       
         LA    R6,FILTOTS                                                       
*                                                                               
XATP18   AP    0(8,R6),0(8,R5)     BUMP FILE TOTALS                             
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XATP18                                                        
*                                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
         LA    R1,AGYTOTS                                                       
         BAS   RE,CLRCLTTL                                                      
         SPACE                                                                  
XATP20   DS    0H                                                               
         MVC   P(22),=C'* GRAND TOTALS (NET) *'                                 
*                                                                               
         LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,FILTOTS                                                       
*                                                                               
XATP22   MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR PENNIES                                
         MVI   15(R3),C'+'         SET SIGN                                     
         CP    0(8,R5),=P'0'                                                    
         BNM   *+8                                                              
         MVI   15(R3),C'-'                                                      
         LA    R5,8(R5)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XATP22                                                        
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
CLRCLTTL LA    R0,3                                                             
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
* TRANSLATE EBCDIC PRD TO UNIQUE ATT BINARY PRD THEN TO AGY PRD                 
         USING AORTABD,R1                                                       
*                                                                               
SETAOR   NTR1                                                                   
         L     RE,ADCLT            POINT TO ATT CLTHDR                          
         LA    RE,CLIST-CLTHDRD(RE)                                             
*                                                                               
SETAOR2  CLC   AORPRD,0(RE)                                                     
         BE    SETAOR4                                                          
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   SETAOR2                                                          
         B     EXIT                                                             
*                                                                               
SETAOR4  MVC   AORPRDAT,3(RE)      SET ATT BINARY PRD CODE                      
*                                                                               
         ICM   RE,15,AORADAGY              POINT TO AGYTAB ENTRY                
         ICM   RE,15,AGYADCLT-AGYTABD(RE)  POINT TO AGY CLTHDR                  
         LA    RE,CLIST-CLTHDRD(RE)                                             
*                                                                               
SETAOR6  CLC   AORPRD,0(RE)        MATCH PRD CODE                               
         BE    SETAOR8                                                          
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   SETAOR6                                                          
         B     EXIT                                                             
*                                                                               
SETAOR8  MVC   AORPRDAG,3(RE)      SET AGY BINARY PRD CODE                      
         B     EXIT                                                             
         EJECT                                                                  
UNHIGH   MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   DMFILE,=C'UNTDIR'                                                
         TM    SVTRACE,X'80'                                                    
         BZ    UNDIR                                                            
         SPACE 1                                                                
         LA    R0,KEYSAVE          BUILD TRACE PARAMS                           
         ST    R0,TRIO1                                                         
         MVI   TRIO1,20                                                         
         LA    R0,KEY                                                           
         ST    R0,TRIO2                                                         
         MVI   TRIO2,25                                                         
         B     UNDIR                                                            
         SPACE 1                                                                
UNSEQ    MVC   COMMAND,=C'DMRSEQ'                                               
         MVC   DMFILE,=C'UNTDIR'                                                
         TM    SVTRACE,X'80'                                                    
         BZ    UNDIR                                                            
         LA    R0,KEY                                                           
         ST    R0,TRIO1                                                         
         MVI   TRIO1,20                                                         
         LA    R0,KEY                                                           
         ST    R0,TRIO2                                                         
         MVI   TRIO2,25                                                         
         SPACE 1                                                                
UNDIR    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,KEY,KEY                    
         LR    RE,R0                                                            
         TM    SVTRACE,X'80'                                                    
         BZR   RE                                                               
         B     UNTRACE                                                          
         SPACE 1                                                                
UNGET    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'UNTFILE',          X        
               KEY+21,ADBUY,DMWORK                                              
         SPACE 1                                                                
         LR    RE,R0                                                            
         TM    SVTRACE,X'80'                                                    
         BZR   RE                                                               
         LA    R0,KEY+21                                                        
         ST    R0,TRIO1                                                         
         MVI   TRIO1,4                                                          
         L     R0,ADBUY                                                         
         ST    R0,TRIO2                                                         
         MVI   TRIO2,3                                                          
         B     UNTRACE                                                          
         EJECT                                                                  
*              TRACE FACILITY                                                   
         SPACE 3                                                                
UNTRACE  NTR1                                                                   
         SPACE 1                                                                
         L     RE,DMCB                                                          
         MVC   P(6),0(RE)                                                       
         L     RE,DMCB+4                                                        
         MVC   P+8(6),0(RE)                                                     
         SPACE 1                                                                
         LA    R4,P+16                                                          
         ZIC   R0,TRIO1                                                         
         GOTO1 HEXOUT,DMCB,TRIO1,(R4),(R0),=C'TOG'                              
         A     R4,DMCB+16                                                       
         LA    R4,2(R4)                                                         
         SPACE 1                                                                
         ZIC   R0,TRIO2            GET OUTPUT REC LEN                           
         GOTO1 HEXOUT,DMCB,TRIO2,(R4),(R0),=C'TOG'                              
         SPACE 1                                                                
         GOTO1 REPORT                                                           
         B     EXIT                RETURN TO CALLER                             
         DROP  R1                                                               
         EJECT                                                                  
NEXTBUF  DC    A(TOTBUFF)                                                       
SVAORPRD DC    A(0)                                                             
APRDTAB  DS    A                                                                
AORSW    DS    C                                                                
SVTRACE  DC    X'00'                                                            
TOTIND   DC    X'00'                                                            
OLDCLT   DC    XL2'00'                                                          
CTOTS    DS    0CL24                                                            
         DC    3PL8'0'             GROSS ORD/GROSS PAID/NET PAID                
AGYTOTS  DC    3PL8'0'                                                          
FILTOTS  DC    3PL8'0'                                                          
PKKEY    DS    CL34                LAST PACKAGE KEY READ                        
DMFILE   DS    CL8                                                              
GDPKGE   DS    CL25                LIST OF EXCEPTABLE PACKAGES                  
SVPKCLT  DS    CL2                                                              
SVPKEST  DS    CL1                                                              
SVPKNET  DS    CL4                                                              
NOAORSW  DS    CL1                                                              
POLESDTS DS    F                                                                
SVATTAGY DS    A                                                                
SVAGYPRD DS    A(PRDTABSV)                                                      
TRIO1    DS    A                                                                
TRIO2    DS    A                                                                
ATNETCNT DS    F                                                                
SVAGYA   DS    CL3                                                              
SVAGYCLT DS    XL2                                                              
SVAGYB   DS    XL1                                                              
ENDPK    DS    CL1                                                              
LSTPKNUM DS    XL1                                                              
XFRAGY   DC    X'00'                                                            
CNDATA   DS    XL14                                                             
SVUNITER DS    CL20                                                             
ELCODE   DS    XL1                                                              
PRDERRSW DC    X'00'                                                            
*                                                                               
SVPROF   DS    CL16                                                             
SVPROF12 EQU   SVPROF+11           HIGH EST FOR XFR                             
*                                                                               
AGYCNT   DC    PL4'0'                                                           
*                                                                               
CTRS     DS    0CL24                                                            
INCNT    DC    PL4'0',CL20'UNIT RECORDS IN'                                     
OUTCNT   DC    PL4'0',CL20'UNIT RECORDS OUT'                                    
INCNTS   DC    PL4'0',CL20'SPOT RECORDS IN'                                     
OUTCNTG  DC    PL4'0',CL20'SPOT RECORDS OUT'                                    
DELCNTU  DC    PL4'0',CL20'UNIT RECS DELETED'                                   
ADDCNTU  DC    PL4'0',CL20'UNIT RECS INSERTED'                                  
DELCNTP  DC    PL4'0',CL20'PACK RECS DELETED'                                   
ADDCNTP  DC    PL4'0',CL20'PACK RECS INSERTED'                                  
DELCNTG  DC    PL4'0',CL20'GOAL RECS DELETED'                                   
ADDCNTG  DC    PL4'0',CL20'GOAL RECS INSERTED'                                  
CTRX     EQU   *-1                                                              
         EJECT                                                                  
FILEINN  DCB   DDNAME=FILEINN,DSORG=PS,RECFM=VB,MACRF=GM,              X        
               EODAD=RT400                                                      
FILEOUTN DCB   DDNAME=FILEOUTN,DSORG=PS,RECFM=VB,MACRF=PM,             X        
               LRECL=4004,BUFNO=2,BLKSIZE=32760                                 
FILEINS  DCB   DDNAME=FILEINS,DSORG=PS,RECFM=VB,MACRF=GM,              X        
               EODAD=PD100                                                      
FILEOUTS DCB   DDNAME=FILEOUTS,DSORG=PS,RECFM=VB,MACRF=PM,             X        
               LRECL=4004,BUFNO=2,BLKSIZE=32760                                 
         EJECT                                                                  
AGYTABD  DSECT                                                                  
AGYAGY   DS    CL2                 ALPHA AGENCY CODE                            
AGYCLT   DS    CL3                 ATT FILE CLIENT CODE                         
AGYPKGN  DS    XL1                 PACK ASSIGN NUMBER                           
AGYCLTP  DS    XL2                 PACKED CLIENT CODE                           
AGYADCLT DS    AL4                 CLTHDR SAVE AREA ADDRESS                     
AGYTABL  EQU   *-AGYTABD                                                        
         SPACE                                                                  
SPNX02   CSECT                                                                  
         DS    0D                                                               
         DC    C'*AGYTAB*'                                                      
ATAGYTAB DS    0C                                                               
ATAGYMC  DC    C'MCMC ',AL1(1),AL2(0),AL4(ATTMC)                                
ATAGYNW  DC    C'NWNW ',AL1(2),AL2(0),AL4(ATTNW)                                
ATAGYOM  DC    C'OGOM ',AL1(3),AL2(0),AL4(ATTOM)                                
ATAGYYR  DC    C'YNYR ',AL1(4),AL2(0),AL4(ATTYR)                                
         DC    X'FF'                        EOL FLAG                            
         EJECT                                                                  
***** A O R TABLE ******                                                        
         SPACE                                                                  
AORTABD  DSECT                                                                  
*                                                                               
AORPRD   DS    CL3                 ALPHA PRODUCT CODE                           
AORPRDAT DS    XL1                 ATT BINARY PRODUCT CODE                      
AORPRDAG DS    XL1                 AOR BINARY PRODUCT CODE                      
AORADAGY DS    AL4                 AOR ADDRESS OF OWNER AGY                     
AORTABL  EQU   *-AORTABD                                                        
*                                                                               
SPNX02   CSECT                                                                  
         SPACE                                                                  
* THIS PARAMETER LIST USED FOR BINSRCH ON AORTAB *                              
AORPARMS DC    A(0)                A(KEYARG)                                    
         DC    A(AORTAB)           A(TABLE)                                     
         DC    A(AORTABCT)         TABLE COUNT                                  
         DC    A(AORTABL)          ENTRY LENGTH                                 
         DC    AL1(3),AL3(1)       KEY DSPL, KEY LENGTH                         
         DC    A(AORTABCT)         MAX TABLE COUNT                              
         SPACE 2                                                                
FLIST    DC    CL8' SPTFILE'                                                    
         DC    CL8' SPTDIR '                                                    
         DC    CL8' STAFILE'                                                    
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
*                                                                               
FLIST2   DC    CL8' UNTFILE'                                                    
         DC    CL8' UNTDIR'                                                     
         DC    CL8'X'                                                           
*                                                                               
         DS    0D                                                               
         DC    C'MTNETTBL'                                                      
MTNETTBL DS    0C                                                               
         DC    C'YN',X'8',CL4'CNH',CL4'CNNH'                                    
         DC    CL4'WEA',CL4'WCN',CL4'USA',CL4'USAC'                             
         DC    CL4'TOV',CL4'TVTC',CL4'AAE',CL4'AETM'                            
         DC    CL4'SYC',CL4'SSI',CL4'BUS',CL4'BTN'                              
         DC    CL4'TUR',CL4'TPE'                                                
         DC    C'MC',X'9',CL4'SYN',CL4'SSI'                                     
         DC    CL4'BUS',CL4'BTN',CL4'USA',CL4'USAC'                             
         DC    CL4'VID',CL4'TVTC',CL4'ESPN',CL4'ESP'                            
         DC    CL4'TWC',CL4'WCN',CL4'PRO',CL4'PRSV'                             
         DC    CL4'ARTS',CL4'AETM',CL4'RAY',CL4'RCOM'                           
         DC    X'FF'                        EOL FLAG                            
*                                                                               
         LTORG                                                                  
         DS    0D                                                               
         DC    C'*AORTAB*'                                                      
AORTAB   DS    0C                                                               
         SPACE                                                                  
* NW ATT COMMUNICATIONS                                                         
         DC    CL3'AAB',2X'00',AL4(ATAGYNW)                                     
         DC    CL3'AAC',2X'00',AL4(ATAGYNW)                                     
         DC    CL3'GIF',2X'00',AL4(ATAGYNW)                                     
         DC    CL3'KEY',2X'00',AL4(ATAGYNW)                                     
         DC    CL3'NIC',2X'00',AL4(ATAGYNW)                                     
         DC    CL3'ROA',2X'00',AL4(ATAGYNW)                                     
         DC    CL3'INF',2X'00',AL4(ATAGYNW)                                     
         DC    CL3'TAR',2X'00',AL4(ATAGYNW)                                     
* NW ATT CORPORATE                                                              
         DC    CL3'DCA',2X'00',AL4(ATAGYNW)                                     
         DC    CL3'DFJ',2X'00',AL4(ATAGYNW)                                     
         DC    CL3'ICA',2X'00',AL4(ATAGYNW)                                     
* NW ATT CONSUMER                                                               
         DC    CL3'ATM',2X'00',AL4(ATAGYNW)                                     
         DC    CL3'BNT',2X'00',AL4(ATAGYNW)                                     
         DC    CL3'CAB',2X'00',AL4(ATAGYNW)                                     
* MC ATT COMM                                                                   
         DC    CL3'ADS',2X'00',AL4(ATAGYMC)                                     
         DC    CL3'ATC',2X'00',AL4(ATAGYMC)                                     
         DC    CL3'EHS',2X'00',AL4(ATAGYMC)                                     
         DC    CL3'EHU',2X'00',AL4(ATAGYMC)                                     
         DC    CL3'EOE',2X'00',AL4(ATAGYMC)                                     
         DC    CL3'JAD',2X'00',AL4(ATAGYMC)                                     
         DC    CL3'LDS',2X'00',AL4(ATAGYMC)                                     
         DC    CL3'MPE',2X'00',AL4(ATAGYMC)                                     
         DC    CL3'PRA',2X'00',AL4(ATAGYMC)                                     
         DC    CL3'SDN',2X'00',AL4(ATAGYMC)                                     
         DC    CL3'WAT',2X'00',AL4(ATAGYMC)                                     
* YR ATT COMM                                                                   
         DC    CL3'MS ',2X'00',AL4(ATAGYYR)                                     
         DC    CL3'CCS',2X'00',AL4(ATAGYYR)                                     
         DC    CL3'OBR',2X'00',AL4(ATAGYYR)                                     
         DC    CL3'OBB',2X'00',AL4(ATAGYYR)                                     
         DC    CL3'OC ',2X'00',AL4(ATAGYYR)                                     
* OM ATT INFO SYSTEMS                                                           
         DC    CL3'LBS',2X'00',AL4(ATAGYOM)                                     
         DC    CL3'DSD',2X'00',AL4(ATAGYOM)                                     
         DC    CL3'TIG',2X'00',AL4(ATAGYOM)                                     
         DC    CL3'GBS',2X'00',AL4(ATAGYOM)                                     
         DC    CL3'CSD',2X'00',AL4(ATAGYOM)                                     
         DC    CL3'IMM',2X'00',AL4(ATAGYOM)                                     
         DC    CL3'ATS',2X'00',AL4(ATAGYOM)                                     
AORTABCT EQU   (*-AORTAB)/AORTABL                                               
         EJECT                                                                  
         DS    0D                                                               
         DC    C'**XAIO**'                                                      
XAIO     DS    2000C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'PRDTABSV'                                                    
PRDTABSV DS    4XL256              USED TO SAVE CONVERTED PRODUCTS TT           
*                                                                               
         DS    0D                                                               
         DC    C'*PRDTAB'                                                       
PRDTAB   DS    XL256               USED TO CONVERT AGY/ATT TO ATT/ATT           
*                                                                               
         DS    0D                                                               
         DC    CL8'ATPOLTAB'                                                    
ATPOLTAB DS    256XL4                                                           
ATPOLTBX EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'ATESTTAB'                                                    
ATESTTAB DS    210XL256                                                         
ATESTTBX EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'ATNETTAB'                                                    
ATNETTAB DS    CL800               200 NETWORKS ALLOWED                         
ATNETTBX EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'AGCLTHDR'                                                    
AGCLTHDR DS    1000C               THIS AREA FOR AGY/ATT CLTHDR                 
*                                                                               
         DS    0D                                                               
         DC    CL8'**ATTMC**'                                                   
ATTMC   DS     1000C               THIS AREA FOR ATT/AGY CLTHDR                 
*                                                                               
         DS    0D                                                               
         DC    CL8'**ATTNW**'                                                   
ATTNW   DS     1000C               THIS AREA FOR ATT/AGY CLTHDR                 
*                                                                               
         DS    0D                                                               
         DC    CL8'**ATTOM**'                                                   
ATTOM   DS     1000C               THIS AREA FOR ATT/AGY CLTHDR                 
*                                                                               
         DS    0D                                                               
         DC    CL8'**ATTYR**'                                                   
ATTYR   DS     1000C               THIS AREA FOR ATT/AGY CLTHDR                 
*                                                                               
         DS    0D                                                               
         DC    CL8'TOTBUFF'                                                     
TOTBUFF  DS    800D                                                             
         EJECT                                                                  
* CLIENT/ESTIMATE/GOAL/UNIT/PACKAGE RECORDS FOLLOW                              
* DDCNTRL/SPREPMODES/SPGENSTEQ/DEDBLOCK/SPREPWORKD FOLLOW                       
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
* STATION                                                                       
       ++INCLUDE SPGENSTA                                                       
* UNIT                                                                          
       ++INCLUDE NEGENUNIT                                                      
* PACKAGE                                                                       
       ++INCLUDE NEGENPACK                                                      
*                                                                               
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPGENSTEQ                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
         ORG   P                                                                
ERRKEY   DS    CL40                                                             
ERRMSG   DS    CL92                                                             
         ORG   P                                                                
ERRSTEQ  DS    CL132                                                            
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072NENETXA02 05/01/02'                                      
         END                                                                    
