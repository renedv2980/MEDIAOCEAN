*          DATA SET SPREPBW02  AT LEVEL 017 AS OF 03/23/04                      
*PHASE SPBW02A                                                                  
         TITLE 'SPREPBW02 -MOVE SPOT DATA TO ANOTHER FILE'                      
***********************************************************************         
*                                                                     *         
*        MOVE HEADERS,BUYS AND GOALS FROM ONE AGENCY TO ANOTHER       *         
*                                                                     *         
*        GENERALIZED TO READ INPUT FOR AGENCIES TO MOVE               *         
*                                                                     *         
*        PARAMETERS PASSED IN NORMAL SPOT REQUEST CARD                *         
*              QAGY SOURCE AGENCY                                     *         
*              QMED SOURCE MEDIA                                      *         
*              QPRD SOURCE PRODUCT                                    *         
*              QEST STARTING ESTIMATE TO MOVE                         *         
*              QESTEND END OF ESTIMATE RANGE OR BLANK FOR 1 ESTIMATE  *         
*              QDEMOVRD 'E' REPORT ERRORS ONLY                        *         
*              QSTART START DATE OF PERIOD TO BE MOVED                *         
*              QEND   END   DATE OF PERIOD TO BE MOVED                *         
*              QEND   END   DATE OF PERIOD TO BE MOVED                *         
*              QPRD2 NEW PRODUCT CODE IF CHANGING PRODUCT ID ZL3      *         
*                    ONLY NEEDED IF READING FROM TAPE                 *         
*              QPRD3 ADD TO LINE NUMBER IN CASE OF DUPLICATE LINE #'S *         
*              QOPT1 MOVE BUYS                                        *         
*                    C'Y' MOVE BUYS COMPLETE                          *         
*                    C'D' MOVE BUYS WITH NO DEMOS                     *         
*                    C'C' MOVE BUYS WITH NO COSTS OR DEMOS            *         
*              QOPT2 MOVE GOALS                                       *         
*              QOPT3 MOVE CLIENT GROUP RECORDS                        *         
*              QOPT4 'C' - DO NOT MOVE CLIENT HEADERS                 *         
*                    'P' - DO NOT MOVE CLIENT/PRODUCT HEADERS         *         
*                    'E' - DO NOT MOVE CLIET/PRODUCT/ESTIMATE HDRS    *         
*              QOPT5 MOVE CABLE BUYS                                  *         
*              QGRP(1) 'D' DELETE SPILL ELEMENTS                      *         
*              QUESTOR - NEW AGY/CLT/EST/MKT - SEE QNW FIELDS         *         
*              QUESTOR+11  C'F' - READ SPTFILE                        *         
*                          C'T' - READ DUMP TAPE                      *         
*                          C'W' - READ/WRITE SPTFILE                  *         
*                                                                     *         
*        SEVERAL REQUEST CARDS CAN BE SUBMITTED                       *         
*        SEVERAL CLT/PRD/EST COMBINATIONS ALLOWED                     *         
*                                                                     *         
*        DO NOT DELETE                                                *         
*        SAVE FOR POSTERITY                                           *         
*                                                                     *         
*        THIS MODULE RESTRICTED TO SPTDIR/FIL                         *         
*                                                                     *         
*        TEST THAT LOCKIN ELEMENTS ARE ALL DELETED                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - INIT'                       
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT NOGEN                                                            
SPBW02   CSECT                                                                  
         NMOD1 0,SPBW02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         L     RC,=A(WORKD)        ESTABLISH WORKING STORAGE                    
         USING WORKD,RC                                                         
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQ1ST                                                           
*                                                                               
INITX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - REQ1ST'                     
***********************************************************************         
*                                                                     *         
*        THIS IS THE ONLY MODE RECOGNIZED - FORCED END OF REQUEST     *         
*              WHEN OVER                                              *         
*                                                                     *         
*        REQUEST CARD USED TO SET OLD AND NEW AGENCIES                *         
*        CARD DATASET IS READ INTO QAREA TO DETERMINE                 *         
*        DATA TO MOVE                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
REQ1ST   DS    0H                                                               
*                                                                               
         ZAP   RECCNTR,=P'0'       INIT RECORD COUNTER                          
         ZAP   WRCTR,=P'0'         INIT RECORD WRITE COUNTER                    
*                                                                               
*        SAVE AGENCY POWER CODES                                                
*                                                                               
         MVC   AGYCHOLD,QAGY       OLD AGENCY                                   
         MVC   AGYCHNEW,QNWAGY     NEW AGENCY                                   
*                                                                               
         MVC   MVRDTYPE,QRDTYPE    SAVE TYPE OF READS TAPE/FILE                 
*                                                                               
         CLI   MVRDTYPE,C'W'       IF WRITING TO FILE                           
         BNE   *+12                                                             
         MVI   MVWRTYPE,C'W'          SET TO WRITE TO FILE                      
         MVI   MVRDTYPE,C'F'          SET TO READ FROM FILE                     
*                                                                               
         CLI   MVRDTYPE,C'F'       IF NOT FROM THE FILE                         
         BE    *+8                                                              
         MVI   MVRDTYPE,C'T'          DEFAULT TO TAPE                           
*                                                                               
         L     RF,UTL                                                           
         MVC   AGYSEOLD,4(RF)      SAVE SE FOR OLD AGENCY                       
*                                                                               
         MVC   AGYAGOLD,BAGYMD     GET OLD AGENCY MEDIA                         
         NI    AGYAGOLD,X'F0'      KILL MEDIA NYBBLE                            
*                                                                               
*        FIND NEW AGENCY'S SE NUMBER AND AGENCY NUMBER                          
*                                                                               
         XC    CNDATA,CNDATA       INIT ID AREA                                 
*                                                                               
         LA    R5,CNDATA           ESTABLISH CONTROL AREA                       
         USING CND,R5                                                           
*                                                                               
         MVC   CNAGY,AGYCHNEW      SET NEW AGENCY ALPHA CODE                    
         XC    FULL,FULL           RELATIVE NUMBER OF SYS TO BE FOUND           
*                                                                               
         GOTO1 CONFID,DMCB,CNDATA,(1,FULL)                                      
         OC    FULL,FULL           TEST ID FOUND                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AGYSENEW,CNSSE      SAVE NEW SPOT SE NUMBER                      
         MVC   AGYAGNEW,CNSCD      SAVE NEW SPOT AGENCY NUMBER                  
*                                                                               
         DROP  R5                                                               
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),AGYSENEW    SET UTL TO NEW AGENCY                        
*                                                                               
*        OPEN STATION FILES FOR NEW AGENCY                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',SPTFILES,DMWORK                 
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),AGYSEOLD    SET UTL TO OLD AGENCY                        
*                                                                               
*        DETERMINE CONVERSION OPTIONS                                           
*                                                                               
         CLI   QDEMOVRD,C'E'       CHECK FOR ERRORS ONLY                        
         BNE   *+8                                                              
         MVI   MVERROPT,C'E'                                                    
*                                                                               
         CLI   QOPT1,C'Y'          TEST FOR BUYS CONVERSION                     
         BNE   *+16                                                             
         MVI   MVBUYS,C'Y'                                                      
         MVI   MVCOSTS,C'Y'                                                     
         MVI   MVDEMOS,C'Y'                                                     
*                                                                               
         CLI   QOPT1,C'D'          TEST FOR BUYS CONVERSION                     
         BNE   *+16                  WITHOUT DEMOS                              
         MVI   MVBUYS,C'Y'                                                      
         MVI   MVCOSTS,C'Y'                                                     
         MVI   MVDEMOS,C'N'                                                     
*                                                                               
         CLI   QOPT1,C'C'          TEST FOR BUYS CONVERSION                     
         BNE   *+16                  WITHOUT DEMOS OR COSTS                     
         MVI   MVBUYS,C'Y'                                                      
         MVI   MVCOSTS,C'N'                                                     
         MVI   MVDEMOS,C'N'                                                     
*                                                                               
         CLI   QOPT2,C'Y'          TEST FOR GOALS CONVERSION                    
         BNE   *+8                                                              
         MVI   MVGOALS,C'Y'                                                     
*                                                                               
         CLI   QOPT3,C'Y'          TEST FOR CLIENT GROUP CONVERSION             
         BNE   *+8                                                              
         MVI   MVCGRP,C'Y'                                                      
*                                                                               
         CLI   QOPT4,C'C'          TEST FOR CLIENT  RECORD SKIPPED              
         BE    MTBINICL                                                         
         CLI   QOPT4,C'P'                                                       
         BE    MTBINICL                                                         
         CLI   QOPT4,C'E'                                                       
         BNE   *+8                                                              
MTBINICL MVI   MVCLT,C'Y'                                                       
*                                                                               
         CLI   QOPT4,C'P'          TEST FOR PRODUCT RECORD SKIPPED              
         BE    MTBINIPR                                                         
         CLI   QOPT4,C'E'                                                       
         BNE   *+8                                                              
MTBINIPR MVI   MVPRD,C'Y'                                                       
*                                                                               
         CLI   QOPT4,C'E'          TEST FOR ESTIMATE RECORD SKIPPED             
         BNE   *+8                                                              
MTBINIES MVI   MVEST,C'Y'                                                       
*                                                                               
         CLI   QOPT5,C'Y'          MOVING CABLE BUYS?                           
         BNE   *+8                                                              
         MVI   MVCABLE,C'Y'                                                     
*                                                                               
         CLI   QGRP,C'D'           DELETING SPILL ELEMENTS                      
         BNE   *+8                                                              
         MVI   MVSPILL,C'D'                                                     
*                                                                               
*        BUILD DATA TO MOVE TABLE                                               
*                                                                               
         BRAS  RE,MVTBBLD          BUILD DATA TO MOVE TABLE                     
*                                                                               
*        BUILD STATION TABLE                                                    
*                                                                               
         BRAS  RE,STABBLD          BUILD STATION TABLE                          
*                                                                               
*****    GOTO1 AENDREQ             ALL DONE                                     
*                                                                               
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - OPENFILE'                   
***********************************************************************         
*                                                                     *         
*        OPEN INPUT AND OUTPUT SPOT FILES                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OPENFILE DS    0H                                                               
*                                                                               
         CLI   MVRDTYPE,C'T'       IF READING FROM TAPE                         
         BNE   OPNTAPEN                                                         
*                                                                               
         OPEN  (TAPEIN,(INPUT))      SPTFILE DUMP TAPE                          
*                                                                               
OPNTAPEN DS    0H                                                               
*                                                                               
         OPEN  (FILEOUT,(OUTPUT))                                               
*                                                                               
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - DMXREC'                     
***********************************************************************         
*                                                                     *         
*        READ DUMP TAPE AND MOVE RECORDS TO OUTPUT TAPE               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INLOOP   DS    0H                                                               
*                                                                               
         BRAS  RE,GETNEXT          FIND NEXT RECORD                             
         BNE   INDONE              END OF FILE                                  
*                                                                               
         L     R3,AREC             POINT TO FOUND RECORD                        
*                                                                               
*        DETERMINE RECORD TYPE                                                  
*                                                                               
         CLI   0(R3),X'00'         TEST HEADER                                  
         BNE   *+12                                                             
         BRAS  RE,HDR                                                           
         B     INCONT                                                           
*                                                                               
         CLI   0(R3),X'10'         TEST BUYREC                                  
         BNH   *+12                                                             
         BRAS  RE,BUY                                                           
         B     INCONT                                                           
*                                                                               
         CLI   0(R3),X'02'         TEST GOALREC                                 
         BNE   *+12                                                             
         BRAS  RE,GOAL                                                          
         B     INCONT                                                           
*                                                                               
         CLI   0(R3),X'0D'         TEST CLIENT GROUP                            
         BNE   *+8                                                              
         CLI   1(R3),X'04'         TEST CLIENT GROUP                            
         BNE   *+12                                                             
         BRAS  RE,GRP                                                           
         B     INCONT                                                           
*                                                                               
         B     INCONT                                                           
*                                                                               
INCONT   DS    0H                                                               
         B     INLOOP                                                           
*                                                                               
INDONE   DS    0H                                                               
*                                                                               
         CLI   MVRDTYPE,C'T'       IF READING FROM TAPE                         
         BNE   CLSTAPEN                                                         
*                                                                               
         CLOSE TAPEIN              CLOSE FILES                                  
*                                                                               
CLSTAPEN DS    0H                                                               
*                                                                               
         CLOSE FILEOUT                                                          
*                                                                               
         MVI   LINE,100            FORCE NEW PAGE                               
*                                                                               
         GOTO1 REPORT              PRINT BLANK LINE                             
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
REQ1STX  DS    0H                                                               
*                                                                               
         MVC   P(25),=C'RECORDS ADDED TO FILE = '                               
         EDIT  (P8,RECCNTR),(15,P+25),COMMAS=YES,ALIGN=LEFT                     
         EDIT  (P8,WRCTR),(15,P+40),COMMAS=YES,ALIGN=LEFT                       
*                                                                               
         GOTO1 REPORT              PRINT RECORD COUNT                           
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
         GOTO1 AENDREQ             ALL DONE                                     
*                                                                               
SPTFILES DS    0H                                                               
         DC    CL8' SPTDIR '                                                    
         DC    CL8' SPTFIL '                                                    
         DC    CL8' STAFILE'                                                    
         DC    CL8'X'                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT A LITTLE TRACE OF DELETED ELEMENTS                                      
***********************************************************************         
         SPACE 1                                                                
PRTDEL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CP    PRTCOUNT,=P'50'                                                  
         BH    PRTDEL2                                                          
*                                                                               
         AP    PRTCOUNT,=P'1'                                                   
         MVC   P(1),PRTFLAG                                                     
         MVC   P+1(3),=C'DEL'                                                   
*                                                                               
         GOTO1 HEXOUT,DMCB,(R3),P+10,13,0,0   PRINT KEY                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,1(R6)          ELEMENT LENGTH                               
         GOTO1 HEXOUT,DMCB,(R6),P+40,(RF),0,0   PRINT REGULAR ELEMENT           
*                                                                               
         CLI   MVERROPT,C'E'       SKIP IF ONLY PRINTING ERRORS                 
         BE    PRTDELA                                                          
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
PRTDELA  DS    0H                                                               
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
         MVI   PRTFLAG,C' '                                                     
*                                                                               
PRTDEL2  GOTO1 RECUP,DMCB,(R3),(R6)                                             
*                                                                               
         CLI   0(R6),X'0D'                                                      
         BL    PRTDELX                                                          
         CLI   0(R6),X'20'                                                      
         BL    PRTDEL2                                                          
*                                                                               
PRTDELX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - GETCBL'                     
***********************************************************************         
*                                                                     *         
*        FIND LOCAL CABLE IN CONVERSION TABLE                         *         
*                                                                     *         
*NTRY    R2==> BINARY MARKET/STATION CODE                             *         
*        R4==> ENTRY IN TABLE OF THINGS TO MOVE                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETCBL   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         USING MVTABD,R4           ESTABLISH MOVE TABLE ENTRY                   
*                                                                               
*        UNPACK OLD MARKET/STATION/NETWORK CODE                                 
*                                                                               
         ST    R2,GCBAMKST         SAVE A(PACKED MKT/STA/NET)                   
*                                                                               
         XC    STAWORK,STAWORK     INIT STAPACK WORKAREA                        
         LA    R6,STAWORK                                                       
         USING STAPACKD,R6                                                      
*                                                                               
         MVI   STAPACT,C'U'        ACTION - UNPACK                              
         MVC   STAPAGY,MVTCHOLD    AGENCY                                       
         MVC   STAPCTRY,COUNTRY                                                 
         MVC   STAPMED,MVTMDOLD    MEDIA                                        
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMKST,0(R2)      MKT/STA/NET                                  
*                                                                               
         GOTO1 VSTAPACK,STAWORK    UNPACK MKT/STA/NET                           
*                                                                               
         CLI   STAPERR,0                                                        
         BNE   GETCBLER                                                         
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BNE   GETCBL1                                                          
*                                                                               
         L     R5,AREC             POINT TO INCOMING RECORD                     
         GOTO1 HEXOUT,DMCB,(R5),P+2,13,0,0   PRINT OLD KEY                      
*                                                                               
         MVC   P+65(4),STAPQMKT       OLD MARKET                                
         MVC   P+70(8),STAPQSTA       OLD STATION/NETWORK                       
*                                                                               
         GOTO1 HEXOUT,DMCB,0(R2),P+95,5,0,0                                     
GETCBL1  DS    0H                                                               
*                                                                               
         MVC   GCBQSTNT,STAPQSTA   SAVE STATION/NETWORK                         
*                                                                               
         MVI   STAPACT,C'P'        SET ACTION TO PACK                           
         MVC   STAPQNET,=C'   '    CLEAR NETWORK                                
*                                                                               
         GOTO1 VSTAPACK,STAWORK    PACK MKT/STA  WITHOUT NETWORK                
*                                                                               
         CLI   STAPERR,0                                                        
         BNE   GETCBLER                                                         
*                                                                               
         LA    R2,STAPMKST         POINT TO BASE PACKED MKT/STA                 
*                                                                               
         MVI   SWITCH,C'C'         INDICATE FROM GET CABLE ROUTINE              
         BRAS  RE,GETSTA           TRANSLATE STATION CALL LETTERS               
         MVI   SWITCH,0            RESET SWITCH                                 
         BNZ   GETCBLER            ERROR                                        
*                                                                               
         L     RF,UTL              POINT TO UTL                                 
         MVC   4(1,RF),AGYSENEW    SET TO NEW AGENCIES FILES                    
*                                                                               
         MVI   STAPACT,C'U'        ACTION UNPACK                                
         MVC   STAPAGY,MVTCHNEW    AGENCY                                       
         MVC   STAPMED,MVTMDNEW    MEDIA                                        
         MVC   STAPQSTA(8),SPACES  INIT OUTPUT AREA                             
*                                                                               
         GOTO1 VSTAPACK,STAWORK    UNPACK BASE MKT/STA                          
*                                                                               
         CLI   STAPERR,0                                                        
         BNE   GETCBLER                                                         
*                                                                               
         MVI   STAPACT,C'P'        ACTION PACK                                  
         MVC   STAPQNET,GCBQNET    RESTORE NETWORK                              
*                                                                               
         GOTO1 VSTAPACK,STAWORK    PACK MKT/STA/NET                             
*                                                                               
         CLI   STAPERR,0                                                        
         BNE   GETCBLER                                                         
*                                                                               
         L     RF,UTL              POINT TO UTL                                 
         MVC   4(1,RF),AGYSEOLD    SET TO OLD AGENCIES FILES                    
*                                                                               
         L     R2,GCBAMKST         RE-POINT TO INCOMING MKT/STA                 
*                                                                               
         CLC   2(3,R2),STAPMKST+2  FORCE PRINTING IF HEADEND DIFFERENT          
         BE    *+16                                                             
         MVC   P+120(7),=CL7'HEADEND'                                           
         ZAP   CTR,=P'10'                                                       
*                                                                               
         MVC   0(5,R2),STAPMKST    SET NEW MARKET/STATION                       
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BL    GETCBLX                                                          
*                                                                               
         L     R3,=A(NEWREC)       POINT TO NEW RECORD                          
         GOTO1 HEXOUT,DMCB,(R3),P+32,13,0,0   PRINT NEW KEY                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,STAPMKST       UNPACK MARKET NUMBER                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  P+80(4),DUB         MARKET                                       
         MVC   P+85(8),STAPQSTA    STATION                                      
         GOTO1 HEXOUT,DMCB,STAPMKST,P+110,5,0,0   PRINT NEW MARKET STA          
*                                                                               
         CLI   MVERROPT,C'E'       SKIP IF ONLY PRINTING ERRORS                 
         BE    GETCBLX                                                          
         GOTO1 REPORT                                                           
*                                                                               
GETCBLX  DS    0H                                                               
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
         B     GETCBLXX                                                         
*                                                                               
GETCBLER DS   0H                   PRINT TRACE IF NO MATCH                      
*                                                                               
         MVC   P(132),SPACES       INIT PRINT LINE                              
*                                                                               
         L     R5,AREC             POINT TO INCOMING RECORD                     
         GOTO1 HEXOUT,DMCB,(R5),P+2,13,0,0   PRINT OLD KEY                      
*                                                                               
         MVC   P+65(4),STAPQMKT       OLD MARKET                                
         MVC   P+70(8),STAPQSTA       OLD STATION                               
*                                                                               
         MVC   P+90(15),=CL15'UNKNOWN STATION'                                  
*                                                                               
GETCBLEX DS    0H                                                               
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
         L     RF,UTL              POINT TO UTL                                 
         MVC   4(1,RF),AGYSEOLD    SET TO OLD AGENCIES FILES                    
*                                                                               
         LTR   RB,RB               SET NON-ZERO CC                              
         B     GETCBLXZ                                                         
*                                                                               
GETCBLXX DS    0H                                                               
*                                                                               
         SP    CTR,=P'1'           DECREMENT COUNTER                            
         BP    *+10                                                             
         ZAP   CTR,=P'10'          RESET COUNTER                                
*                                                                               
         CR    RB,RB               SET =CC                                      
         B     GETCBLXZ                                                         
*                                                                               
GETCBLXZ DS    0H                                                               
         XIT1                                                                   
*                                                                               
GCBCTR   DS    PL2                 CTR SAVEAREA                                 
GCBAMKST DS    A                   A(INCOMING PACKED MKT/STA)                   
GCBQSTNT DS    0CL8                UNPACKED STA/NET SAVEAREA                    
GCBQSTA  DS    CL5                 STATION                                      
GCBQNET  DS    CL3                 NETWORK                                      
*                                                                               
         LTORG                                                                  
*                                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - GETSTA'                     
***********************************************************************         
*                                                                     *         
*        FIND STATION IN CONVERSION TABLE                             *         
*                                                                     *         
*NTRY    R2==> BINARY MARKET CODE                                     *         
*        R4==> ENTRY IN TABLE OF THINGS TO MOVE                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETSTA   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         USING MVTABD,R4           ESTABLISH MOVE TABLE ENTRY                   
*                                                                               
         LA    R7,STAEXCPS         POINT TO STATION EXCEPTIONS TABLE            
         USING STANTRYD,R7         ESTABLISH AS ENTRY IN STATION TABLE          
*                                                                               
GSTEXCLP DS    0H                                                               
*                                                                               
         CLI   STTAMOLD,X'00'      DONE AT END OF TABLE                         
         BE    GSTEXCDN                                                         
*                                                                               
         CLC   MVTAMOLD,STTAMOLD   MATCH ON AGENCY/MEDIA                        
         BNE   GSTEXCCN                                                         
*                                                                               
         CLC   STTMSOLD,0(R2)      MATCH ON INCOMING MKT/STATION                
         BE    GSTEXCFD                                                         
*                                                                               
GSTEXCCN DS    0H                                                               
*                                                                               
         LA    R7,STANTRYL(R7)     BUMP TO NEXT TABLE ENTRY                     
         B     GSTEXCLP                                                         
*                                                                               
GSTEXCFD DS    0H                                                               
*                                                                               
         L     R5,AREC             POINT TO INCOMING RECORD                     
         GOTO1 HEXOUT,DMCB,(R5),P+2,13,0,0   PRINT NEW KEY                      
*                                                                               
         ZAP   CTR,=P'10'          FORCE TRACE                                  
*                                                                               
         B     GSTCLTFD                                                         
*                                                                               
GSTEXCDN DS    0H                                                               
*                                                                               
         LA    R3,BSPSTA           POINT TO STATION BINSRCH PARAMETERS          
         USING BSRPRMD,R3          ESTABLISH BINSRCH PARAMETERES                
*                                                                               
         CLI   SWITCH,C'C'         SKIP IF CALLED FROM GETCBL                   
         BE    GETSTA1                                                          
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BNE   GETSTA1                                                          
*                                                                               
         L     R5,AREC             POINT TO INCOMING RECORD                     
         GOTO1 HEXOUT,DMCB,(R5),P+2,13,0,0   PRINT OLD KEY                      
*                                                                               
GETSTA1  DS    0H                                                               
*                                                                               
         CLI   MVTMDOLD,C'N'       SKIP IF CANADIAN NETWORK                     
         BE    GSTCLTF1                                                         
*                                                                               
         LA    R7,STANTRYC         ESTABLISH BINSRCH TABLE WORKAREA             
         USING STANTRYD,R7                                                      
*                                                                               
         XC    STANTRY(STANTRYL),STANTRY INIT WORKAREA                          
*                                                                               
         MVC   STTAMOLD,MVTAMOLD   SET OLD AGY/MD  IN KEY                       
         MVC   STTMSOLD,0(R2)      SET OLD MKT/STA IN KEY                       
*                                                                               
         GOTO1 BINSRCH,BSPPRMS,('BSPRDHI',STTKEY)   FIND IN TABLE               
*                                                                               
         CLI   BSPCNTL,BSPNF       SKIP IF NOT FOUND                            
         BE    GETSTAER                                                         
*                                                                               
         ICM   R7,15,BSPAREC       POINT TO FOUND RECORD                        
         BZ    GETSTAER            SKIP IF NOT FOUND                            
*                                                                               
         CLC   STTAMOLD,MVTAMOLD   MUST MATCH AGY/MED                           
         BNE   GETSTAER                                                         
*                                                                               
         CLC   STTMSOLD,0(R2)      MUST MATCH MKT/STA                           
         BNE   GETSTAER                                                         
*                                                                               
GSTCLTFD DS    0H                                                               
*                                                                               
         OC    STTMSNEW,STTMSNEW   ERROR IF NO NEW MKT/STA                      
         BZ    GETSTAE1                                                         
*                                                                               
         MVC   0(5,R2),STTMSNEW    SET NEW MARKET/STATION                       
*                                                                               
         CLI   SWITCH,C'C'         SKIP IF CALLED FROM GETCBL                   
         BE    GETSTAXX                                                         
*                                                                               
GSTCLTF1 DS    0H                                                               
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BL    GETSTAXX                                                         
*                                                                               
         CLI   MVTMDOLD,C'N'       SKIP IF CANADIAN NETWORK                     
         BE    GETSTAF2                                                         
*                                                                               
         L     R5,AREC             POINT TO INCOMING RECORD                     
         GOTO1 HEXOUT,DMCB,(R5),P+2,13,0,0   PRINT OLD KEY                      
*                                                                               
         MVC   P+65(4),STTMKOLD       OLD MARKET                                
         MVC   P+70(5),STTSTOLD       OLD STATION                               
         MVC   P+76(3),STTCLOLD       OLD CLIENT                                
*                                                                               
         MVC   P+80(4),STTMKNEW       NEW MARKET                                
         MVC   P+85(5),STTSTNEW       NEW STATION                               
*                                                                               
GETSTAF2 DS    0H                                                               
*                                                                               
         L     R3,=A(NEWREC)       POINT TO NEW RECORD                          
         GOTO1 HEXOUT,DMCB,(R3),P+32,13,0,0   PRINT NEW KEY                     
*                                                                               
GETSTAX  DS    0H                                                               
*                                                                               
         CLI   MVERROPT,C'E'       SKIP IF ONLY PRINTING ERRORS                 
         BE    GETSTAX1                                                         
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
GETSTAX1 DS    0H                                                               
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
         B     GETSTAXX                                                         
*                                                                               
GETSTAER DS   0H                   PRINT TRACE IF NO MATCH                      
*                                                                               
         LA    R7,STANTRYC         ESTABLISH BINSRCH TABLE WORKAREA             
         USING STANTRYD,R7                                                      
*                                                                               
GETSTAE1 DS   0H                   HAVE STATION TABLE ENTRY                     
*                                                                               
         CLI   SWITCH,C'C'         SKIP IF CALLED FROM GETCBL                   
         BE    GETSTAE2                                                         
*                                                                               
         L     R5,AREC                                                          
         GOTO1 HEXOUT,DMCB,(R5),P+2,13,0,0   PRINT OLD KEY                      
*                                                                               
         MVC   P+65(4),STTMKOLD       OLD MARKET                                
         MVC   P+70(5),STTSTOLD       OLD STATION                               
         MVC   P+76(3),STTCLOLD       OLD CLIENT                                
*                                                                               
         MVC   P+90(15),=CL15'UNKNOWN STATION'                                  
*                                                                               
GETSTAEX DS    0H                                                               
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
GETSTAE2 DS    0H                                                               
*                                                                               
         LTR   RB,RB               SET NON-ZERO CC                              
         B     GETSTAXZ                                                         
*                                                                               
GETSTAXX DS    0H                                                               
*                                                                               
         CLI   SWITCH,C'C'         SKIP IF CALLED FROM GETCBL                   
         BE    GETSTAXY                                                         
*                                                                               
         SP    CTR,=P'1'           DECREMENT COUNTER                            
         BP    *+10                                                             
         ZAP   CTR,=P'10'          RESET COUNTER                                
*                                                                               
GETSTAXY DS    0H                                                               
*                                                                               
         CR    RB,RB               SET =CC                                      
         B     GETSTAXZ                                                         
*                                                                               
GETSTAXZ DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
*      IF YOU USE THIS FEATURE CHECK THAT STATION TABLE HASN'T CHANGED          
*                                                                               
*        TABLE OF MKT/STATION EXCEPTIONS                                        
*        SAME FORMAT AS STATION TABLE                                           
*                                                                               
STAEXCPS DS    0D                  EXCEPTION STATIONS                           
******   DC    XL1'92'             NEW AGENCY/MEDIA                             
******   DC    XL5'00E8C68A61'     NEW MARKET/STATION                           
******   DC    XL2'AC01'           NEW CLIENT - PACKED                          
******   DC    CL4'0232'           NEW MARKET NUMBER                            
******   DC    CL5'WHFSF'          NEW STATION                                  
******   DC    CL3'LAB'            NEW CLIENT                                   
******   DC    XL1'12'             NEW AGENCY/MEDIA                             
******   DC    XL5'00E8C68A61'     NEW MARKET/STATION                           
******   DC    CL4'0232'           NEW MARKET NUMBER                            
******   DC    CL5'WHFSF'          NEW STATION                                  
*                                                                               
         DC    X'00'               EOT                                          
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - GETEL '                     
***********************************************************************         
*                                                                     *         
*        GETEL MACRO                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         GETEL R6,42,ELCODE                                                     
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - GETMKT'                    
***********************************************************************         
*                                                                     *         
*        FIND MARKET  IN CONVERSION TABLE                             *         
*                                                                     *         
*NTRY    R2==> BINARY MARKET CODE                                     *         
*        R4==> ENTRY IN TABLE OF THINGS TO MOVE                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETMKT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING MVTABD,R4           ESTABLISH MOVE TABLE ENTRY                   
*                                                                               
         BRAS  RE,SPCMKT          CHECK IF A SPECIAL MARKET                     
         BH    GETMKTER           ERROR                                         
         BL    GETMKTA            NOT FOUND IN TABLE                            
*                                                                               
         EDIT  (B2,HALF),(4,P+65)   OLD MARKET                                  
         EDIT  (B2,0(R2)),(4,P+80)   NEW MARKET                                 
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     GETMKTX                                                          
*                                                                               
GETMKTA  DS    0H                                                               
*                                                                               
         LA    R3,BSPSTA           POINT TO STATION BINSRCH PARAMETERS          
         USING BSRPRMD,R3          ESTABLISH BINSRCH PARAMETERES                
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BNE   GETMKT1                                                          
*                                                                               
         L     R5,AREC             POINT TO INCOMING RECORD                     
         GOTO1 HEXOUT,DMCB,(R5),P+2,13,0,0   PRINT OLD KEY                      
*                                                                               
GETMKT1  DS    0H                                                               
*                                                                               
         LA    R7,STANTRYC         ESTABLISH BINSRCH TABLE WORKAREA             
         USING STANTRYD,R7                                                      
*                                                                               
         XC    STANTRY(STANTRYL),STANTRY INIT WORKAREA                          
*                                                                               
         MVC   STTAMOLD,MVTAMOLD   SET OLD AGY/MED IN KEY                       
*                                                                               
         TM    STTAMOLD,X'08'      IF MEDIA C                                   
         BNO   *+12                                                             
         NI    STTAMOLD,X'FF'-X'08'   CHANGE TO TV                              
         OI    STTAMOLD,X'01'         CHANGE TO TV                              
*                                                                               
         MVC   STTMSOLD(2),0(R2)   SET OLD MKT     IN KEY                       
*                                                                               
         GOTO1 BINSRCH,BSPPRMS,('BSPRDHI',STTKEY)   FIND IN TABLE               
*                                                                               
         CLI   BSPCNTL,BSPNF       SKIP IF NOT FOUND                            
         BE    GETMKTER                                                         
*                                                                               
         ICM   R7,15,BSPAREC       POINT TO FOUND RECORD                        
         BZ    GETMKTER            SKIP IF NOT FOUND                            
*                                                                               
GMKCLTLP DS    0H                                                               
*                                                                               
         OC    STTMSNEW,STTMSNEW   SKIP IF NO NEW MKT/STA                       
         BZ    GMKCLTCN                                                         
*                                                                               
         CLC   STTMSOLD(2),0(R2)   FIND MARKET MATCH                            
         BNE   GETMKTER                                                         
*                                                                               
         CLC   STTCPOLD,=X'FFFF'   USE IF MASTER STATION                        
         BE    GMKCLTFD                                                         
*                                                                               
         CLC   STTCPOLD,WRKCLT     USE IF CLIENT MATCHES                        
         BE    GMKCLTFD                                                         
*                                                                               
GMKCLTCN DS    0H                                                               
*                                                                               
         LA    R7,STANTRYL(R7)     BUMP TO NEXT TABLE ENTRY                     
         B     GMKCLTLP                                                         
*                                                                               
GMKCLTFD DS    0H                                                               
*                                                                               
         MVC   0(2,R2),STTMSNEW    SET NEW MARKET                               
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BL    GETMKTXX                                                         
*                                                                               
         MVC   P+65(4),STTMKOLD       OLD MARKET                                
*                                                                               
         L     R5,=A(NEWREC)                                                    
         GOTO1 HEXOUT,DMCB,(R5),P+32,13,0,0   PRINT NEW KEY                     
*                                                                               
         MVC   P+80(4),STTMKNEW       NEW MARKET                                
*                                                                               
GETMKTX  DS    0H                                                               
*                                                                               
         CLI   MVERROPT,C'E'       SKIP IF ONLY PRINTING ERRORS                 
         BE    GETMKTX1                                                         
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
GETMKTX1 DS    0H                                                               
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
         B     GETMKTXX                                                         
*                                                                               
GETMKTER DS   0H                   PRINT TRACE IF NO MATCH                      
*                                                                               
         L     R5,AREC                                                          
         GOTO1 HEXOUT,DMCB,(R5),P+2,13,0,0   PRINT OLD KEY                      
*                                                                               
         EDIT  (B2,0(R2)),(4,P+65)   OLD MARKET                                 
*                                                                               
*******  LA    R5,NEWREC                                                        
*******  GOTO1 HEXOUT,DMCB,(R5),P+32,13,0,0   PRINT NEW KEY                     
*                                                                               
         MVC   P+80(15),=CL15'UNKNOWN MARKET'                                   
*                                                                               
GETMKTEX DS    0H                                                               
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
         LTR   RB,RB               SET NE CC                                    
         B     GETMKTXZ                                                         
*                                                                               
GETMKTXX DS    0H                                                               
*                                                                               
         SP    CTR,=P'1'           DECREMENT COUNTER                            
         BP    *+10                                                             
         ZAP   CTR,=P'10'          RESET COUNTER                                
*                                                                               
         CR    RB,RB               SET EQ CC                                    
*                                                                               
GETMKTXZ DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SPREPBW02  - MOVE BETWEEN AGENCIES - SPCMKT'                    
***********************************************************************         
*                                                                     *         
*        CHECK IF MARKET IS IN SPECIAL TABLE                          *         
*                                                                     *         
*NTRY    R2==> BINARY MARKET CODE                                     *         
*        R4==> ENTRY IN TABLE OF THINGS TO MOVE                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPCMKT   NTR1  LABEL=*                                                          
*                                                                               
         USING MVTABD,R4          ESTABLISH MOVE TABLE ENTRY                    
*                                                                               
         LA    RF,SMKTTAB         START OF TABLE                                
*                                                                               
SPCMKTLP DS    0H                                                               
*                                                                               
         CLI   0(RF),X'FF'        TEST FOR END OF TABLE                         
         BE    SPCMKTDN                                                         
*                                                                               
         CLC   MVTAMOLD,0(RF)     MATCH ON AGENCY/MEDIA                         
         BNE   SPCMKTCN                                                         
*                                                                               
         CLC   0(2,R2),1(RF)      MATCH ON OLD MKT NUMBER                       
         BE    SPCMKTFD                                                         
*                                                                               
SPCMKTCN DS    0H                                                               
*                                                                               
         LA    RF,SMKTTABL(RF)    BUMP TO NEXT ENTRY                            
         B     SPCMKTLP                                                         
*                                                                               
SPCMKTFD DS    0H                                                               
*                                                                               
         MVC   HALF,0(R2)         SAVE OLD MARKET                               
         MVC   0(2,R2),3(RF)      RETURN NEW MKT NUMBER                         
*                                                                               
         LA    RE,1               SET RETURN CODE                               
*                                                                               
         OC    0(2,R2),0(R2)      IF ZERO MKT NUMBER                            
         BNZ   *+8                                                              
         LA    RE,2                  SET RETURN CODE                            
*                                                                               
         B     SPCMKTX                                                          
*                                                                               
SPCMKTDN DS    0H                                                               
*                                                                               
         LA    RE,0               SET RETURN CODE                               
         B     SPCMKTX                                                          
*                                                                               
SPCMKTX  DS    0H                                                               
         CHI   RE,1               SET CONDITION CODES                           
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
SMKTTAB  DS    0D                 SPECIAL MKT TRANSLATE TABLE                   
         DC    X'FF',AL2(0191),AL2(0191) A/M,OLD MKT,NEW MKT                    
SMKTTABL EQU   *-SMKTTAB          ENTRY LENGTH                                  
         TITLE 'SPREPBW02  - MOVE BETWEEN AGENCIES - COPY'                      
***********************************************************************         
*                                                                     *         
*        COPY CURRENT RECORD TO WORKAREA                              *         
*                                                                     *         
*NTRY    R3==> RECORD TO BE COPIED                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
COPY     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R0,R3               POINT TO INCOMING RECORD                     
         L     RE,=A(NEWREC)       POINT TO STORAGE AREA                        
         SR    R1,R1                                                            
         ICM   R1,3,13(R3)         GET RECORD LENGTH                            
         LR    RF,R1               COPY LENGTH                                  
*                                                                               
         LA    RF,4(RF)            ALLOW FOR LENGTH BYTES                       
         AHI   RE,-4                                                            
         STCM  RF,3,0(RE)          SET OUTPUT FILE LENGTH                       
         AHI   RE,4                                                             
         AHI   RF,-4               RESTORE TRUE RECORD LENGTH                   
*                                                                               
         MVCL  RE,R0               COPY INCOMING RECORD                         
*                                                                               
         SR    R1,R1                                                            
         L     RE,=A(NEWREC)       POINT TO STORAGE AREA                        
         ICM   R1,3,13(RE)         GET RECORD LENGTH                            
*                                                                               
         LA    RF,0(R1,RE)         POINT TO END OF NEW RECORD                   
         MVI   0(RF),0             FORCE ENDING NULLS                           
*                                                                               
         CLC   AGYCHOLD,20(RE)    IF ALPHA AGENCY PRESENT                       
         BNE   *+10                                                             
         MVC   20(2,RE),AGYCHNEW                                                
*                                                                               
COPYX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - WRITE'                     
***********************************************************************         
*                                                                     *         
*        WRITE WORKAREA RECORD TO OUTPUT DATASET                      *         
*                                                                     *         
*NTRY    NEWREC HAS RECORD TO GO                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RECWRITE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R1,=A(FILEOUT)                                                   
         L     R2,=A(RRLEN)        POINT TO RECORD                              
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,4+13(R2)       GET LENGTH OF RECORD                         
         AHI   RF,4                ADD ON RRLEN                                 
*                                                                               
         LA    RE,0(RF,R2)         POINT TO END OF RECORD                       
         MVI   0(RE),0             SET TRAILING NULL                            
         STCM  RF,3,0(R2)          UPDATE RRLEN                                 
*                                                                               
         CLC   SAVEKEY,4(R2)       SKIP IF DUPLICATE RECORD                     
******   BNE   *+6                                                              
******   DC    H'0'                                                             
******   BE    RECWRITX                                                         
*                                                                               
         MVC   SAVEKEY,4(R2)       SAVE CURRENT KEY                             
*                                                                               
         PUT   (1),(R2)            WRITE TO OUTPUT FILE                         
*                                                                               
         AP    RECCNTR,=P'1'       INCREMENT RECORD COUNTER                     
*                                                                               
         CLI   MVWRTYPE,C'W'       SKIP IF WRITING TO FILE                      
         BNE   RECWRITX                                                         
*                                                                               
         MVC   RCWKEY,KEY          SAVE CURRENT KEY                             
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),AGYSENEW    SET UTL TO NEW AGENCY                        
*                                                                               
         ICM   R0,15,AREC          SAVE CURRENT A(REC)                          
         MVC   RCFADBUY,ADBUY                                                   
*                                                                               
         AHI   R2,4                POINT TO BUYRECORD                           
         STCM  R2,15,AREC                                                       
         STCM  R2,15,ADBUY                                                      
*                                                                               
         GOTO1 ADD                                                              
*                                                                               
         STCM  R0,15,AREC          RESTORE A(REC)                               
         MVC   ADBUY,RCFADBUY                                                   
*                                                                               
         AP    WRCTR,=P'1'                                                      
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),AGYSEOLD    SET UTL TO OLD AGENCY                        
*                                                                               
         MVC   KEY,RCWKEY          RESTORE CURRENT KEY                          
*                                                                               
         GOTO1 HIGH                RESET FILE POINTERS                          
*                                                                               
RECWRITX DS    0H                                                               
         XIT1                                                                   
*                                                                               
RCFADBUY DS    A                                                                
RCWKEY   DS    CL13                KEY SAVEAREA                                 
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - GETNEXT'                    
***********************************************************************         
*                                                                     *         
*        READ NEXT RECORD FROM TAPE OR FILE                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETNEXT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9       ESTABLISH SPONSOR WORKING STORAGE            
         USING WORKD,RC            ESTABLISH LOCAL WORKING STORAGE              
*                                                                               
         CLI   MVRDTYPE,C'T'       IF READING FROM TAPE                         
         BNE   GTNTAPEN                                                         
*                                                                               
*        READ FROM TAPE                                                         
*                                                                               
         L     R3,=A(DARECH)          RECORD INPUT AREA                         
         L     R1,=A(TAPEIN)                                                    
         GET   (1),(R3)               GET NEXT SPOT RECORD                      
*                                                                               
         AHI   R3,4                   POINT TO START OF RECORD                  
         ST    R3,AREC                SAVE RECORD POINTER                       
*                                                                               
         B     GTNOKAY                                                          
*                                                                               
GTNTAPEN DS    0H                  READ FILE                                    
*                                                                               
*        READ SPOT FILE                                                         
*                                                                               
         CLI   GTNRECCD,C' '       IF FIRST TIME                                
         BH    GTNHDR10                                                         
*                                                                               
         MVI   GTNRECCD,C'H'          SET FOR HEADER KEYS                       
*                                                                               
         L     R4,=A(MVTAB)           POINT TO FIRST ENTRY                      
         ST    R4,GTNTABA             RESET TABLE POINTER                       
         USING MVTABD,R4           ESTABLISH MOVE TABLE ENTRY                   
*                                                                               
         XC    GTNKEY,GTNKEY       INIT MASTER KEY SAVEAREA                     
*                                                                               
         LA    R3,GTNKEY           ESTABLISH AS ESTIMATE KEY                    
         USING ESTHDR,R3                                                        
*                                                                               
         MVC   EKEYAM,MVTAMOLD     SET AGENCY/MEDIA                             
         MVC   EKEYCLT,MVTCLTPK    SET CLIENT - PACKED                          
*                                                                               
         MVC   KEY,GTNKEY          SET STARTING KEY                             
*                                                                               
         GOTO1 HIGH                READ FIRST HEADER FOR TABLE ENTRY            
*                                                                               
         B     GTNHDRLP                                                         
*                                                                               
GTNHDR10 DS    0H                                                               
*                                                                               
         L     R4,GTNTABA          POINT TO  CURRENT MOVE TABLE ENTRY           
         USING MVTABD,R4           ESTABLISH MOVE TABLE ENTRY                   
*                                                                               
         GOTO1 SEQ                 READ NEXT POINTER ON FILE                    
*                                                                               
         CLI   GTNRECCD,C'H'       SKIP IF NOT HEADER RECORDS                   
         BNE   GTNHDRN                                                          
*                                                                               
GTNHDRLP DS    0H                                                               
*                                                                               
         LA    R3,KEY              ESTABLISH AS ESTIMATE KEY                    
         USING ESTHDR,R3                                                        
*                                                                               
         CLC   EKEY(EKEYPRD-EKEY),GTNKEY MATCH ON AGY/CLT                       
         BE    GTNGET              GO READ IN RECORD                            
*                                                                               
GTNHDRCN DS    0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      BUMP TO NEXT TABLE ENTRY                     
*                                                                               
         CLI   0(R4),X'FF'         DONE ON EOT                                  
         BE    GTNHDRDN                                                         
*                                                                               
         ST    R4,GTNTABA          RESET TABLE POINTER                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY              ESTABLISH BASE KEY                           
         USING ESTHDR,R3                                                        
*                                                                               
         MVC   EKEYAM,MVTAMOLD     SET AGENCY/MEDIA                             
         MVC   EKEYCLT,MVTCLTPK    SET CLIENT - PACKED                          
*                                                                               
         CLC   EKEY(EKEYPRD-EKEY),GTNKEY MATCH ON AGY/CLT                       
         BE    GTNHDRCN            NEED NEW CLIENT                              
*                                                                               
         MVC   GTNKEY,KEY          SAVE STARTING KEY                            
*                                                                               
         GOTO1 HIGH                READ FIRST HEADER FOR TABLE ENTRY            
*                                                                               
         B     GTNHDRLP                                                         
*                                                                               
GTNHDRDN DS    0H                                                               
*                                                                               
         CLI   MVGOALS,C'Y'        SKIP IF NOT MOVING GOALS                     
         BNE   GTNGOLDN                                                         
*                                                                               
         MVI   GTNRECCD,C'G'       SET FOR GOALS                                
*                                                                               
         L     R4,=A(MVTAB)           POINT TO FIRST ENTRY                      
         ST    R4,GTNTABA             RESET TABLE POINTER                       
         USING MVTABD,R4           ESTABLISH MOVE TABLE ENTRY                   
*                                                                               
         XC    GTNKEY,GTNKEY       INIT MASTER KEY SAVEAREA                     
*                                                                               
         LA    R3,GTNKEY           ESTABLISH AS GOAL KEY                        
         USING GKEY,R3                                                          
*                                                                               
         MVI   GKEYTYPE,X'02'      RECORD TYPE                                  
         MVC   GKEYAM,MVTAMOLD     SET AGENCY/MEDIA                             
         MVC   GKEYCLT,MVTCLTPK    SET CLIENT - PACKED                          
*                                                                               
         MVC   KEY,GTNKEY          SET STARTING KEY                             
*                                                                               
         GOTO1 HIGH                READ FIRST GOAL KEY                          
*                                                                               
GTNHDRN  DS    0H                                                               
*                                                                               
         CLI   GTNRECCD,C'G'       SKIP IF NOT READING GOAL RECORDS             
         BNE   GTNGOLN                                                          
*                                                                               
GTNGOLLP DS    0H                                                               
*                                                                               
         LA    R3,KEY              ESTABLISH AS GOAL KEY                        
         USING GKEY,R3                                                          
*                                                                               
         CLC   GKEY(GKEYPRD-GKEY),GTNKEY MATCH ON AGY/CLT                       
         BNE   GTNGOLCN                                                         
*                                                                               
         CLI   GKEY+12,0           ACCEPT ONLY MASTER POINTERS                  
         BE    GTNGET                                                           
*                                                                               
GTNGOLC1 DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT GOAL                               
*                                                                               
         B     GTNGOLLP                                                         
*                                                                               
GTNGOLCN DS    0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      BUMP TO NEXT TABLE ENTRY                     
*                                                                               
         CLI   0(R4),X'FF'         DONE ON EOT                                  
         BE    GTNGOLDN                                                         
*                                                                               
         ST    R4,GTNTABA          RESET TABLE POINTER                          
*                                                                               
         XC    KEY,KEY             INIT BASE KEY                                
         LA    R3,KEY              ESTABLISH BASE KEY                           
         USING GKEY,R3                                                          
*                                                                               
         MVI   GKEYTYPE,X'02'      RECORD TYPE                                  
         MVC   GKEYAM,MVTAMOLD     SET AGENCY/MEDIA                             
         MVC   GKEYCLT,MVTCLTPK    SET CLIENT - PACKED                          
*                                                                               
         CLC   GKEY(GKEYPRD-GKEY),GTNKEY MATCH ON AGY/CLT                       
         BE    GTNGOLCN            NEED NEW CLIENT                              
*                                                                               
         MVC   GTNKEY,KEY          SAVE STARTING KEY                            
*                                                                               
         GOTO1 HIGH                READ FIRST HEADER FOR TABLE ENTRY            
*                                                                               
         B     GTNGOLLP                                                         
*                                                                               
GTNGOLDN DS    0H                                                               
*                                                                               
         CLI   MVCGRP,C'Y'         SKIP IF NOT MOVING CLIENT GROUP              
         BNE   GTNGRPDN                                                         
*                                                                               
         MVI   GTNRECCD,C'C'       SET FOR GROUPS                               
*                                                                               
         L     R4,=A(MVTAB)           POINT TO FIRST ENTRY                      
         ST    R4,GTNTABA             RESET TABLE POINTER                       
         USING MVTABD,R4           ESTABLISH MOVE TABLE ENTRY                   
*                                                                               
         XC    GTNKEY,GTNKEY       INIT MASTER KEY SAVEAREA                     
*                                                                               
         LA    R3,GTNKEY           ESTABLISH AS GROUP KEY                       
         USING GRPRECD,R3                                                       
*                                                                               
         MVI   GRPKTYP,GRPKTYPQ    RECORD TYPE                                  
         MVI   GRPKSTYP,GRPKCTYQ   CLIENT GROUP                                 
         MVC   GRPKAGMD,MVTAMOLD   SET AGENCY/MEDIA                             
*                                                                               
         MVC   KEY,GTNKEY          SET STARTING KEY                             
*                                                                               
         GOTO1 HIGH                READ FIRST GROUP FOR TABLE ENTRY             
*                                                                               
GTNGOLN  DS    0H                                                               
*                                                                               
         CLI   GTNRECCD,C'C'       SKIP IF NOT READING GROUP RECORDS            
         BNE   GTNGRPN                                                          
*                                                                               
GTNGRPLP DS    0H                                                               
*                                                                               
         LA    R3,KEY              ESTABLISH AS GROUP KEY                       
         USING GRPKEY,R3                                                        
*                                                                               
         CLC   GRPKEY(GRPKID-GRPKEY),GTNKEY MATCH ON AGY/MED                    
         BE    GTNGET                                                           
*                                                                               
GTNGRPCN DS    0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      BUMP TO NEXT TABLE ENTRY                     
*                                                                               
         CLI   0(R4),X'FF'         DONE ON EOT                                  
         BE    GTNGRPDN                                                         
*                                                                               
         ST    R4,GTNTABA          RESET TABLE POINTER                          
*                                                                               
         XC    KEY,KEY             INIT BASE KEY                                
         LA    R3,GTNKEY           ESTABLISH AS GROUP KEY                       
         USING GRPRECD,R3                                                       
*                                                                               
         MVI   GRPKTYP,GRPKTYPQ    RECORD TYPE                                  
         MVI   GRPKSTYP,GRPKCTYQ   CLIENT GROUP                                 
         MVC   GRPKAGMD,MVTAMOLD   SET AGENCY/MEDIA                             
*                                                                               
         MVC   KEY,GTNKEY          SET STARTING KEY                             
*                                                                               
         CLC   GRPKEY(GRPKID-GRPKEY),GTNKEY MATCH ON AGY/MED                    
         BE    GTNGRPCN            NEED NEW AGY/MED                             
*                                                                               
         MVC   GTNKEY,KEY          SAVE STARTING KEY                            
*                                                                               
         GOTO1 HIGH                READ FIRST HEADER FOR TABLE ENTRY            
*                                                                               
         B     GTNGRPLP                                                         
*                                                                               
GTNGRPDN DS    0H                                                               
*                                                                               
         CLI   MVBUYS,C'Y'         SKIP IF NOT MOVING BUYS                      
         BE    *+12                                                             
         CLI   MVCABLE,C'Y'        OR LOCAL CABLE                               
         BNE   GTNBUYDN                                                         
*                                                                               
         MVI   GTNRECCD,C'B'       SET FOR BUYS                                 
*                                                                               
         L     R4,=A(MVTAB)           POINT TO FIRST ENTRY                      
         ST    R4,GTNTABA             RESET TABLE POINTER                       
         USING MVTABD,R4           ESTABLISH MOVE TABLE ENTRY                   
*                                                                               
         XC    GTNKEY,GTNKEY       INIT MASTER KEY SAVEAREA                     
*                                                                               
         LA    R3,GTNKEY           ESTABLISH AS BUY KEY                         
         USING BUYRECD,R3                                                       
*                                                                               
         MVC   BUYKAM,MVTAMOLD     SET AGENCY/MEDIA                             
         MVC   BUYKCLT,MVTCLTPK    SET CLIENT - PACKED                          
****     MVC   BUYKPRD,MVTPCD      SET PRODUCT                                  
         MVI   BUYKPRD,X'FF'       SET FOR PRODUCT POOL                         
*                                                                               
         MVC   KEY,GTNKEY          SET STARTING KEY                             
*                                                                               
         GOTO1 HIGH                READ FIRST HEADER FOR TABLE ENTRY            
*                                                                               
GTNGRPN  DS    0H                                                               
*                                                                               
         CLI   GTNRECCD,C'B'       SKIP IF NOT READING BUY RECORDS              
         BNE   GTNBUYN                                                          
*                                                                               
         LA    R3,KEY              ESTABLISH AS BUY KEY                         
*                                                                               
GTNBUYLP DS    0H                                                               
*                                                                               
         USING BUYKEY,R3                                                        
*                                                                               
         CLC   BUYKEY(BUYKPRD-BUYKEY),GTNKEY MATCH ON AGY/CLT                   
         BNE   GTNBUYCN                                                         
*                                                                               
         CLI   BUYKPRD,X'FF'       ACCEPT IF PRODUCT POOL                       
         BE    GTNBUY10                                                         
*                                                                               
         CLC   BUYKEY(BUYKMSTA-BUYKEY),GTNKEY MATCH ON AGY/CLT/PRD              
         BNE   GTNBUYCN                                                         
*                                                                               
GTNBUY10 DS    0H                                                               
*                                                                               
         CLI   BUYKEY+10,0         ACCEPT ONLY MASTER POINTERS                  
         BE    GTNGET                                                           
*                                                                               
         GOTO1 SEQ                 READ NEXT BUY                                
*                                                                               
         B     GTNBUYLP                                                         
*                                                                               
GTNBUYCN DS    0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      BUMP TO NEXT TABLE ENTRY                     
*                                                                               
         CLI   0(R4),X'FF'         DONE ON EOT                                  
         BE    GTNBUYDN                                                         
*                                                                               
         ST    R4,GTNTABA          RESET TABLE POINTER                          
*                                                                               
         XC    KEY,KEY             INIT BASE KEY                                
         LA    R3,KEY              ESTABLISH BASE KEY                           
         USING BUYKEY,R3                                                        
*                                                                               
         MVC   BUYKAM,MVTAMOLD     SET AGENCY/MEDIA                             
         MVC   BUYKCLT,MVTCLTPK    SET CLIENT - PACKED                          
****     MVC   BUYKPRD,MVTPCD      SET PRODUCT                                  
         MVI   BUYKPRD,X'FF'       SET FOR PRODUCT POOL                         
*                                                                               
         CLC   BUYKEY(BUYKMSTA-BUYKEY),GTNKEY MATCH ON AGY/CLT/PRD              
         BE    GTNBUYCN            NEED NEW PRODUCT                             
*                                                                               
****     CLI   BUYKPRD,X'FF'       ACCEPT IF PRODUCT POOL                       
****     BE    GTNBUY10                                                         
*                                                                               
         MVC   GTNKEY,KEY          SAVE STARTING KEY                            
*                                                                               
         GOTO1 HIGH                READ FIRST HEADER FOR TABLE ENTRY            
*                                                                               
         B     GTNBUYLP                                                         
*                                                                               
GTNBUYDN DS    0H                                                               
*                                                                               
GTNBUYN  DS    0H                                                               
*                                                                               
GTNEOF   DS    0H                  END OF FILE                                  
*                                                                               
         LTR   RB,RB                                                            
*                                                                               
         B     GETNEXTX                                                         
*                                                                               
GTNGET   DS    0H                  READ RECORD FROM SPTFILE                     
*                                                                               
         L     RF,=A(DAREC)                                                     
         ST    RF,AREC             SET A(INPUT AREA)                            
*                                                                               
         GOTO1 GET                 READ IN RECORD                               
*                                                                               
         L     RF,AREC             POINT TO FOUND RECORD                        
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,13(RF)         GET RECORD LENGTH                            
         AHI   RE,4                ADD IN LENGTH OF RRLEN FIELD                 
*                                                                               
         L     RF,=A(DARECH)                                                    
         STCM  RE,3,0(RF)          SET RRLEN                                    
*                                                                               
GTNOKAY  DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
*                                                                               
GETNEXTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
GTNRECCD DC    C' '                RECORD TYPE ID                               
*                                  C'H' - HEADERS                               
*                                  C'G' - GOALS                                 
*                                  C'C' - GROUPS                                
*                                  C'B' - BUYS                                  
*                                                                               
GTNTABA  DS    A                   A(CURRENT MOVE TABLE ENTRY)                  
*                                                                               
         DS    0D                  ALIGNMENT                                    
         DC    CL8'*GTNKEY*'       DUMP TAG                                     
GTNKEY   DS    XL(L'KEY)           MASTER KEY                                   
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - NEWPCD'                     
***********************************************************************         
*                                                                     *         
*        READ CLIENT HEADER ON NEW FILE AND SEARCH CLIST FOR          *         
*             PRODUCT  CODE OF NEW PRODUCT ID                         *         
*                                                                     *         
*NTRY    R4 ==> ENTRY IN MVTTAB                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
NEWPCD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9       ESTABLISH SPONSOR WORKING STORAGE            
         USING WORKD,RC            ESTABLISH LOCAL WORKING STORAGE              
*                                                                               
         USING MVTABD,R4           ESTABLISH MOVE TABLE ENTRY                   
*                                                                               
         CLI   MVTPCDNW,0          IF WE HAVE A NEW PRODUCT CODE                
         BNE   NEWPCDX                USE IT                                    
*                                                                               
*        READ NEW SPOT FILE                                                     
*                                                                               
         MVC   NPCKEY,KEY          SAVE CURRENT KEY                             
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),AGYSENEW    SET UTL TO NEW AGENCY                        
*                                                                               
         XC    KEY,KEY             INIT KEY BUILD AREA                          
*                                                                               
         LA    R3,KEY              ESTABLISH AS CLIENT KEY                      
         USING CLTHDRD,R3          ESTABLISH CLIENT HEADER KEY                  
*                                                                               
         MVC   CKEYAM,MVTAMNEW     SET AGENCY/MEDIA                             
         MVC   CKEYCLT,MVTCLPKN    SET CLIENT - PACKED                          
*                                                                               
         GOTO1 HIGH                READ CLIENT HEADER FOR TABLE ENTRY           
*                                                                               
         CLC   CKEY,KEYSAVE        MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R0,15,AREC          SAVE A(CURRENT IOAREA)                       
*                                                                               
         LA    RF,NPCCHDR                                                       
         ST    RF,AREC             SET A(INPUT AREA)                            
*                                                                               
         GOTO1 GET                 READ IN RECORD                               
*                                                                               
         L     R3,AREC             POINT TO FOUND RECORD                        
*                                                                               
         STCM  R0,15,AREC          RESTORE IOAREA POINTER                       
*                                                                               
         USING CLTHDRD,R3          ESTABLISH CLIENT HEADER                      
*                                                                               
         LA    R1,CLIST            SEARCH CLIST FOR NEW PRODUCT                 
         LA    R0,L'SAVCLIST/4     MAX NUMBER OF PRODUCTS                       
*                                                                               
NPCPRDLP DS    0H                                                               
*                                                                               
         CLI   3(R1),0             CHECK FOR END OF LIST                        
         BE    NPCPRDDN                                                         
*                                                                               
         CLC   MVTPRDNW,0(R1)      MATCH ON PRODUCT ID                          
         BE    NPCPRDFD                                                         
*                                                                               
NPCPRDCN DS    0H                                                               
*                                                                               
         LA    R1,4(R1)            BUMP TO NEXT PRODUCT IN LIST                 
         BCT   R0,NPCPRDLP                                                      
*                                                                               
NPCPRDDN DS    0H                                                               
         DC    H'0'                MUST FIND PRODUCT IN LIST                    
*                                                                               
NPCPRDFD DS    0H                                                               
*                                                                               
         MVC   MVTPCDNW,3(R1)      SET NEW PRODUCT CODE                         
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),AGYSEOLD    SET UTL TO OLD AGENCY                        
*                                                                               
         MVC   KEY,NPCKEY          RESTORE CURRENT KEY                          
*                                                                               
         GOTO1 HIGH                RESET FILE POINTERS                          
*                                                                               
NEWPCDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0D                  ALIGNMENT                                    
         DC    CL8'*NPCKEY*'       DUMP TAG                                     
NPCKEY   DS    XL(L'KEY)           MASTER KEY                                   
NPCCHDR  DS    XL4096              IOAREA FOR CLIENT HEADER                     
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - MVTBBLD'                    
***********************************************************************         
*                                                                     *         
*        BUILD DATA TO MOVE TABLE                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MVTBBLD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9       ESTABLISH SPONSOR WORKING STORAGE            
         USING WORKD,RC            ESTABLISH LOCAL WORKING STORAGE              
*                                                                               
*        FILL IN MOVE TABLE                                                     
*                                                                               
         OPEN  (CARD,INPUT)         OPEN REQUEST CARD INPUT FILE                
*                                                                               
         L     R4,=A(MVTAB)        POINT TO SELECTION TABLE                     
         USING MVTABD,R4           ESTABLISH TABLE                              
*                                                                               
         XC    MVTENT(MVTENTL),MVTENT INIT TABLE ENTRY                          
*                                                                               
*        SET UP COLUMN HEADINGS                                                 
*                                                                               
         MVI   LINE,100            FORCE NEW PAGE                               
*                                                                               
         LA    R5,MID1             ESTABLISH PRINT LINE                         
         USING PTBLINED,R5                                                      
         MVC   MID1,SPACES                                                      
*                                                                               
*        SET HEADLINES                                                          
*                                                                               
         MVC   PTBAMOLD,=C'OL'                                                  
         MVC   PTBCLT,=C'OLD'                                                   
         MVC   PTBCLTPK,=C'OLD '                                                
         MVC   PTBPRD,=C'OLD'                                                   
         MVC   PTBPCD,=C'OLD'                                                   
         MVC   PTBESTST,=C'OL'                                                  
         MVC   PTBESTEN,=C'OLD'                                                 
         MVC   PTBAMNEW,=C'NW'                                                  
         MVC   PTBCLTNW,=C'NEW'                                                 
         MVC   PTBCLPKN,=C'NEW '                                                
         MVC   PTBPRDNW,=C'NEW'                                                 
         MVC   PTBPCDNW,=C'NW'                                                  
         MVC   PTBESTNW,=C'NEW'                                                 
         MVC   PTBMKT,=C'NEW '                                                  
*                                                                               
         LA    R5,MID2             ESTABLISH PRINT LINE                         
         USING PTBLINED,R5                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
*        SET HEADLINES                                                          
*                                                                               
         MVC   PTBAMOLD,=C'AM'                                                  
         MVC   PTBCLT,=C'CLT'                                                   
         MVC   PTBCLTPK,=C'CLPK'                                                
         MVC   PTBPRD,=C'PRD'                                                   
         MVC   PTBPCD,=C'PC'                                                    
         MVC   PTBESTST,=C'EST'                                                 
         MVC   PTBESTEN,=C'EST'                                                 
         MVC   PTBAMNEW,=C'AM'                                                  
         MVC   PTBCLTNW,=C'CLT'                                                 
         MVC   PTBCLPKN,=C'CLPK'                                                
         MVC   PTBPRDNW,=C'PRD'                                                 
         MVC   PTBPCDNW,=C'PC'                                                  
         MVC   PTBESTNW,=C'EST'                                                 
         MVC   PTBMKT,=C'MKT '                                                  
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
         ZAP   MTBCTR,=P'0'        INIT ITEMS COUNTER                           
*                                                                               
MTBINILP DS    0H                                                               
*                                                                               
         L     R1,=A(CARD)                                                      
         GET   (1),QAREA           READ NEXT REQUEST CARD                       
*                                                                               
         MVC   MVTCHOLD,AGYCHOLD   SAVE OLD AGENCY CODE                         
         MVC   MVTCHNEW,AGYCHNEW   SAVE NEW AGENCY CODE                         
*                                                                               
         MVC   MVTMDOLD,QMED       SAVE MEDIA                                   
         MVC   MVTMDNEW,QMED       MEDIA CAN'T CHAMNGE                          
*                                                                               
         MVI   MVTAMOLD,X'01'      ASSUME MEDIA TV                              
*                                                                               
         CLI   QMED,C'R'           IF MEDIA IS RADIO                            
         BNE   *+8                                                              
         MVI   MVTAMOLD,X'02'         RESET AGYMEDIA CODE                       
*                                                                               
         CLI   QMED,C'N'           IF MEDIA IS NETWORK                          
         BNE   *+8                                                              
         MVI   MVTAMOLD,X'03'         RESET AGYMEDIA CODE                       
*                                                                               
         MVC   MVTAMNEW,MVTAMOLD   INIT NEW AGENCY/MEDIA                        
*                                                                               
         OC    MVTAMOLD,AGYAGOLD   FILL IN AGENCY NYBBLE                        
         OC    MVTAMNEW,AGYAGNEW   FILL IN AGENCY NYBBLE                        
*                                                                               
         MVC   MVTCLOLD,QCLT       GET CLIENT CODE                              
*                                                                               
         GOTO1 CLPACK,DMCB,MVTCLOLD,MVTCLTPK  PACK CLIENT CODE                  
*                                                                               
         MVC   MVTCLTNW,MVTCLOLD   INIT NEW CLIENT                              
         MVC   MVTCLPKN,MVTCLTPK                                                
*                                                                               
         CLC   QNWCLT,SPACES       DONE IF NO NEW CLIENT                        
         BNH   MTBINI05                                                         
*                                                                               
         MVC   MVTCLTNW,QNWCLT     SAVE NEW CLIENT                              
*                                                                               
         GOTO1 CLPACK,DMCB,MVTCLTNW,MVTCLPKN    PACK CLIENT CODE                
*                                                                               
MTBINI05 DS    0H                                                               
*                                                                               
         CLC   =C'ALL',QPRD        OKAY IF ALL PRODUCTS                         
         BE    MTBINI06                                                         
*                                                                               
         CLC   QPRD,SPACES         OKAY IF ALL PRODUCTS                         
         BNH   MTBINI06                                                         
*                                                                               
         CLC   =C'POL',QPRD        OKAY IF PRODUCT POL                          
         BE    MTBINI06                                                         
*                                                                               
         MVC   MVTPRD,QPRD            ELSE SAVE PRODUCT                         
*                                                                               
         CLC   QNWPRD,SPACES       IF PRODUCT BEING CHANGED                     
         BNH   MTBINI06                                                         
*                                                                               
         MVC   MVTPRDNW,QNWPRD        SAVE NEW PRODUCT IN TABLE                 
*                                                                               
         CLC   QPRD2,SPACES        SKIP IF SPACES                               
         BNH   MTBINI06                                                         
*                                                                               
         PACK  DUB,QPRD2           ELSE QPRD2 HAS NEW PRODUCT CODE              
         CVB   RF,DUB                                                           
         STC   RF,MVTPCDNW            SAVE IT                                   
*                                                                               
MTBINI06 DS    0H                                                               
*                                                                               
         CLC   QPRD3,SPACES        SKIP IF SPACES                               
         BNH   MTBINI07                                                         
*                                                                               
         PACK  DUB,QPRD3           ELSE QPRD3 HAS INCREMENT FOR LINE #          
         CVB   RF,DUB                                                           
         STC   RF,MVTLN#NW            SAVE IT                                   
*                                                                               
MTBINI07 DS    0H                                                               
*                                                                               
         MVI   MVTESTST,1          ASSUME ALL ESTIMATES WANTED                  
         MVI   MVTESTEN,255                                                     
*                                                                               
         CLC   QEST,=C'   '        SKIP IF ALL ESTIMATES                        
         BE    MTBINI10                                                         
         CLC   QEST,=C'ALL'        SKIP IF ALL ESTIMATES                        
         BE    MTBINI10                                                         
*                                                                               
         PACK  DUB,QEST            PACK ESTIMATE NUMBER                         
         CVB   RF,DUB              CVB                                          
         STC   RF,MVTESTST                                                      
*                                                                               
         MVC   MVTESTEN,MVTESTST   INIT END ESTIMATE                            
*                                                                               
         CLC   QESTEND,=C'   '     IF NOT A RANGE                               
         BH    MTBINI08                                                         
*                                                                               
         CLC   QNWEST,=C'   '      SKIP IF NOT NEW ESTIMATE                     
         BNH   MTBINI10                                                         
*                                  ELSE PACK NEW ESTIMATE NUMBER                
         PACK  DUB,QNWEST                                                       
         CVB   RF,DUB                                                           
         STC   RF,MVTESTNW         SAVE IN TABLE                                
*                                                                               
         B     MTBINI10                                                         
*                                                                               
MTBINI08 DS    0H                                                               
*                                                                               
         PACK  DUB,QESTEND         PACK ESTIMATE NUMBER                         
         CVB   RF,DUB              CVB                                          
         STC   RF,MVTESTEN                                                      
*                                                                               
MTBINI10 DS    0H                                                               
*                                                                               
         CLC   QSTART,SPACES                                                    
         BE    MTBINI12                                                         
*                                                                               
         GOTO1 DATCON,DMCB,QSTART,(2,MVTSTDTE)                                  
*                                                                               
MTBINI12 DS    0H                                                               
*                                                                               
         CLC   QEND,SPACES                                                      
         BE    MTBINI14                                                         
*                                                                               
         GOTO1 DATCON,DMCB,QEND,(2,MVTNDDTE)                                    
*                                                                               
MTBINI14 DS    0H                                                               
*                                                                               
         CLC   QMKT,SPACES         IF MARKET PRESENT                            
         BNH   *+18                                                             
         PACK  DUB,QMKT               PACK                                      
         CVB   RF,DUB                                                           
         STCM  RF,3,MVTMKT            SAVE                                      
*                                                                               
*        TEST FOR OVERRIDES TO MOVING HEADERS                                   
*                                                                               
         CLI   QOPT4,C' '          SKIP IF NO OVERRIDE                          
         BNH   MTBIOVRX                                                         
*                                                                               
         CLI   QOPT4,C'C'          TEST FOR CLIENT  RECORD SKIPPED              
         BE    MTBIOVCL                                                         
         CLI   QOPT4,C'P'                                                       
         BE    MTBIOVCL                                                         
         CLI   QOPT4,C'E'                                                       
         BNE   *+8                                                              
MTBIOVCL MVI   MVTCLTMV,C'Y'                                                    
*                                                                               
         CLI   QOPT4,C'P'          TEST FOR PRODUCT RECORD SKIPPED              
         BE    MTBIOVPR                                                         
         CLI   QOPT4,C'E'                                                       
         BNE   *+8                                                              
MTBIOVPR MVI   MVTPRDMV,C'Y'                                                    
*                                                                               
         CLI   QOPT4,C'E'          TEST FOR ESTIMATE RECORD SKIPPED             
         BNE   *+8                                                              
MTBIOVES MVI   MVTESTMV,C'Y'                                                    
*                                                                               
*        TEST FOR OVERRIDES FOR MOVING BUYS AND GOALS                           
*                                                                               
         CLI   QOPT1,C'N'         IF MOVING BUYS                                
         BNE   *+16                                                             
         MVI   MVTBUYS,C'N'                                                     
         MVI   MVTDEMOS,C'N'                                                    
         MVI   MVTCOSTS,C'N'                                                    
*                                                                               
         CLI   QOPT1,C'D'         IF MOVING BUYS WITHOUT DEMOS                  
         BNE   *+16                                                             
         MVI   MVTBUYS,C'Y'                                                     
         MVI   MVTDEMOS,C'N'                                                    
         MVI   MVTCOSTS,C'Y'                                                    
*                                                                               
         CLI   QOPT1,C'D'         IF MOVING BUYS WITHOUT DEMOS                  
         BNE   *+16                   OR COSTS                                  
         MVI   MVTBUYS,C'Y'                                                     
         MVI   MVTDEMOS,C'N'                                                    
         MVI   MVTCOSTS,C'N'                                                    
*                                                                               
         CLI   QOPT2,C'N'         IF MOVING GOALS                               
         BNE   *+8                                                              
         MVI   MVTGOALS,C'N'                                                    
*                                                                               
MTBIOVRX DS    0H                                                               
*                                                                               
         LA    R5,P                ESTABLISH PRINT LINE                         
         USING PTBLINED,R5                                                      
*                                                                               
*        PRINT TABLE ENTRY                                                      
*                                                                               
         GOTO1 HEXOUT,DMCB,MVTAMOLD,PTBAMOLD,1,0,0 AGENCY/MEDIA - OLD           
*                                                                               
         GOTO1 HEXOUT,DMCB,MVTAMNEW,PTBAMNEW,1,0,0 AGENCY/MEDIA - NEW           
*                                                                               
         MVC   PTBCLT,MVTCLOLD      CLIENT                                      
*                                                                               
         GOTO1 HEXOUT,DMCB,MVTCLTPK,PTBCLTPK,2,0,0   PACKED CLIENT              
*                                                                               
         MVC   PTBCLTNW,MVTCLTNW    NEW CLIENT                                  
*                                                                               
         GOTO1 HEXOUT,DMCB,MVTCLPKN,PTBCLPKN,2,0,0   PACKED CLIENT              
*                                                                               
         MVC   PTBPRD,MVTPRD       PRODUCT                                      
         OC    PTBPRD,=C'   '      MAKE PRINTABLE                               
*                                                                               
         GOTO1 HEXOUT,DMCB,MVTPCD,PTBPCD,1,0,0 PRODUCT CODE - OLD               
*                                                                               
         MVC   PTBPRDNW,MVTPRDNW   NEW PRODUCT                                  
         OC    PTBPRD,=C'   '      MAKE PRINTABLE                               
*                                                                               
         GOTO1 HEXOUT,DMCB,MVTPCDNW,PTBPCDNW,1,0,0 PRODUCT CODE - NEW           
*                                                                               
         EDIT  (B1,MVTESTST),(3,PTBESTST)   START ESTIMATE                      
         EDIT  (B1,MVTESTEN),(3,PTBESTEN)   END   ESTIMATE                      
*                                                                               
         EDIT  (B1,MVTESTNW),(3,PTBESTNW)   NEW   ESTIMATE                      
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
MTBINICN DS    0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      BUMP TO NEXT TABLE ENTRY                     
         XC    MVTENT(MVTENTL),MVTENT INIT TABLE ENTRY                          
*                                                                               
         AP    MTBCTR,=P'1'        BUMP ITEM COUNTER                            
*                                                                               
         B     MTBINILP                                                         
*                                                                               
MTBINIDN DS    0H                                                               
*                                                                               
         MVI   MVTENT,X'FF'        INDICATE END OF TABLE                        
*                                                                               
         L     R1,=A(CARD)                                                      
         CLOSE ((1))               CLOSE REQUEST FILE                           
*                                                                               
*        SORT TABLE IN AGY/CLT/PRD/EST ORDER                                    
*                                                                               
         CVB   RF,MTBCTR           CVB NUMBER OF ITEMS                          
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+11,MVTENTL     KEY AND RECORD LENGTHS                       
         MVI   DMCB+15,MVTENTL                                                  
*                                                                               
         GOTO1 XSORT,DMCB,MVTAB,(RF)                                            
*                                                                               
MVTBBLDX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DS    0D                  ALIGNMENT                                    
MTBCTR   DS    PL8                 ITEMS IN TABLE COUNTER                       
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - STABBLD'                    
***********************************************************************         
*                                                                     *         
*        BUILD STATION TABLE                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STABBLD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9       ESTABLISH SPONSOR WORKING STORAGE            
         USING WORKD,RC            ESTABLISH LOCAL WORKING STORAGE              
*                                                                               
*        ISSUE GETMAIN FOR STORAGE TO HOLD STATION CONVERSION TABLE             
*                                                                               
         LHI   R0,STANTRYL         RECORD LENGTH                                
         MHI   R0,STTRMAXQ         *MAXIMUM NUMBER OF RECORDS                   
*                                                                               
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STCM  R1,15,STATABA       SAVE A(GETMAIN AREA)                         
         LR    R3,R1               SAVE A(GETMAIN AREA)                         
*                                                                               
*        INITIALIZE BINSRCH PARAMETERS                                          
*                                                                               
         LA    R2,BSPSTA           POINT TO STATION BINSRCH PARAMETERS          
         USING BSRPRMD,R2          ESTABLISH BINSRCH PARAMETERES                
*                                                                               
         MVC   BSPATAB,STATABA     A(TABLE)                                     
*                                                                               
         LA    RF,STANTRYL         SET ENTRY LENGTH                             
         ST    RF,BSPLENR                                                       
*                                                                               
         XC    BSPNOR,BSPNOR       INIT RECORD COUNTER                          
*                                                                               
         LA    RF,STTKEYLQ         SET KEY LENGTH                               
         ST    RF,BSPLENK                                                       
         MVI   BSPKEYD,STTKEY-STANTRY KEY DISPLACEMENT                          
*                                                                               
         LHI   RF,STTRMAXQ         SET # OF AVAILABLE ENTRIES                   
         ST    RF,BSPMAX                                                        
*                                                                               
*        FILL CONVERSION TABLE                                                  
*                                                                               
         MVI   LINE,100            FORCE NEW PAGE                               
*                                                                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         MVC   MID1+13(15),=CL12'OLD AGENCY DATA'                               
         MVC   MID1+47(15),=CL12'NEW AGENCY DATA'                               
*                                                                               
         LA    R1,MID2                                                          
         USING PLINED,R1           ESTABLISH PRINT LINE                         
*                                                                               
         MVC   POLDAGY,=C'AG'                                                   
         MVC   POLDAM,=C'AM'                                                    
         MVC   POLDMSTA,=CL10' MKT/STA'                                         
         MVC   POLDMK,=CL04'MKT '                                               
         MVC   POLDSTA,=CL05'STA '                                              
         MVC   POLDCLT,=CL03'CLT'                                               
         MVC   PNEWAGY,=C'AG'                                                   
         MVC   PNEWAM,=C'AM'                                                    
         MVC   PNEWMSTA,=CL10' MKT/STA'                                         
         MVC   PNEWMK,=CL04'MKT '                                               
         MVC   PNEWSTA,=CL05'STA '                                              
         MVC   PNEWCLT,=CL03'CLT'                                               
*                                                                               
         MVC   PLINED+90(18),=C'TABLE ENTRY IN HEX'                             
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
         DROP  R1                                                               
*                                                                               
         LA    R7,STANTRYC         ESTABLISH STATION TABLE BUILD AREA           
         USING STANTRYD,R7                                                      
*                                                                               
*        BUILD OLD AGENCY STARTING STATION KEY                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING STARECD,R5                                                       
*                                                                               
         MVI   STAKTYPE,C'S'       STANDARD KEY                                 
*                                                                               
         MVC   STAKOLD,KEY         SAVE AS OLD STARTING KEY                     
         MVC   STAKNEW,KEY         SAVE AS NEW STARTING KEY                     
*                                                                               
         GOTO1 HIGHSTA             READ FIRST OLD STATION RECORD.               
*                                                                               
         GOTO1 HIGHSTA             TO RESET POINTERS ON FIRST TIME              
*                                                                               
STBLOOP  DS    0H                                                               
*                                                                               
         L     R5,ADSTAT           ADSTAT-->RECORD FOUND.                       
*                                                                               
         CLC   STAKTYPE(STAKMED-STAKEY),STAKOLD DONE ON BREAK IN                
         BNE   STBDONE             STARTING KEY                                 
*                                                                               
         MVC   STAKOLD,STAKEY      UPDATE KEY SAVEAREA                          
*                                                                               
         CLI   STAKMED,C'R'        ONLY INTERESTED IN RADIO                     
         BE    *+16                                                             
         CLI   STAKMED,C'T'        OR TV                                        
         BL    STBCONT                                                          
         BH    STBDONE                                                          
*                                                                               
         CLC   STAKAGY,AGYCHOLD    KEEP IF FOR OLD AGENCY                       
         BNE   STBCONT                                                          
*                                                                               
*****    CLI   STAKCALL,C'0'       SKIP IF LOCAL CABLE                          
*****    BNL   STBCONT                                                          
*                                                                               
*        CHECK IF CLIENT IN LIST TO BE MOVED                                    
*                                                                               
         L     R4,=A(MVTAB)        POINT TO SELECTION TABLE                     
         USING MVTABD,R4                                                        
*                                                                               
STBMVTLP DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    STBMVTDN                                                         
*                                                                               
         CLC   STAKMED,MVTMDOLD    MATCH ON MEDIA                               
         BNE   STBMVTCN                                                         
*                                                                               
         CLC   STAKCLT,=C'000'     KEEP IF MASTER STATION RECORD                
         BE    STBMVTFD                                                         
*                                                                               
         CLC   STAKCLT,MVTCLOLD    MATCH ON CLIENT                              
         BE    STBMVTFD                                                         
*                                                                               
STBMVTCN DS    0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      BUMP TO NEXT TABLE ENTRY                     
         B     STBMVTLP                                                         
*                                                                               
STBMVTDN DS    0H                  NO MATCH IN TABLE                            
         B     STBCONT                                                          
*                                                                               
STBMVTFD DS    0H            MATCH - ADD STATION TO TABLE                       
*                                                                               
*        BUILD CONVERSION FILE RECORD IN BINSRCH AREA                           
*                                                                               
         XC    STANTRY(STANTRYL),STANTRY   INIT TABLE ENTRY                     
*                                                                               
         MVC   STTAMOLD,MVTAMOLD   SET AGENCY/MEDIA                             
         MVC   STTMDOLD,STAKMED    SET MEDIA                                    
         MVC   STTMKOLD,SMKT       MARKET                                       
         MVC   STTSTOLD(5),STAKCALL  OLD STATION                                
         MVC   STTCLOLD,STAKCLT    OLD CLIENT                                   
*                                                                               
*        PACK MARKET/STATION                                                    
*                                                                               
         XC    STAWORK,STAWORK     INIT STAPACK WORKAREA                        
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'        ACTION IS PACK                               
         MVC   STAPAGY,MVTCHOLD    AGENCY                                       
         MVC   STAPCTRY,COUNTRY    COUNTRY                                      
         MVC   STAPMED,STAKMED     MEDIA                                        
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,SMKT       MARKET                                       
         MVC   STAPQSTA(5),STAKCALL  STATION                                    
*                                                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                SHOULDN'T HAPPEN                             
*                                                                               
         MVC   STTMSOLD,STAPMKST   MARKET/STATION PACKED                        
*                                                                               
         CLC   STAKCLT,=C'000'     IF MASTER STATION                            
         BNE   *+14                                                             
         MVC   STTCPOLD,=X'FFFF'      SET HIGH VALUES                           
         B     STBLP10                                                          
*                                                                               
         GOTO1 CLPACK,DMCB,STAKCLT,STTCPOLD  CLIENT PACKED                      
*                                                                               
STBLP10  DS    0H                                                               
*                                                                               
         MVC   STTAMNEW,MVTAMNEW   SET NEW AGENCY/MEDIA                         
*                                                                               
*        FIND STATION ON NEW AGENCY FILES                                       
*                                                                               
         MVC   STAKOLD,STAKEY      SAVE CURRENT KEY                             
*                                                                               
         L     RF,UTL              RESET UTL FOR NEW AGENCY                     
         MVC   4(1,RF),AGYSENEW                                                 
*                                                                               
         LA    R5,KEY              BUILD STATION KEY FOR NEW AGENCY             
*                                                                               
         MVC   STAKEY,=32C'0'      INIT NEW AGENCY STATION KEY                  
         MVI   STAKTYPE,C'S'       STANDARD KEY                                 
*                                                                               
         MVC   STAKMED,STTMDOLD    MEDIA STAYS THE SAME                         
         MVC   STAKCALL,STTSTOLD   CALL LETTERS REMAIN THE SAME                 
         MVC   STAKAGY,AGYCHNEW    NEW AGENCY                                   
*                                                                               
         CLC   STTCLOLD,=C'000'    IF NOT A MASTER STATION RECORD               
         BE    *+10                                                             
         MVC   STAKCLT,MVTCLTNW       SET NEW CLIENT CODE                       
*                                                                               
         MVC   STAKNEW,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTO1 HIGHSTA             READ NEW AGENCY STATION RECORD               
*                                                                               
         NOP   STBLP15                                                          
         OI    *-3,X'F0'           ONLY DO ONCE                                 
         GOTO1 HIGHSTA             RESET FILE POINTERS                          
*                                                                               
STBLP15  DS    0H                                                               
*                                                                               
         L     R5,ADSTAT           POINT TO FOUND KEY                           
*                                                                               
         CLC   STAKEY,STAKNEW      OKAY IF RECORD FOUND                         
         BE    STBLP16                                                          
*                                                                               
         LA    R5,KEY              RESTORE DSECT POINTER                        
*                                                                               
         MVC   STAKEY,STAKNEW      RESTORE ORIGINAL KEY                         
         MVC   STAKCLT,=C'000'     SET TO FIND MASTER RECORD                    
         MVC   STAKNEW,STAKEY                                                   
*                                                                               
         GOTO1 HIGHSTA             READ NEW AGENCY MASTER STATION REC           
*                                                                               
         L     R5,ADSTAT           POINT TO FOUND KEY                           
*                                                                               
         CLC   STAKEY,STAKNEW      SKIP IF RECORD NOT FOUND                     
         BNE   STBLP20                                                          
*                                                                               
STBLP16  DS    0H                                                               
*                                                                               
*        FILL NEW AGENCY STATION DATA                                           
*                                                                               
         MVC   STTAMNEW,MVTAMNEW   NEW AGENCY MEDIA                             
         MVC   STTMKNEW,SMKT       NEW MARKET                                   
         MVC   STTSTNEW,STTSTOLD   NEW STATION IS OLD STATION                   
*                                                                               
*        PACK MARKET/STATION                                                    
*                                                                               
         XC    STAWORK,STAWORK     INIT STAPACK WORKAREA                        
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'        ACTION IS PACK                               
         MVC   STAPAGY,MVTCHNEW    AGENCY                                       
         MVC   STAPCTRY,COUNTRY    COUNTRY                                      
         MVC   STAPMED,STAKMED     MEDIA                                        
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,SMKT       MARKET                                       
         MVC   STAPQSTA(5),STAKCALL  STATION                                    
*                                                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                SHOULDN'T HAPPEN                             
*                                                                               
         MVC   STTMSNEW,STAPMKST   MARKET/STATION PACKED                        
*                                                                               
STBLP20  DS    0H                                                               
*                                                                               
         L     RF,UTL              RESET UTL FOR OLD AGENCY                     
         MVC   4(1,RF),AGYSEOLD                                                 
*                                                                               
         MVC   KEY,STAKOLD         RESTORE OLD AGENCY FILE POINTERS             
*                                                                               
         GOTO1 HIGHSTA             RE-POINT TO CURRENT STATION                  
*                                                                               
         L     R5,ADSTAT           POINT TO FOUND RECORD                        
*                                                                               
*        PRINT TABLE ENTRY                                                      
*                                                                               
         LA    R3,P                ESTABLISH PRINT LINE                         
         USING PLINED,R3                                                        
*                                                                               
         GOTO1 HEXOUT,DMCB,STTAMOLD,POLDAM,1,0,0 AGENCY/MEDIA - OLD             
*                                                                               
         GOTO1 HEXOUT,DMCB,STTAMNEW,PNEWAM,1,0,0 AGENCY/MEDIA - NEW             
*                                                                               
         GOTO1 HEXOUT,DMCB,STTMSOLD,POLDMSTA,5,0,0   MKT/STA - OLD              
*                                                                               
         MVC   POLDMK,STTMKOLD     OLD MARKET NUMBER                            
*                                                                               
         MVC   POLDSTA,STTSTOLD    OLD STATION                                  
         MVC   POLDCLT,STTCLOLD    OLD CLIENT                                   
*                                                                               
         MVC   POLDAGY,MVTCHOLD    OLD AGENCY                                   
*                                                                               
         OC    STTMKNEW,STTMKNEW   IF STATION NOT ON NEW AGENCY                 
         BNZ   *+14                                                             
         MVC   PNEWMK(15),=C'UNKNOWN STATION'                                   
         B     STBLP29                                                          
*                                                                               
         GOTO1 HEXOUT,DMCB,STTMSNEW,PNEWMSTA,5,0,0   MKT/STA - NEW              
*                                                                               
         MVC   PNEWMK,STTMKNEW     NEW MARKET NUMBER                            
*                                                                               
         MVC   PNEWSTA,STTSTOLD    NEW STATION                                  
*                                                                               
         MVC   PNEWAGY,MVTCHNEW                                                 
*                                                                               
*        ADD ENTRY TO TABLE                                                     
*                                                                               
STBLP29  DS    0H                                                               
*                                                                               
         GOTO1 BINSRCH,BSPPRMS,('BSPADD',STANTRY) ADD REC TO TABLE              
*                                                                               
         OC    BSPAREC,BSPAREC     DIE IF TABLE FILLED                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   RF,15,BSPAREC                                                    
*                                                                               
         MVC   P+90(42),0(RF)      DISPLAY TABLE ENTRY                          
*                                                                               
STBLP30  DS    0H                                                               
*                                                                               
         CLI   MVERROPT,C'E'       IF ONLY PRINTING ERRORS                      
         BNE   STBLP32                                                          
*                                                                               
         CLC   =C'UNKNOWN ',PNEWMK    MAKE SURE WE HAVE AN ERROR                
         BNE   STBLP31                                                          
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
STBLP31  DS    0H                                                               
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
         B     STBCONT                                                          
*                                                                               
STBLP32  DS    0H                                                               
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
STBCONT  DS    0H                                                               
*                                                                               
         MVC   KEYSAVE,STAKOLD     UPDATE KEY SAVEAREA FOR TRACING              
         MVC   KEY,STAKOLD         UPDATE KEY SAVEAREA FOR TRACING              
*                                                                               
         GOTO1 SEQSTA              READ NEXT STATION ON FILE                    
         B     STBLOOP                                                          
*                                                                               
STBDONE  DS    0H                                                               
*                                                                               
STABBLDX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - HDR'                        
***********************************************************************         
*                                                                     *         
*        HEADER RECORDS                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HDR      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9       ESTABLISH SPONSOR WORKING STORAGE            
         USING WORKD,RC            ESTABLISH LOCAL WORKING STORAGE              
*                                                                               
         NOP   HDR1STX             PRINT HEADERS FIRST TIME                     
         OI    *-3,X'F0'                                                        
         MVI   LINE,100            FORCE NEW PAGE                               
         MVC   P+3(7),=C'OLD KEY'                                               
         MVC   P+33(7),=C'NEW KEY'                                              
         GOTO1 REPORT              PRINT HEADERS                                
*                                                                               
         MVC   P(132),SPACES                                                    
         GOTO1 REPORT              PRINT BLANK LINE                             
*                                                                               
HDR1STX  DS    0H                                                               
*                                                                               
         L     R4,=A(MVTAB)        POINT TO SELECTION TABLE                     
         USING MVTABD,R4                                                        
*                                                                               
         USING ESTHDRD,R3          ESTABLISH AS ESTIMATE HEADER RECORD          
*                                                                               
         OC    EKEYEST+1(5),EKEYEST+1 SKIP IF NOT ESTIMATE(IE.BILLREC)          
         BNZ   HDRKEYDN                                                         
*                                                                               
HDRKEYLP DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    HDRKEYDN                                                         
*                                                                               
         CLC   EKEYAM,MVTAMOLD     MATCH ON AGENCY/MEDIA                        
         BNE   HDRKEYCN                                                         
*                                                                               
         CLC   EKEYCLT,MVTCLTPK    MATCH ON PACKED CLIENT                       
         BNE   HDRKEYCN                                                         
*                                                                               
         OC    EKEYPRD(9),EKEYPRD  IF CLIENT HEADER                             
         BNZ   HDRCLTN                                                          
*                                                                               
         USING CLTHDRD,R3             ESTABLISH CLIENT HEADER                   
*                                                                               
         LA    R0,4                SAVE CLIST                                   
         LA    R1,SAVCLIST                                                      
         LA    RE,CLIST                                                         
*                                                                               
         MVC   0(220,R1),0(RE)                                                  
         AHI   R1,220                                                           
         AHI   RE,220                                                           
         BCT   R0,*-14                                                          
*                                                                               
         CLI   MVCLT,C'Y'             DROP IF NOT MOVING CLIENT HEADER          
         BE    HDRKEYDN                                                         
*                                                                               
         B     HDRKEYFD               ELSE KEEP CLIENT HEADER                   
*                                                                               
HDRCLTN  DS    0H                                                               
*                                                                               
         USING ESTHDRD,R3          ESTABLISH SPOT GENERIC HEADER RECORD         
*                                                                               
         CLC   MVTPRD,SPACES       SKIP IF ALL PRODUCTS WANTED                  
         BNH   HDRPRD20                                                         
*                                                                               
         CLC   EKEYPRD,=C'POL'     IF NOT PRODUCT POL                           
         BE    HDRPRD20                                                         
*                                                                               
         CLC   EKEYPRD,MVTPRD         MATCH ON PRODUCT                          
         BNE   HDRKEYCN                                                         
*                                                                               
HDRPRD20 DS    0H                                                               
*                                                                               
         OC    EKEYEST(6),EKEYEST  IF PRODUCT HEADER                            
         BNZ   HDRPRDN                                                          
*                                                                               
         CLC   EKEYPRD,=C'POL'     SKIP IF POOL PRODUCT                         
         BE    HDRPRDDN                                                         
*                                                                               
*        FIND INTERNAL PRODUCT CODE                                             
*                                                                               
         LA    RF,SAVCLIST         FIND PRODUCT CODE IN CLIST                   
         LA    R0,L'SAVCLIST/4     MAX ENTRIES IN LIST                          
*                                                                               
HDRPRDLP DS    0H                                                               
*                                                                               
         CLI   3(RF),0             CHECK FOR END OF LIST                        
         BE    HDRPRDDN            OKAY IF NOT IN LIST                          
*                                                                               
         CLC   EKEYPRD,0(RF)       MATCH ON PRODUCT ALPHA                       
         BE    HDRPRDFD                                                         
*                                                                               
HDRPRDCN DS    0H                                                               
*                                                                               
         LA    RF,4(RF)            NEXT PRODUCT                                 
         BCT   R0,HDRPRDLP                                                      
         B     HDRPRDDN            OKAY IF NOT IN PRODUCT LIST                  
*                                                                               
HDRPRDFD DS    0H                                                               
*                                                                               
         MVC   MVTPCD,3(RF)        SAVE PRODUCT CODE                            
*                                                                               
HDRPRDDN DS    0H                                                               
*                                                                               
         CLC   EKEYPRD,=C'POL'     SKIP IF POOL PRODUCT                         
         BE    HDRPRD80                                                         
*                                                                               
         CLC   MVTPRDNW,SPACES     IF PRODUCT CODE CHANGING                     
         BNH   *+8                                                              
         BRAS  RE,NEWPCD              FIND NEW PRODUCT CODE                     
*                                                                               
HDRPRD80 DS    0H                                                               
*                                                                               
         CLC   EKEYPRD,=C'POL'     SKIP IF NOT POL                              
         BNE   *+12                                                             
         CLI   MVTPRDMV,C'Y'       DROP IF THIS PRDHDR TO BE DROPPED            
         BE    HDRKEYDN                                                         
*                                                                               
         CLI   MVPRD,C'Y'          DROP IF NOT MOVING PRODUCT HEADER            
         BE    HDRKEYDN                                                         
*                                                                               
         B     HDRKEYFD               ELSE MOVE PRODUCT HEADER                  
*                                                                               
HDRPRDN  DS    0H                                                               
*                                                                               
         CLC   EKEYEST,MVTESTST    MATCH ON ESTIMATE RANGE                      
         BL    HDRKEYCN                                                         
         CLC   EKEYEST,MVTESTEN                                                 
         BH    HDRKEYCN                                                         
*                                                                               
         CLC   EKEYPRD,=C'POL'     SKIP IF POOL PRODUCT                         
         BE    HDRESTDN                                                         
*                                                                               
*        FIND INTERNAL PRODUCT CODE                                             
*                                                                               
         LA    RF,SAVCLIST         FIND PRODUCT CODE IN CLIST                   
         LA    R0,L'SAVCLIST/4     MAX ENTRIES IN LIST                          
*                                                                               
HDRESTLP DS    0H                                                               
*                                                                               
         CLI   3(RF),0             CHECK FOR END OF LIST                        
         BE    HDRESTDN            OKAY IF NOT IN LIST                          
*                                                                               
         CLC   EKEYPRD,0(RF)       MATCH ON PRODUCT ALPHA                       
         BE    HDRESTFD                                                         
*                                                                               
HDRESTCN DS    0H                                                               
*                                                                               
         LA    RF,4(RF)            NEXT PRODUCT                                 
         BCT   R0,HDRESTLP                                                      
         B     HDRESTDN            OKAY IF NOT IN PRODUCT LIST                  
*                                                                               
HDRESTFD DS    0H                                                               
*                                                                               
         MVC   MVTPCD,3(RF)        SAVE PRODUCT CODE                            
*                                                                               
HDRESTDN DS    0H                                                               
*                                                                               
         CLC   EKEYPRD,=C'POL'     SKIP IF POOL PRODUCT                         
         BE    HDREST80                                                         
*                                                                               
         CLC   MVTPRDNW,SPACES     IF PRODUCT CODE CHANGING                     
         BNH   *+8                                                              
         BRAS  RE,NEWPCD              FIND NEW PRODUCT CODE                     
*                                                                               
HDREST80 DS    0H                                                               
*                                                                               
         CLC   EKEYPRD,=C'POL'     SKIP IF NOT POL                              
         BNE   *+12                                                             
         CLI   MVTESTMV,C'Y'       DROP IF THIS ESTHDR TO BE DROPPED            
         BE    HDRKEYDN                                                         
*                                                                               
         CLI   MVEST,C'Y'          DROP IF NOT MOVING ESTIMATE HEADERS          
         BE    HDRKEYDN                                                         
         B     HDRKEYFD                                                         
*                                                                               
HDRKEYCN DS    0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      POINT TO NEXT ENTRY IN TABLE                 
         B     HDRKEYLP                                                         
*                                                                               
HDRKEYFD DS    0H                                                               
*                                                                               
         BRAS  RE,COPY             COPY RECORD TO WORKAREA                      
*                                                                               
         L     R3,=A(NEWREC)       POINT TO NEW RECORD                          
         USING ESTHDRD,R3          ESTABLISH SPOT GENERIC HEADER RECORD         
*                                                                               
         NI    EKEYAM,X'0F'        KILL   OLD AGENCY NYBBLE                     
         OC    EKEYAM,AGYAGNEW     ADD IN NEW AGENCY NYBBLE                     
*                                                                               
         MVC   EKEYCLT,MVTCLPKN    SET NEW CLIENT                               
*                                                                               
         OC    EKEYPRD,EKEYPRD     IF PRODUCT IN KEY                            
         BZ    HDRKEYF0                                                         
         CLC   EKEYPRD,=C'POL'     AND NOT POOL                                 
         BE    HDRKEYF0                                                         
         CLC   MVTPRDNW,SPACES     AND NEW PRODUCT GIVEN                        
         BNH   HDRKEYF0                                                         
*                                                                               
         MVC   EKEYPRD,MVTPRDNW               CHANGE PRODUCT ID                 
*                                                                               
         OC    EKEYEST,EKEYEST     IF PRODUCT HEADER                            
         BNZ   *+10                                                             
         MVC   PCODE+1-PRDHDRD(1,R3),MVTPCDNW  CHANGE PRODUCT CODE              
*                                                                               
HDRKEYF0 DS    0H                                                               
*                                                                               
         OC    EKEYEST,EKEYEST     IF ESTIMATE NUMBER EXISTS                    
         BZ    HDRKEYF2                                                         
*                                                                               
         CLC   EKEYPRD,=C'POL'     AND NOT POOL                                 
         BE    HDRKEYF1                                                         
*                                                                               
         CLC   MVTPRDNW,SPACES     IF NEW PRODUCT ID                            
         BNH   *+10                                                             
         MVC   EPRDCD+1(1),MVTPCDNW        CHANGE PRODUCT CODE                  
*                                                                               
HDRKEYF1 DS    0H                                                               
*                                                                               
         OC    MVTESTNW,MVTESTNW   IF NEW ESTIMATE NUMBER GIVEN                 
         BZ    *+10                                                             
         MVC   EKEYEST,MVTESTNW       REPLACE ESTIMATE NUMBER                   
*                                                                               
         ZAP   ECURPDN,=P'0'                                                    
*                                                                               
         LHI   R0,26                                                            
         LA    R1,EORD                                                          
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LHI   R0,26                                                            
         LA    R1,EPAID                                                         
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
HDRKEYF2 DS    0H                                                               
*                                                                               
         BRAS  RE,RECWRITE         WRITE RECORD TO OUTPUT DATASET               
*                                                                               
         L     R5,AREC             POINT TO INCOMING RECORD                     
         GOTO1 HEXOUT,DMCB,(R5),P+2,13,0,0   PRINT NEW KEY                      
*                                                                               
         GOTO1 HEXOUT,DMCB,(R3),P+32,13,0,0   PRINT NEW KEY                     
*                                                                               
         MVC   P+70(MVTENTL),0(R4)     PRINT TABLE ENTRY                        
*                                                                               
         GOTO1 REPORT              PRINT KEYS                                   
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
HDRKEYDN DS    0H                                                               
*                                                                               
HDRX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - GOAL'                       
***********************************************************************         
*                                                                     *         
*        GOAL RECORD - X'02'                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GOAL     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9       ESTABLISH SPONSOR WORKING STORAGE            
         USING WORKD,RC            ESTABLISH LOCAL WORKING STORAGE              
*                                                                               
         NOP   GOAL1STX            PRINT HEADERS FIRST TIME                     
         OI    *-3,X'F0'                                                        
         MVI   LINE,100            FORCE NEW PAGE                               
         MVC   P+3(7),=C'OLD KEY'                                               
         MVC   P+33(7),=C'NEW KEY'                                              
         GOTO1 REPORT              PRINT HEADERS                                
*                                                                               
         MVC   P(132),SPACES                                                    
         GOTO1 REPORT              PRINT BLANK LINE                             
         ZAP   CTR,=P'10'          FORCE PRINTING                               
*                                                                               
GOAL1STX DS    0H                                                               
*                                                                               
         CLI   MVGOALS,C'Y'        SKIP IF GOALS NOT WANTED                     
         BNE   GOALX                                                            
*                                                                               
         USING GOALRECD,R3         ESTABLISH SPOT GOAL RECORD                   
*                                                                               
         TM    GKEYAM,X'08'        ONLY MEDIA C, R OT T                         
         BO    *+12                                                             
         TM    GKEYAM,X'03'                                                     
         BNM   GOALX                                                            
*                                                                               
         L     R4,=A(MVTAB)        POINT TO SELECTION TABLE                     
*                                                                               
         USING MVTABD,R4                                                        
*                                                                               
GOALKYLP DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    GOALKYDN                                                         
*                                                                               
         CLC   GKEYAM,MVTAMOLD     MATCH ON AGENCY/MEDIA                        
         BNE   GOALKYCN                                                         
*                                                                               
         CLC   GKEYCLT,MVTCLTPK    MATCH ON PACKED CLIENT                       
         BNE   GOALKYCN                                                         
*                                                                               
         CLC   MVTPRD,SPACES       OKAY IF ALL PRDS WANTED                      
         BNH   GOALKY20                                                         
*                                                                               
         CLI   MVTPCD,0            SKIP IF PRODUCT CODE UNKNOWN                 
         BE    GOALKYCN                                                         
*                                                                               
         CLI   GKEYPRD,X'FF'       KEEP BUYING GUIDELINES                       
         BE    *+14                                                             
         CLC   GKEYPRD,MVTPCD      ELSE MATCH ON PRODUCT CODE                   
         BNE   GOALKYCN                                                         
*                                                                               
GOALKY20 DS    0H                                                               
*                                                                               
         CLC   GKEYEST,MVTESTST    MATCH ON ESTIMATE RANGE                      
         BL    GOALKYCN                                                         
         CLC   GKEYEST,MVTESTEN    MATCH ON ESTIMATE                            
         BH    GOALKYCN                                                         
*                                                                               
         OC    MVTMKT,MVTMKT       IF MARKET FILTER EXISTS                      
         BZ    *+14                                                             
         CLC   GKEYMKT,MVTMKT         MATCH ON MARKET                           
         BNE   GOALKYCN                                                         
*                                                                               
         B     GOALKYFD            ACCEPT GOAL RECORD                           
*                                                                               
GOALKYCN DS    0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      POINT TO NEXT ENTRY IN TABLE                 
         B     GOALKYLP                                                         
*                                                                               
GOALKYFD DS    0H                                                               
*                                                                               
         CLI   MVTGOALS,C'N'      CHECK FOR OVERRIDE                            
         BE    GOALKYDN                                                         
*                                                                               
         BRAS  RE,COPY             COPY RECORD TO WORKAREA                      
*                                                                               
         L     R3,=A(NEWREC)       POINT TO NEW RECORD                          
         USING GOALRECD,R3         ESTABLISH SPOT GOAL RECORD                   
*                                                                               
         MVC   WRKCLT,GKEYCLT      SAVE PACKED CLIENT CODE                      
*                                                                               
         NI    GKEYAM,X'0F'        KILL   OLD AGENCY NYBBLE                     
         OC    GKEYAM,AGYAGNEW     ADD IN NEW AGENCY NYBBLE                     
*                                                                               
         MVC   GKEYCLT,MVTCLPKN    SET NEW CLIENT CODE                          
*                                                                               
         OC    MVTPCDNW,MVTPCDNW   IF NEW PRODUCT CODE GIVEN                    
         BZ    *+10                                                             
         MVC   GKEYPRD,MVTPCDNW       REPLACE PRODUCT CODE                      
*                                                                               
         LA    R2,GKEYMKT          R2 POINTS TO MARKET CODE                     
*                                  R4 POINTS TO CONVERSION TABLE ENTRY          
*                                                                               
         OC    MVTESTNW,MVTESTNW   IF NEW ESTIMATE NUMBER GIVEN                 
         BZ    *+10                                                             
         MVC   GKEYEST,MVTESTNW       REPLACE ESTIMATE NUMBER                   
*                                                                               
         BRAS  RE,GETMKT           FIND NEW MARKET                              
         BNZ   GOALKYDN            SKIP WRITING RECORD ON ERROR                 
*                                                                               
*        DROP ANY GOAL OR LOCKIN ELEMENTS OUTSIDE OF RANGE                      
*                                                                               
         OC    MVTSTDTE,MVTSTDTE   SKIP IF NO DATE RANGE SPECIFIED              
         BNZ   *+14                                                             
         OC    MVTNDDTE,MVTNDDTE                                                
         BZ    GLELMDN                                                          
*                                                                               
         LA    R6,GDELEM           POINT TO FIRST ELEMENT                       
*                                                                               
GLELMLP DS     0H                                                               
*                                                                               
         USING GLEMENT,R6          ESTABLISH AS GOAL WEEK ELEMENT               
*                                                                               
         CLI   GLCODE,0            DONE AT RECORD END                           
         BE    GLELMDN                                                          
*                                                                               
         CLI   GLCODE,X'21'        LOOK FOR GOAL ELEMENTS                       
         BE    GLELMFD                                                          
*                                                                               
         CLI   GLCODE,X'30'        LOOK FOR LOCKIN ELEMENTS                     
         BL    GLELMCN                                                          
         CLI   GLCODE,X'60'                                                     
         BH    GLELMCN                                                          
*                                                                               
         MVI   PRTFLAG,C'L'        LOCKIN ELEMENTS                              
         BRAS  RE,PRTDEL                                                        
         B     GLELMLP                                                          
*                                                                               
GLELMFD  DS    0H                                                               
*                                                                               
*        BOTH TYPES OF ELEMENT HAVE DATES IN SAME POSITION                      
*                                                                               
         CLC   GLWEEK,MVTSTDTE                                                  
         BNL   GLELML2             ACCEPT SPOT                                  
         MVI   PRTFLAG,C'S'                                                     
         BRAS  RE,PRTDEL                                                        
         B     GLELMLP                                                          
*                                                                               
GLELML2  OC    MVTNDDTE,MVTNDDTE   ANY END DATE                                 
         BZ    GLELML4                                                          
         CLC   GLWEEK,MVTNDDTE                                                  
         BNH   GLELML4                                                          
         MVI   PRTFLAG,C'E'                                                     
         BRAS  RE,PRTDEL                                                        
         B     GLELMLP                                                          
*                                                                               
GLELML4  DS    0H                  ELEMENT ACCEPTED                             
*                                                                               
GLELMCN DS     0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,GLEN             GET ELEMENT LENGTH                           
         LA    R6,GLEMENT(RF)      BUMP TO NEXT ELEMENT                         
         B     GLELMLP                                                          
*                                                                               
GLELMDN DS     0H                                                               
*                                                                               
         NOP   GLLOOP3                                                          
         OI    *-3,X'F0'                                                        
         MVC   P(10),=C'WRITE GOAL'                                             
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
GLLOOP3  DS    0H                                                               
         BRAS  RE,RECWRITE         WRITE RECORD TO OUTPUT DATASET               
*                                                                               
GOALKYDN DS    0H                                                               
*                                                                               
GOALX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - GRP'                        
**********************************************************************          
*                                                                     *         
*        CLIENT GROUP RECORD - X'0D04'                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GRP      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9       ESTABLISH SPONSOR WORKING STORAGE            
         USING WORKD,RC            ESTABLISH LOCAL WORKING STORAGE              
*                                                                               
         CLI   MVCGRP,C'Y'         SKIP IF CLIENT GROUPS NOT WANTED             
         BNE   GRPX                                                             
*                                                                               
         NOP   GRP1STX             PRINT HEADERS FIRST TIME                     
         OI    *-3,X'F0'                                                        
         MVI   LINE,100            FORCE NEW PAGE                               
         MVC   P+3(7),=C'OLD GRP'                                               
         MVC   P+33(7),=C'NEW GRP'                                              
         GOTO1 REPORT              PRINT HEADERS                                
*                                                                               
         MVC   P(132),SPACES                                                    
         GOTO1 REPORT              PRINT BLANK LINE                             
         ZAP   CTR,=P'10'          FORCE PRINTING                               
*                                                                               
GRP1STX DS    0H                                                                
*                                                                               
         USING GRPRECD,R3          ESTABLISH SPOT GRP RECORD                    
*                                                                               
         TM    GRPKAGMD,X'08'        ONLY MEDIA C, R OT T                       
         BO    *+12                                                             
         TM    GRPKAGMD,X'03'                                                   
         BNM   GRPX                                                             
*                                                                               
         L     R4,=A(MVTAB)        POINT TO SELECTION TABLE                     
*                                                                               
         USING MVTABD,R4                                                        
*                                                                               
GRPKYLP DS     0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    GRPKYDN                                                          
*                                                                               
         CLC   GRPKAGMD,MVTAMOLD     MATCH ON AGENCY/MEDIA                      
         BE    GRPKYFD             ACCEPT GRP RECORD                            
*                                                                               
GRPKYCN DS     0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      POINT TO NEXT ENTRY IN TABLE                 
         B     GRPKYLP                                                          
*                                                                               
GRPKYFD DS     0H                                                               
*                                                                               
         BRAS  RE,COPY             COPY RECORD TO WORKAREA                      
*                                                                               
         L     R3,=A(NEWREC)       POINT TO NEW RECORD                          
         USING GRPRECD,R3          ESTABLISH SPOT GRP RECORD                    
*                                                                               
         NI    GRPKAGMD,X'0F'        KILL   OLD AGENCY NYBBLE                   
         OC    GRPKAGMD,AGYAGNEW     ADD IN NEW AGENCY NYBBLE                   
*                                                                               
         BRAS  RE,RECWRITE         WRITE RECORD TO OUTPUT DATASET               
*                                                                               
GRPKYDN DS     0H                                                               
*                                                                               
GRPX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - BUY'                        
***********************************************************************         
*                                                                     *         
*        BUY RECORD - >X'10'                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BUY      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9       ESTABLISH SPONSOR WORKING STORAGE            
         USING WORKD,RC            ESTABLISH LOCAL WORKING STORAGE              
*                                                                               
         CLI   MVBUYS,C'Y'         SKIP IF BUYS NOT WANTED                      
         BE    *+12                                                             
         CLI   MVCABLE,C'Y'        AND LOCAL CABLE NOT WANTED                   
         BNE   BUYX                                                             
*                                                                               
         NOP   BUY1STX             PRINT HEADERS FIRST TIME                     
         OI    *-3,X'F0'                                                        
         MVI   LINE,100            FORCE NEW PAGE                               
         MVC   P+3(7),=C'OLD KEY'                                               
         MVC   P+33(7),=C'NEW KEY'                                              
         GOTO1 REPORT              PRINT HEADERS                                
*                                                                               
         MVC   P(132),SPACES                                                    
         GOTO1 REPORT              PRINT BLANK LINE                             
         ZAP   CTR,=P'10'          FORCE RECORD PRINTING                        
*                                                                               
BUY1STX  DS    0H                                                               
*                                                                               
         USING BUYRECD,R3          ESTABLISH SPOT BUY RECORD                    
*                                                                               
*****    TM    BUYKAM,X'03'        ACCEPT ONLY RADIO OR TV                      
*****    BNM   BUYX                                                             
*                                                                               
         TM    BUYKAM,X'08'        ACCEPT ONLY RADIO OR TV                      
         BO    BUYX                                                             
*                                                                               
         CLI   BUYMSTA+2,X'E8'     SKIP IF NOT LOCAL CABLE BUY                  
         BL    BUYCBLN                                                          
*                                                                               
         CLI   MVCABLE,C'Y'           DROP IF NOT MOVING CABLE BUYS             
         BNE   BUYX                                                             
*                                                                               
         B     BUYMVX                                                           
*                                                                               
BUYCBLN  DS    0H                                                               
*                                                                               
         CLI   MVBUYS,C'Y'         SKIP IF NOT MOVING BUYS                      
         BNE   BUYX                                                             
*                                                                               
BUYMVX   DS    0H                                                               
*                                                                               
         L     R4,=A(MVTAB)        POINT TO SELECTION TABLE                     
         USING MVTABD,R4                                                        
*                                                                               
BUYKEYLP DS    0H                                                               
*                                                                               
         BC    0,BUYLOOP1                                                       
         OI    *-3,X'F0'                                                        
         MVC   P(9),=C'GOT A BUY '                                              
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(132),SPACES                                                    
         ZAP   PRTCOUNT,=P'0'      RESET PRINTING COUNTER                       
*                                                                               
BUYLOOP1 DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    BUYKEYDN                                                         
*                                                                               
         CLC   BUYKAM,MVTAMOLD     MATCH ON AGENCY/MEDIA                        
         BNE   BUYKEYCN                                                         
*                                                                               
         CLC   BUYKCLT,MVTCLTPK    MATCH ON PACKED CLIENT                       
         BNE   BUYKEYCN                                                         
*                                                                               
         CLC   BUYKEST,MVTESTST    MATCH ON ESTIMATE RANGE                      
         BL    BUYKEYCN                                                         
         CLC   BUYKEST,MVTESTEN    MATCH ON ESTIMATE                            
         BH    BUYKEYCN                                                         
*                                                                               
         CLC   MVTPRD,SPACES       OKAY IF ALL PRDS WANTED                      
         BNH   BUYKEY20                                                         
*                                                                               
         CLI   MVTPCD,0            SKIP IF PRODUCT CODE UNKNOWN                 
         BE    BUYKEYCN                                                         
*                                                                               
         CLC   BUYKPRD,MVTPCD                                                   
         BE    BUYKEYFD            OKAY IF PRODUCT MATCHES                      
*                                                                               
         CLI   BUYKPRD,X'FF'       IF POL BUY                                   
         BNE   BUYKEYCN                                                         
*                                                                               
         CLC   MVTPCD,BDMASPRD        MATCH ON MASTER PRODUCT CODE              
         BNE   BUYKEYCN                                                         
*                                                                               
BUYKEY20 DS    0H                                                               
*                                                                               
         B     BUYKEYFD                                                         
*                                                                               
BUYKEYCN DS    0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      POINT TO NEXT ENTRY IN TABLE                 
         B     BUYKEYLP                                                         
*                                                                               
BUYKEYFD DS    0H                                                               
*                                                                               
         CLI   MVTBUYS,C'N'       CHECK FOR OVERRIDE                            
         BE    BUYKEYDN                                                         
*                                                                               
         NOP   BUYLOOP2                                                         
         OI    *-3,X'F0'                                                        
         MVC   P(9),=C'COPY BUY '                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(132),SPACES                                                    
                                                                                
BUYLOOP2 DS    0H                                                               
         BRAS  RE,COPY             COPY RECORD TO WORKAREA                      
*                                                                               
         L     R3,=A(NEWREC)       POINT TO NEW RECORD                          
         USING BUYRECD,R3          ESTABLISH SPOT BUY RECORD                    
*                                                                               
         MVC   WRKCLT,BUYKCLT      SAVE PACKED CLIENT                           
*                                                                               
         NI    BUYKAM,X'0F'        KILL OLD AGENCY NYBBLE                       
         OC    BUYKAM,AGYAGNEW     ADD IN NEW AGENCY NYBBLE                     
*                                                                               
         MVC   BUYKCLT,MVTCLPKN    NEW CLIENT                                   
*                                                                               
         OC    MVTPCDNW,MVTPCDNW   SKIP IF NO NEW PRODUCT CODE                  
         BZ    BUYLPNWX            USE IT                                       
*                                                                               
         CLI   BUYKPRD,X'FF'       IF POL BUY                                   
         BNE   BUYLOOP4                                                         
*                                                                               
         CP    MASCTR,=P'0'        TEST IF STILL PRINTING MASTER PROD           
         BE    BUYLOOP3                                                         
*                                                                               
         MVC   P+40(14),=C'MASTER PRODUCT'                                      
         MVC   P+55(3),=C'OLD'                                                  
*                                                                               
         GOTO1 HEXOUT,DMCB,BDMASPRD,P+59,1,0,0   PRINT OLD MASTER PROD          
*                                                                               
         MVC   P+65(3),=C'NEW'                                                  
*                                                                               
         GOTO1 HEXOUT,DMCB,MVTPCDNW,P+69,1,0,0   PRINT NEW MASTER PROD          
*                                                                               
         CLI   MVERROPT,C'E'       SKIP IF ONLY PRINTING ERRORS                 
         BE    BUYLOOPA                                                         
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
BUYLOOPA DS    0H                                                               
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
         SP    MASCTR,=P'1'        DECREMENT MASTER PRD PRINT COUNTER           
*                                                                               
BUYLOOP3 DS    0H                                                               
*                                                                               
         MVC   BDMASPRD(1),MVTPCDNW  CHANGE MASTER PRODUCT CODE                 
         B     BUYLPNWX                                                         
*                                                                               
BUYLOOP4 DS    0H                                                               
*                                                                               
         MVC   BUYKPRD,MVTPCDNW    ELSE CHANGE KEY                              
*                                                                               
BUYLPNWX DS    0H                                                               
*                                                                               
         OC    MVTESTNW,MVTESTNW   IF NEW ESTIMATE NUMBER GIVEN                 
         BZ    *+10                                                             
         MVC   BUYKEST,MVTESTNW       REPLACE ESTIMATE NUMBER                   
*                                                                               
         CLI   MVTLN#NW,0          IF LINE NUMBER TO BE BUMPED                  
         BE    BUYLN#X                                                          
*                                                                               
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         IC    RF,BUYKEY+10           GET LINE #                                
         IC    RE,MVTLN#NW            GET INCREMENT                             
         AR    RF,RE                  ADD ON INCREMENT                          
         STC   RF,BUYKEY+10           SET NEW LINE NUMBER                       
*                                                                               
         CHI   RF,255              IF BUYLINE > 255                             
         BNH   BUYLPNLX                                                         
*                                                                               
         L     RF,AREC             POINT TO OLD RECORD                          
         GOTO1 HEXOUT,DMCB,(RF),P+04,13,0,0   PRINT KEY                         
         MVC   P+40(30),=CL30'NEW BUYLINE GREATER THAN 255'                     
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
         GOTOR PRTBUY,DMCB,=C'OLD'    PRINT KEY FOR MEL                         
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
BUYLPNLX DS    0H                                                               
*                                                                               
BUYLN#X  DS    0H                                                               
*                                                                               
         LA    R2,BUYMSTA          R2 POINTS TO MARKET CODE                     
*                                  R4 POINTS TO CONVERSION TABLE ENTRY          
*                                                                               
         CLI   MVTMDOLD,C'N'       SKIP IF NETWORK                              
         BE    BUYCBL10                                                         
*                                                                               
         CLI   BUYMSTA+2,X'E8'     IF LOCAL CABLE BUYS                          
         BL    BUYCBL10                                                         
*                                                                               
         BRAS  RE,GETCBL           FIND NEW MARKET/STATION                      
         BNZ   BUYKEYDN            SKIP WRITING RECORD                          
         B     BUYCBL20                                                         
*                                                                               
BUYCBL10 DS    0H                                                               
*                                                                               
         MVI   SWITCH,C'B'         INDICATE BUY RECORD                          
         BRAS  RE,GETSTA           FIND NEW MARKET/STATION                      
         MVI   SWITCH,0            CLEAR SWITCH                                 
         BNZ   BUYKEYDN            SKIP WRITING RECORD                          
*                                                                               
BUYCBL20 DS    0H                                                               
*                                                                               
         LA    R6,BDELEM           POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
BELMLOOP DS    0H                                                               
*                                                                               
         USING REGELEM,R6          ESTABLISH AS REGULAR ELEMENT                 
*                                                                               
         CLI   RCODE,0             DONE AT RECORD END                           
         BE    BELMDONE                                                         
*                                                                               
         CLI   RCODE,BDCODEQ       IF BUY DESC ELEMENT                          
         BNE   BELMDSCN                                                         
*                                                                               
         CLI   MVTCOSTS,C'Y'          IF CLEARING COSTS                         
         BE    BELMDSCX                                                         
         CLI   MVCOSTS,C'N'                                                     
         BNE   BELMDSCX                                                         
*                                                                               
         USING BDELEM,R6              ESTABLISH DESCRIPTION ELEMENT             
*                                                                               
         XC    BDCOST,BDCOST          KILL MASTER COST                          
*                                                                               
BELMDSCX DS    0H                                                               
*                                                                               
         B     BELMCONT                                                         
*                                                                               
BELMDSCN DS    0H                                                               
*                                                                               
         USING REGELEM,R6          ESTABLISH AS REGULAR ELEMENT                 
*                                                                               
         CLI   RCODE,NDCORGQ       IF DEMO ELEMENT                              
         BNE   BELMDEMN                                                         
*                                                                               
         CLI   MVTDEMOS,C'Y'       IF DELETING DEMOS                            
         BE    BELMDEMX                                                         
         CLI   MVDEMOS,C'N'                                                     
         BNE   BELMDEMX                                                         
*                                                                               
*        ZERO ALL DEMOS                                                         
*                                                                               
         USING NDELEM,R6           ESTABLISH DEMO ELEMENT                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,NDLEN            GET ELEMENT LENGTH                           
         AHI   R0,-(NDEMNO-NDELEM)  SUBTRACT LENGTH OF HEADER                   
         SRL   R0,3                SET FOR BCT ON NUMBER OF DEMOS               
         BNP   BELMDEMX            NO DEMOS IN ELEMENT                          
*                                                                               
         LA    R1,NDEMNO           POINT TO FIRST DEMO                          
         USING NDEMNO,R1           ESTABLISH DEMO PORTION                       
*                                                                               
         MVI   NDSVI,100            SET SVI=100                                 
         XC    NDEMRAW,NDEMRAW      CLEAR OVRD AND DEMO VALUES                  
         LA    R1,NDEMLNQ(R1)     BUMP TO NEXT DEMO                             
         BCT   R0,*-14                                                          
*                                                                               
BELMDEMX DS    0H                                                               
         B     BELMCONT                                                         
*                                                                               
         DROP  R1                                                               
*                                                                               
BELMDEMN DS    0H                                                               
*                                                                               
         USING REGELEM,R6          ESTABLISH AS REGULAR ELEMENT                 
*                                                                               
         CLI   MVTMDOLD,C'N'       SKIP IF NETWORK                              
         BE    BELM10                                                           
*                                                                               
         CLI   RCODE,X'03'         SPILL DEMO ELEMENT                           
         BNE   BELM10                                                           
*                                                                               
         CLI   MVSPILL,C'D'        IF DELETING SPILL ELEMENTS                   
         BNE   BELM01                                                           
*                                                                               
         MVI   PRTFLAG,C'S'           INDICATE SPILL ELEMENT                    
         BRAS  RE,PRTDEL              GO DELETE ELEMENT                         
*                                                                               
         B     BELMLOOP               GO EXAMINE NEXT ELEMENT                   
*                                                                               
BELM01 DS      0H                                                               
*                                                                               
         USING NDELEM,R6           ESTABLISH DEMO ELEMENT                       
*                                                                               
         LA    R2,NDPROG           POINT TO SPILL MARKET NUMBER                 
*                                                                               
         CP    DEMOCTR,=P'0'       IF STILL PRINTING SPILL MARKETS              
         BNP   BELM03                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,(R3),P+04,13,0,0   PRINT KEY                         
         GOTO1 HEXOUT,DMCB,(R6),P+35,10,0,0   PRINT OLD SPILL ELM               
         SR    RF,RF                                                            
         ICM   RF,3,0(R2)          SPILL MARKET                                 
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  P+65(4),DUB         PRINT OLD MARKET NUMBER                      
*                                                                               
BELM03 DS      0H                                                               
*                                                                               
         ZAP   CTR,=P'9'           SUPRESS PRINTING KEYS                        
         BRAS  RE,GETMKT           CHANGE MARKET NUMBER                         
*                                                                               
         CP    DEMOCTR,=P'0'       ONLY PRINT FIRST 50                          
         BNP   BELM05                                                           
*                                                                               
         SP    DEMOCTR,=P'1'       DECREMENT DEMO PRINT COUNTER                 
         SR    RF,RF                                                            
         ICM   RF,3,0(R2)          SPILL MARKET                                 
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  P+80(4),DUB         PRINT NEW MARKET NUMBER                      
*                                                                               
         CLI   MVERROPT,C'E'       SKIP IF ONLY PRINTING ERRORS                 
         BE    BELM04                                                           
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
BELM04 DS      0H                                                               
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
BELM05 DS      0H                                                               
*                                                                               
         B     BELMCONT                                                         
*                                                                               
BELM10 DS      0H                                                               
*                                                                               
         USING REGELEM,R6          RE-ESTABLISH REGULAR SPOT ELEMENT            
*                                                                               
         CLI   RCODE,X'06'         LOOK FOR REGULAR ELEMENTS                    
         BL    BELMCONT                                                         
         CLI   RCODE,X'0D'         LOOK FOR REGULAR ELEMENTS                    
         BH    BELMCONT                                                         
*                                                                               
*        FILTER ON DATE RANGE                                                   
*                                                                               
         OC    MVTSTDTE,MVTSTDTE   ANY START DATE                               
         BZ    BELML2                                                           
*                                                                               
         CLC   RDATE,MVTSTDTE                                                   
         BNL   BELML2              ACCEPT SPOT                                  
*                                                                               
         MVI   PRTFLAG,C'S'                                                     
         BRAS  RE,PRTDEL                                                        
*                                                                               
         B     BELMLOOP                                                         
*                                                                               
BELML2 OC      MVTNDDTE,MVTNDDTE   ANY END DATE                                 
         BZ    BELML4                                                           
*                                                                               
         CLC   RDATE,MVTNDDTE                                                   
         BNH   BELML4                                                           
         MVI   PRTFLAG,C'E'                                                     
         BRAS  RE,PRTDEL                                                        
         B     BELMLOOP                                                         
*                                                                               
BELML4 DS      0H                                                               
*                                                                               
         CLI   RCODE,RCPOLOQ       IF POOL SPOT                                 
         BL    BELMPOLX                                                         
*                                                                               
         CLC   RPPRD,MVTPCD        IF ALLOCATED TO WANTED PRODUCT               
         BNE   BELMPOL5                                                         
         CLC   MVTPRDNW,SPACES     AND PRODUCT ID CHANGING                      
         BNH   BELMPOL5                                                         
*                                                                               
         MVC   RPPRD,MVTPCDNW         SET NEW PRODUCT CODE                      
*                                                                               
BELMPOL5 DS    0H                                                               
*                                                                               
         CLI   MVTCOSTS,C'N'       IF CLEARING COSTS                            
         BE    *+12                                                             
         CLI   MVCOSTS,C'N'                                                     
         BNE   BELMCSTX                                                         
*                                                                               
         NI    RSTATUS,X'FF'-RSRATOVQ    KILL RATE OVERRIDE                     
         XC    RPCOST,RPCOST             KILL COST OVERRIDE                     
*                                                                               
BELMCSTX DS    0H                                                               
*                                                                               
BELMPOLX DS    0H                                                               
*                                                                               
         OC    RPAY,RPAY           OKAY IF UNPAID                               
         BZ    BELMCONT                                                         
*                                                                               
         CP    PAYCTR,=P'0'        SKIP IF ENOUGH PRINTED                       
         BE    BUYPRT10                                                         
*                                                                               
         GOTO1 HEXOUT,DMCB,(R3),P+04,13,0,0   PRINT KEY                         
         GOTO1 HEXOUT,DMCB,(R6),P+40,16,0,0   PRINT REGULAR ELEMENT             
*                                                                               
         CLI   MVERROPT,C'E'       SKIP IF ONLY PRINTING ERRORS                 
         BE    BELML6                                                           
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
BELML6 DS      0H                                                               
*                                                                               
         MVC   P(132),SPACES                                                    
*                                                                               
         SP    PAYCTR,=P'1'                                                     
*                                                                               
BUYPRT10 DS    0H                                                               
*                                                                               
         XC    RPAY,RPAY           KILL CLEARANCE DATE                          
*                                                                               
BELMCONT DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RLEN             GET ELEMENT LENGTH                           
         LA    R6,REGELEM(RF)      BUMP TO NEXT ELEMENT                         
         B     BELMLOOP                                                         
*                                                                               
BELMDONE DS    0H                                                               
*                                                                               
         BC    0,BUYLOOP8                                                       
         OI    *-3,X'F0'                                                        
         MVC   P(9),=C'WRITE BUY'                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(132),SPACES                                                    
                                                                                
BUYLOOP8 DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R0,3,13(R3)         GET RECORD LENGTH                            
         AHI   R0,4                                                             
         LR    RE,R3                                                            
         AHI   RE,-4                                                            
         STH   R0,0(RE)            SET LEN+4 AT REC-4                           
         BRAS  RE,RECWRITE         WRITE RECORD TO OUTPUT DATASET               
*                                                                               
         CLI   MVTESTNW,0          IF CHANGING ESTIMATE NUMBER                  
         BE    BUYKEYDN               GO CHECK IF BUY FITS ANOTHER              
*                                     TABLE ENTRY                               
         L     R3,AREC                RE-POINT TO INCOMING RECORD               
         B     BUYKEYCN                                                         
*                                                                               
BUYKEYDN DS    0H                                                               
*                                                                               
BUYX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPREPBW02 - MOVE BETWEEN AGENCIES - BUY'                        
***********************************************************************         
*                                                                     *         
*        BUY RECORD - >X'10'                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRTBUY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPWORKD,RA,R9       ESTABLISH SPONSOR WORKING STORAGE            
         USING WORKD,RC            ESTABLISH LOCAL WORKING STORAGE              
         USING MVTABD,R4                                                        
*                                                                               
         L     RF,0(R1)            POINT TO OPTION                              
*                                                                               
         MVC   P(132),SPACES       INIT PRINT LINE                              
*                                                                               
         CLC   =C'OLD',0(RF)       IF OLD RECORD                                
         BNE   *+16                                                             
         MVI   PRBSW,C'O'             SET LOCAL SWITCH                          
         L     R3,AREC                POINT TO OLD KEY                          
         B     PRTBUY10                                                         
*                                                                               
         CLC   =C'NEW',0(RF)       IF NEW RECORD                                
         BNE   *+16                                                             
         MVI   PRBSW,C'N'             SET LOCAL SWITCH                          
         L     R3,=A(NEWREC)          POINT TO NEW KEY                          
         B     PRTBUY10                                                         
*                                                                               
         B     PRTBUYX                                                          
*                                                                               
         USING BUYRECD,R3          ESTABLISH BUY RECORD                         
*                                                                               
PRTBUY10 DS    0H                                                               
*                                                                               
         CLI   PRBSW,C'O'          IF OLD KEY                                   
         BNE   PRBOLDN                                                          
*                                                                               
         MVC   P+4(2),MVTCHOLD     AGENCY                                       
         MVC   P+7(1),MVTMDOLD     MEDIA                                        
         MVC   P+9(3),MVTCLOLD     CLIENT                                       
         MVC   P+13(3),MVTPRD      PRINT                                        
*                                                                               
         B     PRTBUY20                                                         
*                                                                               
PRBOLDN  DS    0H                  NEW KEY                                      
*                                                                               
         MVC   P+4(2),MVTCHNEW     AGENCY                                       
         MVC   P+7(1),MVTMDNEW     MEDIA                                        
         MVC   P+9(3),MVTCLTNW     CLIENT                                       
         MVC   P+13(3),MVTPRD      PRINT                                        
*                                                                               
PRTBUY20 DS    0H                                                               
*                                                                               
         XC    STAWORK,STAWORK     INIT STAPACK WORKAREA                        
         LA    R6,STAWORK                                                       
         USING STAPACKD,R6                                                      
*                                                                               
         MVI   STAPACT,C'U'        ACTION - UNPACK                              
         MVC   STAPAGY,MVTCHOLD    AGENCY                                       
         MVC   STAPCTRY,COUNTRY                                                 
         MVC   STAPMED,MVTMDOLD    MEDIA                                        
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMKST,BUYKMSTA   MKT/STA/NET                                  
*                                                                               
         GOTO1 VSTAPACK,STAWORK    UNPACK MKT/STA/NET                           
*                                                                               
         MVC   P+18(4),STAPQMKT       OLD MARKET                                
         MVC   P+23(8),STAPQSTA       OLD STATION/NETWORK                       
*                                                                               
         EDIT  (B1,BUYKEST),(3,P+32)    ESTIMATE                                
         EDIT  (B1,BUYKEY+10),(3,P+36)  LINE NUMBER                             
*                                                                               
PRTBUYX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
PRBSW    DS    XL1                 PRINT SWITCH - O/N                           
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - WORKD'                     
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DS    0D                                                               
*                                                                               
APARM    DS    A                   A(PARAMETER LIST)                            
STATABA  DS    A                   A(STATION TABLE)                             
*                                                                               
SAVCLIST DS    XL880               CLIST SAVEAREA                               
*                                                                               
SWITCH   DC    X'0'                C'B' - INDCATES MKT FROM BUY RECORD          
COUNT    DS    F                                                                
CTR      DC    PL2'10'             TRACE COUNTER                                
RECCNTR  DC    PL8'00'             RECORD COUNTER                               
WRCTR    DC    PL8'00'             RECORD WRITE COUNTER                         
DEMOCTR  DC    PL2'50'             DEMO ELEMENT PRINT COUNTER                   
RELMCTR  DC    PL2'50'             REGULAR ELEMENT PRINT COUNTER                
MASCTR   DC    PL2'50'             MASTER PRODUCT  PRINT COUNTER                
PAYCTR   DC    PL2'50'             PAY ELM         PRINT COUNTER                
STANTRYC DS    XL(STANTRYL)        BUILD AREA FOR STA TAB                       
STAKOLD  DS    XL32                OLD AGENCY STATION KEY                       
STAKNEW  DS    XL32                NEW AGECNY STATION KEY                       
*                                                                               
BSPSTA   DS    XL28                BINSRCH PARMS FOR STATION TABLE              
*                                                                               
BYRNTRYC DS    XL(BYRNTRYL)        BUYER   TABLE ENTRY BUILD AREA               
BYRTABA  DS    A                   A(BUYER   BINSRCH TABLE)                     
*                                                                               
PRINTSW  DC    XL1'00'             X'01' - PRINT TRACE                          
OLDKEY   DC    XL13'00'            OLD KEY SAVEAREA                             
NEWKEY   DC    XL13'00'            NEW KEY SAVEAREA                             
EOFTOSW  DC    X'00'               END OF FILE SWITCH FOR TO FILE               
CNDATA   DC    XL14'00'            DDCNTRL BUILD AREA                           
*                                                                               
ELCODE   DS    X                                                                
WRKCLT   DS    XL2                 PACKED CLIENT CODE                           
*                                                                               
PRTFLAG  DC    C' '                                                             
PRTCOUNT DC    PL3'0'                                                           
*                                                                               
         DS    0F                                                               
STAWORK  DS    CL32                STAPACK WORKAREA                             
*                                                                               
SAVEKEY  DS    XL13                KEY SAVEAREA                                 
*                                                                               
         DS    0F                                                               
AGYCHOLD DS    CL2                 AGENCY       -OLD                            
AGYMDOLD DC    XL1'00'             MEDIA        -OLD                            
AGYAGOLD DC    XL1'00'             AGENCY       -OLD                            
AGYAMOLD DC    XL1'00'             AGENCY/MEDIA -OLD                            
AGYSEOLD DC    XL1'00'             AGENCY SE #  -OLD                            
AGYCHNEW DS    CL2                 AGENCY       -NEW                            
AGYMDNEW DC    XL1'00'             MEDIA        -NEW                            
AGYAGNEW DC    XL1'00'             AGENCY       -NEW                            
AGYAMNEW DC    XL1'00'             AGENCY/MEDIA -NEW                            
AGYSENEW DC    XL1'00'             AGENCY SE #  -NEW                            
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - MVTAB'                     
***********************************************************************         
*                                                                     *         
*        DATA FOR THIS CONVERSION                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*        RECORD TYPES TO MOVE                                                   
*                                                                               
MVBUYS   DC    C'N'                MOVE BUYS             - Y/N                  
MVCOSTS  DC    C'N'                MOVE COSTS            - Y/N                  
MVDEMOS  DC    C'N'                MOVE DEMOS            - Y/N                  
MVGOALS  DC    C'N'                MOVE GOALS            - Y/N                  
MVNWS    DC    C'N'                MOVE BUYERS WORKSHEET - Y/N                  
MVCGRP   DC    C'N'                MOVE CLIENT GROUP RECS- Y/N                  
MVCLT    DC    C'N'                DO NOT MOVE CLIENT   HDR - Y/N               
MVPRD    DC    C'N'                DO NOT MOVE PRODUCT  HDR - Y/N               
MVEST    DC    C'N'                DO NOT MOVE ESTIMATE HDR - Y/N               
MVCABLE  DC    C'N'                MOVE CABLE BUYS       - Y/N                  
MVSPILL  DC    C' '                DELETE SPILL          - D                    
MVRDTYPE DC    C'T'                READ FROM TAPE OR FILE - T/F                 
*                                    DEFAULT IS TAPE                            
MVWRTYPE DC    C' '                WRITE TO FILE - C'W'                         
*                                    DEFAULT IS BLANK                           
MVERROPT DC    C' '                'E' ONLY PRINT ERRORS                        
*                                                                               
*        TABLE OF WHAT TO MOVE                                                  
*                                                                               
MVTAB    DS    0X                  TABLE ENTRY                                  
*                                                                               
         DS    400XL(MVTENTL)      TRANSFER TABLE                               
*                                                                               
         DC    X'FF'               EOT                                          
*                                                                               
RRLEN    DS    XL2                 OUTPUT RECORD LENGTH                         
         DS    XL2                 SPARE                                        
NEWREC   DS    XL4096              NEW RECORD BUILD AREA                        
*                                                                               
WORKLQ   EQU   *-WORKD             LENGTH OF WORKING STORAGE                    
         EJECT                                                                  
CARD     DCB   DDNAME=CARDIN,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,      X        
               EODAD=MTBINIDN                                                   
*                                                                               
*        LOAD FILES                                                             
*                                                                               
FILEOUT  DCB   DDNAME=TEMPOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               BLKSIZE=25000                                                    
*                                                                               
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),EODAD=INDONE,         X        
               RECFM=VB,LRECL=4004,BUFNO=2                                      
*                                                                               
*        INPUT RECORD AREA                                                      
*                                                                               
         DS    0D                                                               
         DC    C'DADADADA'                                                      
DARECH   DS    F                                                                
DAREC    DS    16384C                                                           
DARECX   DS    0C                                                               
         SPACE 2                                                                
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - BSRPRMD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR BINSRCH PARAMETERES                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BSRPRMD  DSECT                                                                  
       ++INCLUDE DDBSRPRMD                                                      
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - MVTABD'                    
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF DATA TO MOVE                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MVTABD   DSECT                                                                  
MVTENT   DS    0X                  ENTRY IN TABLE                               
MVTAGOLD DS    XL1                 AGENCY NMBR  - OLD                           
MVTAGNEW DS    XL1                 AGENCY NMBR  - NEW                           
MVTCHOLD DS    CL2                 AGENCY CH    - OLD                           
MVTCHNEW DS    CL2                 AGENCY CH    - NEW                           
MVTAMOLD DS    XL1                 AGENCY/MEDIA - OLD                           
MVTAMNEW DS    XL1                 AGENCY/MEDIA - NEW                           
MVTMDOLD DS    CL1                 MEDIA  CH    - OLD                           
MVTMDNEW DS    CL1                 MEDIA  CH    - NEW                           
MVTCLTPK DS    XL2                 CLIENT - PACKED                              
MVTCLOLD DS    CL3                 CLIENT - OLD                                 
MVTCLNEW DS    CL3                 CLIENT - NEW                                 
MVTCLPKN DS    XL2                 CLIENT - NEW PACKED                          
MVTCLTNW DS    CL3                 CLIENT - NEW                                 
MVTPCD   DS    XL1                 PRODUCT CODE   - NULLS MEANS ALL             
MVTPCDNW DS    XL1                 NEW PRODUCT CODE                             
MVTPRD   DS    XL3                 PRODUCT        - NULLS MEANS ALL             
MVTPRDNW DS    XL3                 NEW PRODUCT                                  
MVTESTST DS    AL1                 START ESTIMATE - NULLS MEANS ALL             
MVTESTEN DS    AL1                 END   ESTIMATE - NULLS MEANS ALL             
MVTESTNW DS    AL1                 NEW ESTIMATE NUMBER                          
MVTLN#NW DS    AL1                 LINE NUMBER INCREMENT                        
MVTSTDTE DS    XL2                 START DATE                                   
MVTNDDTE DS    XL2                 END DATE                                     
MVTMKT   DS    CL4                 MARKET                                       
MVTSTA   DS    CL8                 STATION                                      
MVTCLTMV DS    CL1                 C'Y'- DON'T MOVE CLTHDR                      
MVTPRDMV DS    CL1                 C'Y'- DON'T MOVE PRDHDR                      
MVTESTMV DS    CL1                 C'Y'- DON'T MOVE ESTHDR                      
MVTBUYS  DS    CL1                 C'Y'- MOVE BUYS                              
MVTDEMOS DS    CL1                 C'N'- MOVE BUYS WITHOUT DEMOS                
MVTCOSTS DS    CL1                 C'N'- MOVE BUYS WITHOUT COSTS                
MVTGOALS DS    CL1                 C'Y'- MOVE GOALS                             
MVTENTL  EQU   *-MVTENT            TABLE ENTRY LENGTH                           
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - STATABD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR STATION CONVERSION TABLE                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STANTRYD DSECT                                                                  
STANTRY  DS    0XL1                CONVERSION RECORD                            
STTKEY   DS    0XL1                KEY FOR TABLE                                
STTAMOLD DS    XL1                 OLD AGENCY/MEDIA                             
STTMSOLD DS    XL5                 OLD MARKET/STATION                           
STTCPOLD DS    CL2                 OLD CLIENT - PACKED                          
STTKEYLQ EQU   *-STTKEY            KEY LENGTH                                   
*                                                                               
STTMDOLD DS    CL1                 OLD MEDIA                                    
STTMKOLD DS    CL4                 OLD MARKET NUMBER                            
STTSTOLD DS    CL8                 OLD STATION                                  
STTCLOLD DS    CL3                 OLD CLIENT                                   
STTAMNEW DS    XL1                 NEW AGENCY/MEDIA                             
STTMSNEW DS    XL5                 NEW MARKET/STATION                           
STTMKNEW DS    CL4                 NEW MARKET NUMBER                            
STTSTNEW DS    CL8                 NEW STATION                                  
STANTRYL EQU   *-STANTRY           RECORD LENGTH                                
*                                                                               
STTRMAXQ EQU   20000               MAXIMUM NUMBER OF RECORDS IN FILE            
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - FROMRECD'                  
***********************************************************************         
*                                                                     *         
*        DSECT FOR FROM MARKET/STATION CONVERSION RECORD              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FRMRECD  DSECT                                                                  
FRMREC   DS    0XL1                CONVERSION RECORD                            
FRMKEY   DS    0XL15               STATION RECORD KEY                           
FRMTYPE  DS    CL1'S'              STATION RECORD TYPE                          
FRMMED   DS    CL1                 MEDIA                                        
FRMCALL  DS    CL5                 STATION CALL LETTERS                         
FRMAGY   DS    CL2                 AGENCY                                       
FRMCLT   DS    CL3                 CLIENT                                       
         DS    CL3'000'            FILL                                         
*                                                                               
FRMMKT   DS    CL4                 MARKET                                       
FRMMKSTA DS    XL5                 MKT/STA PACKED                               
FRMAGYMD DS    CL1                 AGENCY/MEDIA                                 
         DS    XL(80-(*-FRMRECD))  SPARE                                        
FRMRECLQ EQU   *-FRMREC            RECORD LENGTH                                
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - TORECD'                    
***********************************************************************         
*                                                                     *         
*        DSECT FOR TO   MARKET/STATION CONVERSION RECORD              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TORECD   DSECT                                                                  
TOREC    DS    0XL1                CONVERSION RECORD                            
TOKEY    DS    0XL15               STATION RECORD KEY                           
TOTYPE   DS    CL1'S'              STATION RECORD TYPE                          
TOMED    DS    CL1                 MEDIA                                        
TOCALL   DS    CL5                 STATION CALL LETTERS                         
TOAGY    DS    CL2                 AGENCY                                       
TOCLT    DS    CL3                 CLIENT                                       
         DS    CL3'000'            FILL                                         
*                                                                               
TOMKT    DS    CL4                 MARKET                                       
TOMKSTA  DS    XL5                 MKT/STA PACKED                               
TOAGYMD  DS    CL1                 AGENCY/MEDIA                                 
         DS    XL(80-(*-TORECD))   SPARE                                        
TORECLQ EQU    *-TOREC             RECORD LENGTH                                
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - BYRTABD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR BUYER   CONVERSION TABLE                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BYRNTRYD DSECT                                                                  
BYRNTRY  DS    0XL1                CONVERSION RECORD                            
BYTKEY   DS    0XL1                KEY FOR TABLE                                
BYTAMOLD DS    XL1                 OLD AGENCY/MEDIA                             
BYTBCOLD DS    XL1                 OLD INTERNAL BUYER CODE                      
BYTKEYLQ EQU   *-BYTKEY            KEY LENGTH                                   
*                                                                               
BYTBYOLD DS    CL3                 OLD BUYER ID                                 
BYTBNOLD DS    CL20                OLD BUYER NAME                               
BYTAMNEW DS    XL1                 NEW AGENCY/MEDIA                             
BYTBYNEW DS    XL3                 NEW BUYER ID                                 
BYTBCNEW DS    XL1                 NEW INTERNAL BUYER CODE                      
BYTBNNEW DS    CL20                NEW BUYER NAME                               
BYRNTRYL EQU   *-BYRNTRY           RECORD LENGTH                                
*                                                                               
BYTRMAXQ EQU   600                 MAXIMUM NUMBER OF RECORDS IN FILE            
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - BYRFRMD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR FROM BUYER CONVERSION RECORD                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BYRFRMD  DSECT                                                                  
BYRFRM   DS    0XL1                CONVERSION RECORD                            
BFRKEY   DS    0XL13               BUYER RECORD KEY                             
BFRKTYP  DS    XL1'0D'             STATION RECORD TYPE                          
BFRKSUB  DS    XL1'65'             STATION RECORD SUB TYPE                      
BFRKAM   DS    CL1                 AGENCY/MEDIA                                 
BFRKBYR  DS    CL3                 BUYER ID                                     
         ORG   BFRKEY+L'BFRKEY                                                  
*                                                                               
*                                                                               
BFRBYRCD DS    XL1                 BUYER CODE                                   
BFRBYRNM DS    CL20                BUYER NAME                                   
BFRAGYMD DS    CL1                 AGENCY/MEDIA                                 
         DS    XL(80-(*-BYRFRM))   SPARE                                        
BFRRECLQ EQU   *-BYRFRM            RECORD LENGTH                                
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - BYRTOD'                    
***********************************************************************         
*                                                                     *         
*        DSECT FOR TO   BUYER          CONVERSION RECORD              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BYRTOD   DSECT                                                                  
BYRTO    DS    0XL1                CONVERSION RECORD                            
BTOKEY   DS    0XL13               BUYER RECORD KEY                             
BTOKTYP  DS    XL1'0D'             STATION RECORD TYPE                          
BTOKSUB  DS    XL1'65'             STATION RECORD SUB TYPE                      
BTOKAM   DS    CL1                 AGENCY/MEDIA                                 
BTOKBYR  DS    CL3                 BUYER ID                                     
         ORG   BTOKEY+L'BTOKEY                                                  
*                                                                               
*                                                                               
BTOBYRCD DS    XL1                 BUYER CODE                                   
BTOBYRNM DS    CL20                BUYER NAME                                   
BTOAGYMD DS    CL1                 AGENCY/MEDIA                                 
         DS    XL(80-(*-BYRTOD))   SPARE                                        
BTORECLQ EQU   *-BYRTO             RECORD LENGTH                                
*                                                                               
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
         DS    CL1                                                              
         DS    CL2                                                              
         DS    CL1                                                              
         DS    CL2                                                              
POLDAM   DS    CL2                 AGENCY/MEDIA   - OLD - HEX                   
         DS    CL2                                                              
         DS    CL1                                                              
POLDMSTA DS    CL10                MARKET/STATION - OLD - HEX                   
         DS    CL2                                                              
POLDMK   DS    CL4                 MARKET NUMBER  - OLD - DECIMAL               
         DS    CL2                                                              
POLDSTA  DS    CL8                 STATION CALL   - OLD - DECIMAL               
         DS    CL2                                                              
POLDCLT  DS    CL3                 CLIENT         - OLD                         
         DS    CL1                                                              
POLDAGY  DS    CL2                 AGENCY         - OLD                         
         DS    CL1                                                              
PNEWAM   DS    CL2                 AGENCY/MEDIA   - NEW - HEX                   
         DS    CL2                                                              
PNEWMSTA DS    CL10                MARKET/STATION - NEW - HEX                   
         DS    CL2                                                              
PNEWMK   DS    CL4                 MARKET NUMBER  - NEW - DECIMAL               
         DS    CL2                                                              
PNEWSTA  DS    CL8                 STATION CALL   - NEW                         
         DS    CL2                                                              
PNEWAGY  DS    CL2                 AGENCY         - NEW                         
         DS    CL2                                                              
PNEWCLT  DS    CL3                 CLIENT         - NEW                         
         DS    CL1                                                              
* DSECT FOR TITLE PRINT LINE                                                    
PTLINED  DSECT                                                                  
         DS    CL1                                                              
PTAMOLD  DS    CL6'AM-OLD'         AGENCY/MEDIA - OLD - HEX                     
         DS    CL1                                                              
PTAMNEW  DS    CL6'AM-NEW'         AGENCY/MEDIA - NEW - HEX                     
         DS    CL1                                                              
         DS    CL2                                                              
PTCLTOLD DS    CL3'CLT'            CLIENT - OLD                                 
         DS    CL2                                                              
PTCLPOLD DS    CL4'CLT'            PACKED CLIENT - OLD                          
         DS    CL2                                                              
PTAGYOLD DS    CL2'AG'             AGENCY - OLD                                 
         DS    CL1                                                              
PTCLTNEW DS    CL3'CLT'            CLIENT - NEW                                 
         DS    CL2                                                              
PTCLPNEW DS    CL4'CLT'            PACKED CLIENT - NEW                          
         DS    CL2                                                              
PTOLDMST DS    CL10'MKT/STA'       MARKET/STATION - OLD - HEX                   
         DS    CL2                                                              
PTOLDMKT DS    CL4'MKT'            MARKET NUMBER  - OLD - DECIMAL               
         DS    CL2                                                              
PTOLDSTA DS    CL8'STA'            STATION CALL   - OLD                         
         DS    CL2                                                              
PTNEWMST DS    CL10'MKT/STA'       MARKET/STATION - NEW - HEX                   
         DS    CL2                                                              
PTNEWMKT DS    CL4'MKT'            MARKET NUMBER  - NEW - DECIMAL               
         DS    CL2                                                              
PTNEWSTA DS    CL8'STA'            STATION CALL   - NEW                         
         DS    CL1                                                              
PTAGYNEW DS    CL2'AG'             AGENCY - NEW                                 
         DS    CL1                                                              
*                                                                               
PBLINED  DSECT                     BUYER PRINT LINE                             
         DS    CL1                                                              
         DS    CL2                                                              
         DS    CL1                                                              
         DS    CL2                                                              
PBAMOLD  DS    CL2                 AGENCY/MEDIA   - OLD - HEX                   
         DS    CL2                                                              
         DS    CL1                                                              
PBOLDBYR DS    CL3                 BUYER ID       - OLD                         
         DS    CL2                                                              
PBOLDBC  DS    CL3                 BUYER CODE     - OLD - DECIMAL               
         DS    CL2                                                              
PBOLDBN  DS    CL20                BUYER NAME     - OLD                         
         DS    CL2                                                              
PBAMNEW  DS    CL2                 AGENCY/MEDIA   - NEW - HEX                   
         DS    CL2                                                              
PBNEWBYR DS    CL3                 BUYER ID       - NEW                         
         DS    CL2                                                              
PBNEWBC  DS    CL3                 BUYER CODE     - NEW - DECIMAL               
         DS    CL2                                                              
PBNEWBN  DS    CL20                BUYER NAME     - NEW                         
         DS    CL2                                                              
* DSECT FOR TITLE PRINT LINE                                                    
PBTLINED  DSECT                     BUYER PRINT LINE                            
         DS    CL1                                                              
         DS    CL2                                                              
         DS    CL1                                                              
         DS    CL2                                                              
PBTAMOLD DS    CL2                 AGENCY/MEDIA   - OLD - HEX                   
         DS    CL2                                                              
         DS    CL1                                                              
PBTOLDBY DS    CL3                 BUYER ID       - OLD                         
         DS    CL2                                                              
PBTOLDBC DS    CL3                 BUYER CODE     - OLD - DECIMAL               
         DS    CL2                                                              
PBTOLDBN DS    CL20                BUYER NAME     - OLD                         
         DS    CL2                                                              
PBTAMNEW DS    CL2                 AGENCY/MEDIA   - NEW - HEX                   
         DS    CL2                                                              
PBTNEWBY DS    CL3                 BUYER ID       - NEW                         
         DS    CL2                                                              
PBTNEWBC DS    CL3                 BUYER CODE     - NEW - DECIMAL               
         DS    CL2                                                              
PBTNEWBN DS    CL20                BUYER NAME     - NEW                         
         DS    CL2                                                              
*                                                                               
*        MOVE TABLE PRINTOUT LINE                                               
*                                                                               
PTBLINED DSECT                                                                  
         DS    CL2                                                              
PTBAMOLD DS    CL2                 AGENCY/MEDIA - OLD                           
         DS    CL2                                                              
PTBAMNEW DS    CL2                 AGENCY/MEDIA - NEW                           
         DS    CL2                                                              
PTBCLT   DS    CL3                 CLIENT                                       
         DS    CL2                                                              
PTBCLTPK DS    XL4                 CLIENT PACKED                                
         DS    CL2                                                              
PTBCLTNW DS    CL3                 CLIENT NEW                                   
         DS    CL2                                                              
PTBCLPKN DS    XL4                 CLIENT PACKED NEW                            
         DS    CL2                                                              
PTBPRD   DS    CL3                 PRODUCT                                      
         DS    CL2                                                              
PTBPCD   DS    CL2                 PRODUCT CODE                                 
         DS    CL2                                                              
PTBPRDNW DS    CL3                 PRODUCT NEW                                  
         DS    CL2                                                              
PTBPCDNW DS    CL3                 PRODUCT CODE NEW                             
         DS    CL2                                                              
PTBESTST DS    CL3                 START ESTIMATE                               
         DS    CL2                                                              
PTBESTEN DS    CL3                 END   ESTIMATE                               
         DS    CL2                                                              
PTBESTNW DS    CL3                 ESTIMATE NEW                                 
         DS    CL2                                                              
PTBMKT   DS    CL4                 MARKET FILTER                                
         DS    CL2                                                              
         EJECT                                                                  
*SPGENCLT                                                                       
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENPRD                                                                       
         PRINT OFF                                                              
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENBUY                                                                       
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENEST                                                                       
         PRINT OFF                                                              
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENGRP                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPGENGRP                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENSTA                                                                       
         PRINT OFF                                                              
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENGOAL                                                                      
         PRINT OFF                                                              
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
         PRINT ON                                                               
*DDCNTRL                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDCNTRL                                                        
         PRINT ON                                                               
*SPREPMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
         PRINT ON                                                               
*SPREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         ORG   QUESTOR                                                          
QNWAGY   DS    CL2                 NEW AGENCY POWER CODE                        
QNWCLT   DS    CL3                 NEW CLIENT   CODE                            
QNWPRD   DS    CL3                 NEW PRODUCT  CODE                            
*                                    QPRD2 HAS NEW PRODUCT CODE IF              
*                                    READING FROM TAPE                          
QNWEST   DS    CL3                 NEW ESTIMATE CODE                            
QRDTYPE  DS    CL1                 WHAT FILE TO READ                            
*                                  C'T' - DUMP TAPE                             
*                                  C'F' - SPOT FILE                             
*                                                                               
         ORG                                                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPREPBW02 03/23/04'                                      
         END                                                                    
*                                                                               
