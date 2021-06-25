*          DATA SET ACREP5703  AT LEVEL 181 AS OF 02/12/20                      
*PHASE AC5703A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE POWWOW                                                                 
         TITLE 'BANK RECONCILIATION MODULE'                                     
AC5703   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**5703**,R9                                                    
         USING AC57D,RC                                                         
         L     RC,4(R1)            RESTORE RC                                   
         USING ACWORKD,RA                                                       
         L     RA,0(R1)                                                         
         USING BIGPRNTD,R8                                                      
         L     R8,VBIGPRNT                                                      
         SR    R1,R1               SET UP ADDRESSABILITY TO GTBBLK              
         LA    R1,GTBBLK                                                        
         ST    R1,AGTBBLK                                                       
*                                                                               
         CLI   MYBYTE,1                                                         
         BNE   PRCTRN                                                           
         ZAP   DMPCNT,=P'0'        FIRST TIME INITIALIZATION                    
         XC    BLKCNT,BLKCNT                                                    
         GOTO1 =A(FILTER),DMCB,(RC) SEE IF THIS COMPANY QUALIFIES               
         TM    FLAG,FLGTPE    NO- IT DOESN'T                                    
         BO    EXIT                                                             
         B     PROCESS                                                          
*        USING BKD,R2                                                           
*        L     R2,ABKBUF           A(BKBUF RECORDS FROM SORT)                   
*                                                                               
*                                                                               
*        TM    FLAG,FLGPOS         POSPAY?                                      
*        BO    PRCFRST               NEED TO USE ACTIVITY DATE                  
*        CLI   QOPT6,C'A'          ACTIVITY DATE REQUESTED?                     
*        BE    PRCFRST               USE ACTIVITY DATE                          
*        TM    BKDTYP,BKDTRN       IS TRAN DATE WITHIN RANGE?                   
*        BNZ   PROCESS                                                          
*        OI    FLAG,FLGNOP                                                      
*        B     EXIT                                                             
*RCFRST  TM    BKDTYP,BKDACT       IS ACTIVITY DATE WITHIN RANGE?               
*        BNZ   PROCESS                                                          
*        OI    FLAG,FLGNOP                                                      
*        B     EXIT                                                             
*                                                                               
PRCTRN   CLI   MYBYTE,2                                                         
         BNE   PROCESS                                                          
         L     R2,ABKBUF           A(BKBUF RECORDS FROM SORT)                   
         USING BKD,R2                                                           
         CLC   BKBANK,SAVEBANK     STILL READING THE SAME ACCOUNT               
         BE    PRCTRN10            YES,                                         
         GOTO1 =A(FILTER),DMCB,(RC) NO, STILL USING THE SAME ROUTINE?           
         TM    FLAG,FLGSWCH        SAME ROUTINE?                                
         BO    EXIT                NO - EXIT                                    
         TM    FLAG,FLGTPE         SET UP FOR TAPE                              
         BO    EXIT                NO - EXIT                                    
         MVC   SAVEBANK,BKBANK                                                  
PRCTRN10 CLI   BKTRNTYP,X'25'      IS THIS TYPE 37                              
         BNE   PRCTRN15                                                         
         CLI   PROGPROF+1,C'Y'     DO WE WANT THEM                              
         BNE   EXIT                EXCLUDE TYPE 37 FROM TAPE                    
*************                                                                   
PRCTRN15 TM    FLAG,FLGPOS         POSPAY?                                      
         BO    PRCTRN20              NEED TO USE ACTIVITY DATE                  
         CLI   QOPT6,C'A'          ACTIVITY DATE REQUESTED?                     
         BE    PRCTRN20              USE ACTIVITY DATE                          
         TM    BKDTYP,BKDTRN       IS TRAN DATE WITHIN RANGE?                   
         BNZ   PROCESS                                                          
         OI    FLAG,FLGNOP                                                      
         B     EXIT                                                             
PRCTRN20 TM    BKDTYP,BKDACT       IS ACTIVITY DATE WITHIN RANGE?               
         BNZ   PROCESS                                                          
         OI    FLAG,FLGNOP                                                      
         B     EXIT                                                             
*************                                                                   
*                                                                               
PROCESS  LA    R3,TPREC            TPREC=1000 BYTES                             
         L     R6,=A(MYDTF)                                                     
         L     R4,SPECS                                                         
         USING SPECD,R4                                                         
         L     R5,BRANCH                                                        
         OC    BRANCH,BRANCH       MAKE SURE WE HAVE ADDRESS                    
         BNZ   *+14                                                             
         CLI   MYBYTE,3            ONLY OK IN MYBYTE=3 WHICH                    
         BE    EXIT                MEANS NO RECORDS PUT TO SORTER               
         DC    H'0'                  DIE OTHERWISE - SOMETHING IS WRONG         
*                                                                               
         BASR  RE,R5                                                            
*                                                                               
         CLI   MYBYTE,3                                                         
         BNE   EXIT                                                             
*                                                                               
PROCEND  CLI   QOPT2,C'Y'          TEST OUTPUT TAPE                             
         BNE   EXIT                                                             
*                                                                               
         CLI   SVTYP,TYPMQ         DO WE WANT TO SEND MESSAGES TO MQ ?          
         JE    PROC30              YES, PROCESS MQ                              
*                                                                               
         TM    SPSTAT,SPPOSCOM     X'40' - TEST IF COMPRESSING                  
         JO    PROC10                                                           
         TM    SPSTAT,SPPOSEQ      X'80' - TEST POSITIVE PAY                    
         JO    PROC20                                                           
         J     EXIT                                                             
PROC10   GOTO1 ACOM,DMCB,(RC)      SUBMIT COMPRESS JCL                          
PROC20   GOTO1 APOW,DMCB,(RC)      SUBMIT POS PAY TRANSFER JCL                  
         J     EXIT                                                             
*                                                                               
PROC30   GOTOR MQRPQ,DMCB,(RC)     HFTP FILE TO HUB                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
**********************************************************************          
* ROUTINE TO PUT TO TAPE AND TO DUMP FOR TESTING                     *          
**********************************************************************          
*                                                                               
PUTTPE   NTR1                                                                   
         CLI   QOPT2,C'Y'          DO THEY WANT A TAPE?                         
         JNE   PUTTPE10                                                         
         CLI   RCWRITE,C'N'        DO I WANT TO WRITE TO TAPE?                  
         JE    PUTTPE10                                                         
         PUT   (R6),(R3)           GO TO TAPE                                   
*                                                                               
PUTTPE10 CLI   QOPT7,C'Y'          IS DUMP OPTION 'ON'                          
         JNE   PUTTPEX             --NO GET OUT                                 
         LH    R5,SPRECSZ          RECORD SIZE INTO R5                          
         GOTO1 =V(PRNTBL),DMCB,(3,=C'PUT'),(R3),C'DUMP',(R5),=C'2D'             
*                                                                               
PUTTPEX  J     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*****************************************************************               
* CONSTANTS                                                     *               
*****************************************************************               
*                                                                               
APOW     DC    A(POW)                                                           
ACOM     DC    A(COM)                                                           
*                                                                               
* LITERALS                                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* BANK - DEFAULT BANK ROUTINE                                        *          
**********************************************************************          
*                                                                               
         USING SPECD,R4                                                         
BANK     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   MYBYTE,1                                                         
         BNE   BANK20                                                           
         NI    FLAG,X'FF'-FLGMULTI INIT MULTI ACCOUNT FLAG                      
         ZAP   DUBAMT,=P'0'                                                     
         ZAP   FULCNT,=P'0'                                                     
         ZAP   DUBAMT2,=P'0'                                                    
         ZAP   FULCNT2,=P'0'                                                    
         ZAP   DUBAMT3,=P'0'                                                    
         ZAP   FULCNT3,=P'0'                                                    
         ZAP   FULCNT4,=P'0'                                                    
*DSFTK-135                                                                      
         ZAP   FULCNT5,=P'0'       FULL COUNT OF ALL RECORDS IN FILE            
*DSFTK-135                                                                      
         MVC   CURACCT,SPBACCT     SAVE CURRENT ACCOUNT                         
         MVC   SVBKACCT,SPBACCT                                                 
         MVI   BYTE,THDR           FILE HEADER                                  
         BAS   RE,GORUNIT                                                       
         MVI   BYTE,HDR                                                         
         AP    FULCNT4,=P'1'                                                    
         BAS   RE,GORUNIT                                                       
*        TM    FLAG,FLGSKP                                                      
*        BO    BANKX                                                            
*        BRAS  RE,PUTTPE                                                        
*                                                                               
         L     R7,FORMAT                                                        
BANK10   CLI   0(R7),EOF                                                        
         BE    BANKX                                                            
         CLI   0(R7),HDR                                                        
         BE    *+12                                                             
         AHI   R7,FRMLNQ                                                        
         B     BANK10                                                           
*                                                                               
         USING FRMTABD,R7                                                       
         TM    FRMSTAT,FRMHTCNT    HEADERS/TRAILERS INCLUDED IN CNT             
         BNO   BANKX                                                            
         AP    FULCNT,=P'1'                                                     
         B     BANKX                                                            
         DROP  R7                                                               
*                                                                               
BANK20   CLI   MYBYTE,2            DO THE DETAILS                               
         BNE   BANK50                                                           
         MVC   CURACCT,SPBACCT     SAVE CURRENT ACCOUNT                         
         L     R7,FORMAT                                                        
BANK30   CLI   0(R7),EOF                                                        
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE A DTL                              
         CLI   0(R7),DTL                                                        
         BE    *+12                                                             
         AHI   R7,FRMLNQ                                                        
         B     BANK30                                                           
*                                                                               
         USING FRMTABD,R7                                                       
         TM    FRMSTAT,FRMACC      HEADERS AND TRAILERS BY ACCOUNT              
         BNO   BANK40                                                           
         CLC   SVBKACCT,SPBACCT    DID THE ACCOUNT CHANGE?                      
         BE    BANK40                                                           
         MVI   BYTE,TRL                                                         
         OI    FLAG,FLGMULTI       SET MULTI ACCOUNT FLAG                       
         MVC   CURACCT,SVBKACCT                                                 
         TM    FRMSTAT,FRMHTCNT    HEADERS/TRAILERS INCLUDED IN CNT             
         BNO   *+10                                                             
         AP    FULCNT,=P'1'                                                     
         BAS   RE,GORUNIT                                                       
         MVC   CURACCT,SPBACCT                                                  
         MVC   SVBKACCT,SPBACCT    UPDATE THE NEW ACCOUNT                       
         AP    DUBAMT2,DUBAMT                                                   
         AP    FULCNT2,FULCNT                                                   
         ZAP   DUBAMT,=P'0'                                                     
         ZAP   FULCNT,=P'0'                                                     
         ZAP   DUBAMT3,=P'0'                                                    
         ZAP   FULCNT3,=P'0'                                                    
         TM    FRMSTAT,FRMHTCNT    HEADERS/TRAILERS INCLUDED IN CNT             
         BNO   *+10                                                             
         AP    FULCNT,=P'1'                                                     
         MVI   BYTE,HDR                                                         
         AP    FULCNT4,=P'1'                                                    
         BAS   RE,GORUNIT                                                       
*                                                                               
BANK40   MVI   BYTE,DTL                                                         
         BAS   RE,GORUNIT                                                       
         TM    FLAG,FLGSKP                                                      
         BO    BANKX                                                            
         CP    BKNET,=P'0'         IS IT A VOID?                                
         BH    BANK45                                                           
         AP    FULCNT3,=P'1'       COUNT VOIDS                                  
         AP    DUBAMT3,BKNET       TOTAL VOIDS                                  
         TM    FRMSTAT,FRMEXVD     INCLUDE VOID TOTALS?                         
         BNO   BANK43              YES                                          
         TM    FRMSTAT,FRMIVCT     INCLUDE VOIDS IN COUNT?                      
         BNO   BANK48              NO                                           
         AP    FULCNT,=P'1'                                                     
         B     BANK48                                                           
BANK43   TM    FRMSTAT2,FRMADVD    ADD VOIDS AS POSITIVE?                       
         BNO   BANK45                                                           
         AP    FULCNT,=P'1'        CHECK COUNT                                  
         SP    DUBAMT,BKNET                                                     
         B     BANK48                                                           
BANK45   AP    FULCNT,=P'1'        CHECK COUNT                                  
         AP    DUBAMT,BKNET                                                     
BANK48   B     BANKX                                                            
         DROP  R7                                                               
*                                                                               
BANK50   CLI   MYBYTE,3                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R7,FORMAT                                                        
BANK60   CLI   0(R7),EOF                                                        
         BE    BANK70                                                           
         CLI   0(R7),TRL                                                        
         BE    *+12                                                             
         AHI   R7,FRMLNQ                                                        
         B     BANK60                                                           
*                                                                               
         USING FRMTABD,R7                                                       
         TM    FRMSTAT,FRMHTCNT    HEADERS/TRAILERS INCLUDED IN CNT             
         BNO   *+10                                                             
         AP    FULCNT,=P'1'                                                     
         MVI   BYTE,TRL                                                         
         BAS   RE,GORUNIT                                                       
*                                                                               
         TM    FRMSTAT,FRMTOT      TOTAL TRAILER?                               
         BNO   BANK70                                                           
         TM    FRMSTAT,FRMATOT     ALWAYS PASS TOTAL TRAILER RECORD             
         BO    *+12                                                             
         TM    FLAG,FLGMULTI       ARE THERE MULTI ACCS IN THIS RUN             
         BNO   BANK70                                                           
         TM    FRMSTAT,FRMHTCNT    HEADERS/TRAILERS INCLUDED IN CNT             
         BNO   *+10                                                             
         AP    FULCNT,=P'1'                                                     
         MVI   BYTE,TRL2                                                        
         AP    DUBAMT2,DUBAMT                                                   
         AP    FULCNT2,FULCNT                                                   
         ZAP   DUBAMT,DUBAMT2                                                   
         ZAP   FULCNT,FULCNT2                                                   
         BAS   RE,GORUNIT                                                       
         DROP  R7                                                               
*                                                                               
BANK70   TM    SPSTAT,SPPOSEQ      ARE WE POSPAY?                               
         BO    *+12                                                             
         TM    SPSTAT,SPPOSCOM     ARE WE COMPRESSING?                          
         BNO   BANK110                                                          
         TM    FLAG,FLGREC         ARE WE USING BANK RECORD?                    
         BO    BANK110                                                          
         USING LOCTABD,RE                                                       
         L     RE,=A(LOCTAB)       CHECK TO SEE IF WE ARE POSPAY                
BANK80   CLI   0(RE),EOF                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         ICM   R1,15,LOCFRM                                                     
         C     R1,FORMAT                                                        
         BNE   BANK90                                                           
         CLC   LOCID,SPACES        ANY AGENCY OVERRIDE                          
         BE    BANK100                                                          
         CLC   LOCID,ALPHAID       MATCH ON AGENCY                              
         BE    BANK100                                                          
BANK90   AHI   RE,LOCTBLNQ                                                      
         B     BANK80                                                           
*                                                                               
BANK100  MVC   ACCNUM,LOCNUM                                                    
         MVC   USRID,LOCUID                                                     
BANK110  CLOSE ((R6))                                                           
*                                                                               
BANKX    J     EXIT                                                             
         DROP  R4,RE                                                            
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* BUILD GORUNIT BLOCK AND CALL ACGETFORM TO PUT TO OUT PUT LINE      *          
**********************************************************************          
         SPACE 1                                                                
         USING SPECD,R4                                                         
         USING ACBANKD,R7                                                       
GORUNIT  NTR1                                                                   
         L     R7,AGTBBLK          ACGETFORM BLOCK                              
         LR    RE,R7                                                            
         LA    RF,ACBLNQ                                                        
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
*                                                                               
*DSFTK-135                                                                      
         AP    FULCNT5,=P'1'       FULL COUNT OF ALL RECORDS IN FILE            
         ZAP   ACBRECT,FULCNT5                                                  
*DSFTK-135                                                                      
         ZAP   ACBDUBA,DUBAMT      SET UP ACCUMULATORS                          
         ZAP   ACBDUBA2,DUBAMT2                                                 
         ZAP   ACBDUBA3,DUBAMT3                                                 
         ZAP   ACBFULC,FULCNT                                                   
         ZAP   ACBFULC2,FULCNT2                                                 
         ZAP   ACBFULC3,FULCNT3                                                 
         ZAP   ACBFULC4,FULCNT4                                                 
*                                                                               
*DSFTK-135                                                                      
         MVC   ACBSRTE#,SVSRCRTE   SOURCE BANK ROUTING NUMBER                   
*DSFTK-135                                                                      
         ST    R3,ACBOUTR          ADDRESS OF OUTPUT LINE                       
         LA    R1,L'TPREC                                                       
         STH   R1,ACBOUTRL                                                      
         MVC   ACBFRMT,FORMAT      ADDRESS OF FORMAT                            
         MVC   ACBRECLN,SPRECSZ    RECORD SIZE                                  
         MVC   ACBALPHA,ALPHAID    COMPANY ID                                   
         MVC   ACBRMODE,BYTE       GORUNIT MODE                                 
         MVC   ACBCSHAC,SPQACCT    SC ACCOUNT CODE                              
         MVI   ACBFLAG,0                                                        
         TM    SPSTAT,SPCAN        IS THIS CANADIAN?                            
         BNO   *+8                   NO - SKIP                                  
         OI    ACBFLAG,ACBCAN                                                   
         MVC   ACBSBNK#,CURACCT    BANK ACCOUNT NUMBER                          
*                                                                               
         USING NAMELD,RE                                                        
         L     RE,ADCMPNAM                                                      
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q)                                                   
         CHI   R1,L'ACBCPYNM                                                    
         BNH   *+8                                                              
         LA    R1,L'ACBCPYNM                                                    
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACBCPYNM(0),NAMEREC  COMPANY NAME                                
         DROP  RE                                                               
*                                                                               
         L     R5,ARETAREA         R5 = A(DDNAME RETURN AREA)                   
         USING RTARD,R5                                                         
         MVC   ACBFLIDM,RTGEN+1    UPDATE GENERATION NUMBER                     
*                                                                               
         CLI   MYBYTE,2                                                         
         BNE   GORUN10                                                          
         ZAP   ACBNET,BKNET        NET AMOUNT                                   
         ZAP   ACBCD,BKCD          CASH DISCOUNT AMOUNT                         
         MVC   ACBBNKNM,BKBNKNM    SC ACCOUNT NAME                              
         MVC   ACBCHECK,BKCHECK    CHECK NUMBER                                 
         MVC   ACBCUL,BKCUL        CONTRA U/L                                   
         MVC   ACBVULA,BKACCNT     VENDOR ACCOUNT NUMBER                        
         MVC   ACBPAYEE,BKPAYEE    PAYEE NAME                                   
         MVC   ACBTDTE,BKTRNDTE    TRANSACTION DATE                             
         MVC   ACBADTE,BKACTDTE    ACTIVITY DATE                                
         MVC   ACBTTYP,BKTRNTYP    TRANSACTION TYPE                             
         MVC   ACBTSTA,BKTRNSTA    TRANSACTION STATUS                           
         MVC   ACBTANL,BKTRNANL    ANALYSIS                                     
         MVC   ACBADDR(BKADLNQ),BKADDR   PAYEE ADDRESS                          
*                                                                               
GORUN10  NI    FLAG,X'FF'-FLGSKP                                                
         GOTO1 VGETFORM,DMCB,(X'02',(R7)),ADCOMFAC,0                            
*        TM    ACBFLAG,ACBSKP      SHOULD WE SKIP THIS ENTRY?                   
*        BNO   *+12                                                             
*        OI    FLAG,FLGSKP                                                      
*        B     GORUNITX                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,ACBRCNT        RECORD COUNT                                 
*        BZ    GORUNITX                                                         
         BNZ   *+12                                                             
         OI    FLAG,FLGSKP                                                      
         B     GORUNITX                                                         
*                                                                               
         LR    R5,R6                                                            
         SR    RE,RE                                                            
GORUN20  AR    R3,RE                                                            
         STC   R0,BYTE             STORE CURRENT RECORD COUNT                   
*                                                                               
         CLI   QOPT2,C'Y'          DO THEY WANT A TAPE?                         
         JNE   GORUN30                                                          
         CLI   RCWRITE,C'N'        DO I WANT TO WRITE TO TAPE?                  
         JE    GORUN30                                                          
         PUT   (R5),(R3)                                                        
GORUN30  CLI   QOPT7,C'Y'          IS DUMP OPTION 'ON'                          
         JNE   GORUN40             --NO GET OUT                                 
         SR    R2,R2                                                            
         LH    R2,ACBRECLN         RECORD SIZE INTO R2                          
         GOTO1 =V(PRNTBL),DMCB,(3,=C'PUT'),(R3),C'DUMP',(R2),=C'2D'             
*                                                                               
GORUN40  SR    RE,RE                                                            
         LH    RE,ACBRECLN                                                      
         SR    R0,R0                                                            
         IC    R0,BYTE             RECORD COUNT                                 
         BCT   R0,GORUN20                                                       
*                                                                               
GORUNITX J     EXIT                                                             
         DROP  R4,R7                                                            
         EJECT                                                                  
**********************************************************************          
* CONSTANTS/EQUATES                                                  *          
**********************************************************************          
         SPACE 1                                                                
ZERO     DC    20C'0'                                                           
BH       EQU   X'20'               BRANCH ON HIGH                               
BNH      EQU   X'D0'               BRANCH ON NOT HIGH                           
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
         SPACE 1                                                                
ACTTAB   DS    0C                                                               
         DC    C'1M',AL4(CHASE87)                                               
         DC    AL1(EOF)                                                         
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
******************************************************************              
* SUBMIT ADVANTIS JOB TO READER QUEUE                            *              
******************************************************************              
*                                                                               
         USING SPECD,R4                                                         
         USING MASTD,RF                                                         
POW      DS    0H                                                               
         NMOD1 0,**POW***                                                       
         L     RC,0(R1)            RESET RC                                     
         L     RF,ADMASTC                                                       
         L     R4,SVSPECS          RESET SPECS ADDRESS WITH ORIGINAL            
*TPPS                                                                           
* IF WE GET ANOTHER AGENCY ID OVERRIDE WE SHOULD CREATE A TABLE OF              
* ID'S WITH CORRESPONDING ROUTINES FOR JCL OVERRIDES                            
         CLC   ORIGINUM,=H'16557'   TPPS                                        
         BNE   POW005                                                           
         MVC   JOBC2+2(8),MCJOB                                                 
         L     R5,ARETAREA         R5 = A(DDNAME RETURN AREA)                   
         USING RTARD,R5                                                         
         MVC   PUTF4(5),RTGEN                                                   
         MVC   PUTF6(5),RTGEN                                                   
         B     POW120                                                           
         DROP  R5                                                               
                                                                                
*TPPS                                                                           
POW005   MVC   JOBP+2(8),MCJOB     AYNYA57X                                     
         LA    R1,JOBP                                                          
POW010   CLI   0(R1),C' '          REPLACE 57 W/ 5P                             
         BNE   *+6                                                              
         DC    H'0'                IF SPACE DIE - END OF JOB NAME               
         CLC   0(2,R1),=C'57'      FIND 57 IN JOB NAME                          
         BE    *+12                REDO JOB NAME                                
         LA    R1,1(R1)                                                         
         B     POW010                                                           
*                                                                               
         MVC   0(2,R1),=C'5P'       AYNYA5PX - NEW NAME                         
         MVC   TAPEC2,SPDSNNM       ACCTAPE.A57NWB01                            
         TM    FLAG,FLGREC          READING BANK RECORD?                        
         BNO   *+14                                                             
         MVC   SUBJ2,SPCLASS                                                    
         B     POW030                                                           
         MVC   SUBJ2,SPDSNNM+8      A57NWB01                                    
*                                                                               
         L     RE,=A(CLASSEX)       POINT TO CLASS EXCEPTION TABLE              
POW020   CLI   0(RE),X'FF'                                                      
         BE    POW030                                                           
         CLC   SUBJ2,0(RE)          MATCH WITH 8 CHARACTER CLASS                
         BE    *+12                                                             
         LA    RE,L'CLASSEX(RE)                                                 
         B     POW020                                                           
         MVC   SUBJ2,8(RE)          MOVE IN NEW DESIRED CLASS                   
*                                                                               
POW030   MVC   SUBJ4,ACCNUM        UPDATE ACCOUNT NUMBER                        
         MVC   SUBJ5,USRID         UPDATE USER ID                               
*                                                                               
         TM    SPSTAT,SPPOSCOM     DID WE  GO THROUGH COMPRESS STEP             
         BNO   POW040                                                           
         MVC   FILE2,COMPOT2+8     MOVE COMPRESSED ACCTAPE NAME                 
         MVC   FILE3(5),COMPOT4    MOVE FIRST 5 BYTES OF GEN                    
         MVC   TAPEC2,COMPOT2      ACCTAPE.A57??COM                             
         MVC   TAPEC4,COMPOT4      GENERATION NUMBER OF COMPRESSED DSET         
         MVC   SUBJ2,COMPOT2+8                                                  
         B     POW080                                                           
*                                                                               
POW040   DS    0H                                                               
         MVC   AREA,SPACES                                                      
         LA    R1,SVORIGID                                                      
         LHI   RE,L'SVORIGID                                                    
         CLC   SVORIGID,SPACES                                                  
         BH    POW050                                                           
         LA    R1,SVORIGNM                                                      
         LHI   RE,L'SVORIGNM                                                    
         CLC   SVORIGNM,SPACES                                                  
         BNH   POW060                                                           
*                                                                               
POW050   DS    0H                                                               
*DSFTK-201                                                                      
         CLC   ALPHAID,=C'HY'                                                   
         BNE   POW055                                                           
         MVC   FILE2(FILELNQ),SPACES                                            
         MVC   FILE2(9),=C'ARP02FFH.'                                           
         MVC   FILE2+9(4),=C'POS_'                                              
         BCTR  RE,0                DECREMENT LENGTH OF ORIGIN ID                
         MVC   AREA(0),0(R1)       ORIGIN USERID OR NAME                        
         EX    RE,*-6                                                           
         GOTO1 ADSQUASH,DMCB,AREA,L'AREA                                        
         L     RF,DMCB+4              RF=LENGTH OF ID OR NAME                   
         LA    RE,FILE2+13            BUMP PAST POS_                            
         SHI   RF,1                                                             
         EXMVC RF,0(RE),AREA          MOVE ID OR NAME TO PRINT LINE             
         AHI   RF,1                   BUMP BACK UP FOR REAL LENGTH              
         AR    RE,RF                  POINT PAST THE NAME                       
         MVI   0(RE),C'_'                                                       
         AHI   RE,1                                                             
         MVC   0(6,RE),SVDATE         DATE                                      
         MVI   6(RE),C'.'                                                       
         MVC   7(6,RE),SVTIME         TIME                                      
         MVI   13(RE),C'.'                                                      
         L     R5,ARETAREA         R5 = A(DDNAME RETURN AREA)                   
         USING RTARD,R5                                                         
         MVC   14(4,RE),RTGEN+1    UPDATE GENERATION NUMBER                     
         B     POW070                                                           
*                                                                               
POW055   DS    0H                                                               
*DSFTK-201                                                                      
*MN SPEC-36060                                                                  
         CLC   ALPHAID,=C'YG'                                                   
         BNE   POW056                                                           
         MVC   FILE2(FILELNQ),SPACES                                            
         MVC   FILE2(16),=C'ARM02F.RSWYANDR_'                                   
         MVC   FILE2+16(4),=C'POS_'                                             
         BCTR  RE,0                DECREMENT LENGTH OF ORIGIN ID                
         MVC   AREA(0),0(R1)       ORIGIN USERID OR NAME                        
         EX    RE,*-6                                                           
         GOTO1 ADSQUASH,DMCB,AREA,L'AREA                                        
         L     RF,DMCB+4              RF=LENGTH OF ID OR NAME                   
         LA    RE,FILE2+20            BUMP PAST POS_                            
         SHI   RF,1                                                             
         EXMVC RF,0(RE),AREA          MOVE ID OR NAME TO PRINT LINE             
         AHI   RF,1                   BUMP BACK UP FOR REAL LENGTH              
         AR    RE,RF                  POINT PAST THE NAME                       
         MVI   0(RE),C'_'                                                       
         AHI   RE,1                                                             
         MVC   0(6,RE),SVDATE         DATE                                      
         MVI   6(RE),C'.'                                                       
         MVC   7(6,RE),SVTIME         TIME                                      
         MVI   13(RE),C'.'                                                      
         L     R5,ARETAREA         R5 = A(DDNAME RETURN AREA)                   
         USING RTARD,R5                                                         
         MVC   14(4,RE),RTGEN+1    UPDATE GENERATION NUMBER                     
         B     POW070                                                           
*                                                                               
POW056   DS    0H                                                               
*MN SPEC-36060                                                                  
         MVC   FILE2(FILELNQ),SPACES                                            
         MVC   FILE2(4),=C'POS_'                                                
         BCTR  RE,0                                                             
         MVC   AREA(0),0(R1)       ORIGIN USERID OR NAME                        
         EX    RE,*-6                                                           
         GOTO1 ADSQUASH,DMCB,AREA,L'AREA                                        
         L     RF,DMCB+4              RF=LENGTH OF ID OR NAME                   
         LA    RE,FILE2+4             BUMP PAST POS_                            
         SHI   RF,1                                                             
         EXMVC RF,0(RE),AREA          MOVE ID OR NAME TO PRINT LINE             
         AHI   RF,1                   BUMP BACK UP FOR REAL LENGTH              
         AR    RE,RF                  POINT PAST THE NAME                       
         MVI   0(RE),C'_'                                                       
         AHI   RE,1                                                             
         MVC   0(6,RE),SVDATE         DATE                                      
         MVI   6(RE),C'.'                                                       
         MVC   7(6,RE),SVTIME         TIME                                      
         MVI   13(RE),C'.'                                                      
         L     R5,ARETAREA         R5 = A(DDNAME RETURN AREA)                   
         USING RTARD,R5                                                         
         MVC   14(4,RE),RTGEN+1    UPDATE GENERATION NUMBER                     
*        MVC   EXT(7),=C'EXT=TXT'                                               
         B     POW070                                                           
*                                                                               
POW060   MVC   FILE2,SPDSNNM+8     MOVE IN ACCTAPE NAME W/O VER                 
*                                                                               
*ITMF-7166                                                                      
         CLC   ALPHAID,=C'MI'                                                   
         BNE   POW061                                                           
         CLC   SPDSNNM+8(8),=C'A57MIPOA'                                        
         BNE   POW069                                                           
         MVC   EXT(12),=C'EXT=JOAAPKI1'                                         
         B     POW069                                                           
                                                                                
POW061   CLC   ALPHAID,=C'PY'                                                   
         BNE   POW062                                                           
         CLC   SPDSNNM+8(8),=C'A57PYPOA'                                        
         BNE   POW069                                                           
         MVC   EXT(12),=C'EXT=JOAAPKI2'                                         
         B     POW069                                                           
                                                                                
POW062   CLC   ALPHAID,=C'DN'                                                   
         BNE   POW063                                                           
         CLC   SPDSNNM+8(8),=C'A57DNPOA'                                        
         BNE   POW062A                                                          
         MVC   EXT(12),=C'EXT=JOAAPKI3'                                         
         B     POW069                                                           
POW062A  CLC   SPDSNNM+8(8),=C'A57DNPOC'                                        
         BNE   POW069                                                           
         MVC   EXT(12),=C'EXT=JOAAPKI9'                                         
         B     POW069                                                           
                                                                                
POW063   CLC   ALPHAID,=C'OU'                                                   
         BNE   POW064                                                           
         CLC   SPDSNNM+8(8),=C'A57OUPOA'                                        
         BNE   POW069                                                           
         MVC   EXT(12),=C'EXT=JOAAPKI4'                                         
         B     POW069                                                           
                                                                                
POW064   CLC   ALPHAID,=C'CT'                                                   
         BNE   POW065                                                           
         CLC   SPDSNNM+8(8),=C'A57CTPOS'                                        
         BNE   POW064A                                                          
         MVC   EXT(12),=C'EXT=JOAAPKI8'                                         
         B     POW069                                                           
POW064A  CLC   SPDSNNM+8(8),=C'A57CTPOA'                                        
         BNE   POW069                                                           
         MVC   EXT(12),=C'EXT=JOAAPKI6'                                         
         B     POW069                                                           
                                                                                
POW065   CLC   ALPHAID,=C'CY'                                                   
         BNE   POW069                                                           
         CLC   SPDSNNM+8(8),=C'A57CYPOA'                                        
         BNE   POW069                                                           
         MVC   EXT(12),=C'EXT=JOAAPKI7'                                         
         B     POW069                                                           
                                                                                
*ITMF-7166                                                                      
POW069   L     R5,ARETAREA         R5 = A(DDNAME RETURN AREA)                   
         USING RTARD,R5                                                         
         MVC   FILE3(5),RTGEN      MOVE 1ST 5 BYTES OF GEN                      
POW070   MVC   TAPEC4,RTGEN        UPDATE GENERATION NUMBER                     
*                                                                               
         DROP  R5                                                               
*                                                                               
POW080   MVC   FULL,FORMAT                                                      
         TM    FLAG,FLGFRM         ARE WE RUNNING UNDER A FORMAT?               
         BO    *+10                                                             
         MVC   FULL,SPROUTE                                                     
         TM    FLAG,FLGREC         ARE WE USING BANK RECORD?                    
         BZ    *+20                                                             
         MVC   EDICT2,SVTRNKY     MOVE CORRECT DODS??? CHARGE TYPE              
         MVC   SUBJ3,SVCHRG                                                     
         B     POW120                                                           
*                                                                               
         USING DODSTABD,R1                                                      
         L     R1,=A(DODSTAB)                                                   
POW090   CLI   DODSID,X'FF'                                                     
         BE    POW120                                                           
         CLC   DODSID,SPAGYCD      DOES ALPHA ID MATCH                          
         BNE   POW100                                                           
         CLC   DODSBNK,FULL        MATCH ON RTN/FRM FOR OVRRD                   
         BE    POW110                                                           
POW100   LA    R1,DODSTABQ(R1)                                                  
         B     POW090                                                           
POW110   MVC   EDICT2,DODSTYP     MOVE CORRECT DODS??? CHARGE TYPE              
         MVI   SUBJ3,C'3'         MAKE CHA(1) TO CHA(3) IN SUBJECT LINE         
         DROP  R1                                                               
*                                                                               
POW120   LA    R0,NUMCRD            NUMBER OF JCL CARDS                         
         LA    R3,JCL                                                           
*TPPS                                                                           
         CLC   ORIGINUM,=H'16557'   TPPS                                        
         BNE   *+12                                                             
         LA    R0,NUMCRD2           NUMBER OF JCL CARDS                         
         LA    R3,JCLTPPS                                                       
*TPPS                                                                           
POW130   MVC   POWJCL,0(R3)                                                     
         CLI   QOPT7,C'P'          DO I WANT TO PRINT JCL?                      
         BNE   POW140              NO - SKIP IT                                 
         MVC   P(L'JCL),0(R3)                                                   
         GOTO1 ACREPORT                                                         
*                                                                               
POW140   CLI   QOPT6,C'N'          DO I WANT TO RUN POS PAY?                    
         BE    POW150                                                           
         CLI   QOPT8,C'N'          FOR INTERNAL COMPRESS TESTING                
         BE    POW150                                                           
         GOTO1 =V(POWWOW),DMCB,=C'PUT',=C'POWER',POWKEY,POWHEAD                 
*                                                                               
POW150   LA    R3,L'JCL(R3)                                                     
         BCT   R0,POW130                                                        
         XMOD1 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* POWWOW CONSTANTS AND STORAGE                                       *          
**********************************************************************          
*                                                                               
JCL      DS    0CL80                                                            
JOBP     DC    C'//PACKA57   JOB   ''POSPAY'',PACK,MSGCLASS=X,'                 
         DC    CL28'COND=((0,NE)),'                                             
         DC    C'*'                                                             
         DC    CL8' '                                                           
*                                                                               
         DC    CL80'//     MSGLEVEL=(1,1),NOTIFY=CCOL'                          
         DC    CL80'//*MAIN CLASS=ADVANTIS'                                     
PROC1    DC    C'//SS  EXEC  BDEDICT '                                          
         DC    CL(80-(*-PROC1))' '                SPACE FILL                    
*                                                                               
         DC    CL80'//EXTSYSIN DD *'                                            
EDICT1   DC    C'EDICTKEY='                                                     
EDICT2   DC    C'DODS030'                                                       
EDICT3   DC    CL(80-(*-EDICT1))' '               SPACE FILL                    
*                                                                               
SUBJ1    DC    C'SUBJECT=CLA('                                                  
SUBJ2    DC    C'A57NWB30'                                                      
         DC    C'),CHA('                                                        
SUBJ3    DC    C'1'                                                             
         DC    C'),ACC('                                                        
SUBJ4    DC    C'MMB0'                                                          
         DC    C'),USE('                                                        
SUBJ5    DC    C'MMB0004'                                                       
         DC    C'),MOD(0),'                                                     
         DC    CL(80-(*-SUBJ1))' '                SPACE FILL                    
*                                                                               
FILE1    DC    C'FILE='                                                         
FILE2    DC    C'A57JWPOS'                                                      
         DC    C'.'                                                             
FILE3    DC    C'G0541'                                                         
FILE4    DC    CL(80-(*-FILE1))' '                                              
FILELNQ  EQU   *-FILE2                                                          
*                                                                               
*EXT      DC    CL80'EXT=ROE'                                                   
EXT      DC    CL80'EXT=TXT'                                                    
*                                                                               
TAPEC1   DC    C'DSN='                                                          
TAPEC2   DC    CL16'ACCTAPE.A57NWB30'                                           
TAPEC3   DC    CL1'.'                                                           
TAPEC4   DC    CL8'G0000000'                                                    
TAPEC5   DC    CL(80-(*-TAPEC1))' '                                             
*                                                                               
NUMCRD   EQU   (*-JCL)/80                                                       
*                                                                               
POWKEY   DC    CL10' '                                                          
POWHEAD  DC    XL8'00'                                                          
POWJCL   DS    CL80                                                             
*                                                                               
*TPPS                                                                           
JCLTPPS  DS    0CL80         OVERRIDE FOR TPPS                                  
JOBC2    DC    CL80'//CCOLJOBT  JOB   ''CHEUK WONG'',MSGLEVEL=(1,1),CONX        
               D=((0,NE)),'                                                     
         DC    CL80'//     MSGCLASS=X,CLASS=A,NOTIFY=CCOL'                      
         DC    CL80'//*MAIN CLASS=A'                                            
         DC    CL80'//STEP0010 EXEC PGM=FTP,REGION=8M'                          
         DC    CL80'//SYSPRINT DD SYSOUT=*'                                     
         DC    CL80'//SYSTCPD DD DSN=DDS.TCPIP.DATA(TCPD1),DISP=SHR'            
         DC    CL80'//INPUT DD *'                                               
         DC    CL80'10.253.32.36'                                               
         DC    CL80'jpmorgan_accpak'                                            
         DC    CL80'a15P24k3'                                                   
         DC    CL80'ascii'                                                      
         DC    CL80'cd /outbound'                                               
*                                                                               
PUTF1    DC    C'PUT '''                                                        
PUTF2    DC    C'ACCTAPE.A57DSPOA'                                              
PUTF3    DC    C'.'                                                             
PUTF4    DC    C'G0000V00'                                                      
PUTF5    DC    C''' A57DSPOA_'                                                  
PUTF6    DC    C'G0000'                                                         
         DC    C'.txt'                                                          
PUTF7    DC    CL(80-(*-PUTF1))' '                                              
*                                                                               
         DC    CL80'QUIT'                                                       
         DC    CL80'/*'                                                         
         DC    CL80'//'                                                         
*                                                                               
NUMCRD2  EQU   (*-JCLTPPS)/80                                                   
*                                                                               
*TPPS                                                                           
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
******************************************************************              
* SUBMIT COMPRESS JOB                                            *              
******************************************************************              
*                                                                               
         USING SPECD,R4                                                         
         USING MASTD,RF                                                         
COM      DS    0H                                                               
         NMOD1 0,**COM***                                                       
         L     RC,0(R1)            RESET RC                                     
         L     RF,ADMASTC                                                       
         L     R4,SVSPECS          RESET SPECS ADDRESS WITH ORIGINAL            
         MVC   JOBC+2(8),MCJOB     AYNYA57X                                     
         LA    R1,JOBC                                                          
COM10    CLI   0(R1),C' '          REPLACE 57 W/ 5C                             
         BNE   *+6                                                              
         DC    H'0'                IF SPACE DIE - END OF JOB NAME               
         CLC   0(2,R1),=C'57'      FIND 57 IN JOB NAME                          
         BE    *+12                REDO JOB NAME                                
         LA    R1,1(R1)                                                         
         B     COM10                                                            
*                                                                               
         MVC   0(2,R1),=C'5C'       AYNYA5CX - NEW NAME                         
*                                                                               
         MVI   SECU3,X'5E'          MOVE IN SEMICOLON                           
         MVI   SECU3MC,X'5E'          MOVE IN SEMICOLON                         
         MVC   COMDSNNM,SPACES      FOR DYNAMIC ALLOCATION                      
         MVC   COMDSNNM,SPDSNNM                                                 
         MVC   COMDSNNM+13(3),=C'COM' CHANGE POS/POC/POA TO COM                 
*                                                                               
         L     R5,ARETAREA         R5 = A(DDNAME RETURN AREA)                   
         USING RTARD,R5                                                         
         MVC   COMPIN2(L'RTDSN),RTDSN      ACCTAPE.A57NWB01                     
         MVC   COMPIN4,RTGEN       UPDATE GENERATION NUMBER                     
         CLC   RTGEN,SPACES                                                     
         BNE   COM40                                                            
*                                                                               
         MVC   COMPIN5(18),SPACES                                               
         LHI   R3,L'RTDSN                                                       
         LA    RE,COMPIN2                                                       
         LA    R1,L'RTDSN-1(RE)    POINT AFTER DSN NAME                         
COM20    CLI   0(R1),C' '          IS IT A SPACE                                
         BNE   COM30                                                            
         BCTR  R1,0                                                             
         BCT   R3,COM20                                                         
         DC    H'0'                NO DSN NAME GENERATED                        
COM30    LA    R1,1(R1)                                                         
         MVC   0(9,R1),=C',DISP=SHR'                                            
*                                                                               
COM40    MVC   DSNME,SPACES                                                     
         MVC   DSNME(6),=C'DATAOT'                                              
         GOTO1 DYNALLOC,DMCB,DSNME,(X'00',COMDSNNM)                             
         LINKX MF=(E,PARMLST),SF=(E,LINKLST)                                    
         LA    R5,RETAREA          GET ADDRESS OF RETAREA                       
         MVC   COMPOT4,RTGEN       GENERAION NUMBER                             
         MVC   COMPOT2(L'RTDSN),RTDSN   ACCTAPE.A57HHCOM./RGUP.CITITEST         
         CLC   RTGEN,SPACES                                                     
         BNE   COM70                                                            
*                                                                               
         MVC   COMPOT5(18),SPACES                                               
         LHI   R3,L'RTDSN                                                       
         LA    RE,COMPOT2                                                       
         LA    R1,L'RTDSN-1(RE)    POINT AFTER DSN NAME                         
COM50    CLI   0(R1),C' '          IS IT A SPACE                                
         BNE   COM60                                                            
         BCTR  R1,0                                                             
         BCT   R3,COM50                                                         
         DC    H'0'                NO DSN NAME GENERATED                        
COM60    LA    R1,1(R1)                                                         
         MVC   0(10,R1),=C',DISP=SHR,'                                          
         DROP  R5                                                               
*                                                                               
COM70    DS    0H                                                               
         EDIT  SPRECSZ,DCB2,ZERO=NOBLANK,FILL=0                                 
         EDIT  SPBLKSZ,DCB3A,ZERO=NOBLANK,FILL=0                                
         CLC   ALPHAID(2),=C'1M'   MCCANN REQUEST                               
         BE    COM78               NO                                           
         CLC   ALPHAID(2),=C'MC'   MCCANN REQUEST                               
         BE    COM78               NO                                           
         CLC   ALPHAID(2),=C'I5'   ICCNJ  REQUEST                               
         BE    COM78               NO                                           
         CLC   ALPHAID(2),=C'M$'   MULLEN REQUEST                               
         BNE   COM80               NO                                           
                                                                                
COM78    MVC   SECU1(80),SECU1MC                                                
COM80    LA    R0,CARDNUM           NUMBER OF JCL CARDS                         
         LA    R3,COMJCL                                                        
COM90    MVC   POWJCLC,0(R3)                                                    
         CLI   QOPT7,C'P'          DO I WANT TO PRINT JCL?                      
         BNE   COM100              NO - SKIP IT                                 
         MVC   P(L'POWJCL),0(R3)                                                
         GOTO1 ACREPORT                                                         
*                                                                               
COM100   CLI   QOPT6,C'N'          DO I WANT TO RUN POS PAY?                    
         BE    COM110                                                           
*                                                                               
         CLI   QOPT8,C'N'          FOR INTERNAL COMPRESS TESTING                
         BE    COM110                                                           
*                                                                               
         GOTO1 =V(POWWOW),DMCB,=C'PUT',=C'POWER',POWKEYC,POWHEADC               
*                                                                               
COM110   LA    R3,L'POWJCLC(R3)                                                 
         BCT   R0,COM90                                                         
         GOTO1 ACREPORT                                                         
         XMOD1 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* COMPRESS JCL'S CONSTANTS AND STORAGE                               *          
**********************************************************************          
*                                                                               
COMJCL   DS    0CL80                                                            
JOBC     DC    C'//PACKA57   JOB ,''USER      '',CLASS=A,MSGCLASS=X,'           
         DC    CL22'MSGLEVEL=(1,1),'                                            
         DC    C'*'                                                             
         DC    CL8' '                                                           
*                                                                               
         DC    CL80'//     NOTIFY=CCOL'                                         
         DC    CL80'//*MAIN CLASS=SAS'                                          
         DC    CL80'//SS  EXEC  COMPRES1'                                       
*                                                                               
         DC    CL80'//SECFILE  DD *'                                            
SECU1    DC    C'     SENDER(DONOVAN DATA) RECEIVER(CFS) '                      
         DC    C'TRANSACTION(*)'                                                
SECU3    DC    C' '                                                             
         DC    CL(80-(*-SECU1))' '                                              
*                                                                               
COMPIN1  DC    C'//DATAIN  DD DSN='                                             
COMPIN2  DC    CL16'RGUP.A57TAPE4'                                              
         DC    C'.'                                                             
COMPIN4  DC    CL8'G0000000'                                                    
COMPIN5  DC    C',DISP=SHR'                                                     
         DC    CL(80-(*-COMPIN1))' '                                            
*                                                                               
COMPOT1  DC    C'//DATAOT  DD DSN='                                             
COMPOT2  DC    CL16'ACCTAPE.A57**COM'                                           
COMPOT3  DC    C'.'                                                             
COMPOT4  DC    CL8'G0000000'                                                    
COMPOT5  DC    C',DISP=(NEW,CATLG),'                                            
         DC    CL(80-(*-COMPOT1))' '                                            
         DC    CL80'//           UNIT=SYSDA,SPACE=(CYL,(1,1),RLSE),'            
*                                                                               
DCB1     DC    C'//           DCB=(LRECL='                                      
DCB2     DC    CL3'200'                                                         
DCB3     DC    C',BLKSIZE=02000,RECFM=FB)'                                      
         ORG   DCB3+9                                                           
DCB3A    DC    CL5'00000'                                                       
         ORG                                                                    
DCB4     DC    CL(80-(*-DCB1))' '                                               
*                                                                               
CARDNUM  EQU   (*-COMJCL)/80                                                    
SECU1MC  DC    C'     SENDER(DONOVAN DATA) RECEIVER(CITIBANKDEL) '              
         DC    C'TRANSACTION(*)'                                                
SECU3MC  DC    C' '                                                             
         DC    CL(80-(*-SECU1MC))' '                                            
POWKEYC  DC    CL10' '                                                          
POWHEADC DC    XL8'00'                                                          
POWJCLC  DS    CL80                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* FILTER ROUTINE - DETERMINE IF WE WANT THIS COMPANY                 *          
*      AT ENTRY - R2 = ABKBUF (ADDRESS OF BUFFER RECORD FROM SORT)   *          
**********************************************************************          
*                                                                               
FILTER   DS    0H                                                               
         NMOD1 0,**FILT**                                                       
         L     RC,0(R1)            RESET RC                                     
*        XC    FLAG,FLAG                                                        
*                                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         MVC   VMQRPT,MCVMQRPT   MQ INTERFACE                                   
*                                                                               
         USING SSOOFF,RE         CREATE AN EDICT HEADER OR MQ                   
         ICM   RE,15,MCSSB       HEADER                                         
         JZ    FILTRG2           NO DETAILS PRESENT                             
*                                                                               
         CLI   SSODSPAC,C'C'     CSC REGION ?                                   
         JE    FILTRG1           YES, SET FILE AS A TEST                        
*                                NO ,                                           
         CLI   SSODSPAC,C'Q'     FQA REGION ?                                   
         JE    FILTRG1           YES, SET FILE AS A TEST                        
*                                NO ,                                           
         CLI   SSODSPAC,C'T'     TST REGION ?                                   
         JNE   FILTRG2           NO , MUST BE PRODUCTION                        
*                                YES,                                           
FILTRG1  DS    0H                                                               
         MVC   DYDDSN2,=CL5'TEST.' MARK IT AS A TEST DATASET                    
         DROP  RF,RE                                                            
*                                                                               
FILTRG2  DS    0H                                                               
*                                                                               
         USING SPECD,R4                                                         
         L     R4,ASPCBLK                                                       
         OC    0(SPECFLNQ,R4),0(R4)                                             
         BZ    FILT01                                                           
         OI    FLAG,FLGFRM                                                      
         B     FILT60                                                           
*                                                                               
FILT01   MVC   SVBKACT,BKBANK                                                   
         CLI   MYBYTE,1            CALL FROM FIRST PROCACC                      
         BNE   FILT05              NO, ENTER LOOP                               
*        L     R3,ADACC                                                         
*        MVC   SVBKACT,3(R3)                                                    
         MVC   SVBKACT,0(R7)                                                    
*                                                                               
FILT05   L     R4,=A(ACCSPECS)                                                  
         USING TABSPECD,R1                                                      
         L     R1,=A(TABSPECS)     BANK ROUTINE TABLE                           
FILT10   CLI   0(R1),EOF           AGENCY RUNS OFF BANK ROUTINES                
         BE    FILT30                                                           
         CLC   ALPHAID,TABALPHA    MATCH ON ALPHA ID                            
         BNE   FILT20                                                           
         CLC   TABBACCT,SVBKACT    MATCH ON BANK ACCOUNT                        
         BNE   FILT20                                                           
         TM    TABSTAT,TABFRM      IS THIS ENTRY ON FORMATS                     
         BNO   FILT30                                                           
         L     R4,=A(FRMSPECS)     FORMAT TABLE                                 
         OI    FLAG,FLGFRM         SET BANK IS FORMAT BIT                       
         B     FILT30                                                           
FILT20   AHI   R1,TABSPECQ                                                      
         B     FILT10                                                           
*                                                                               
FILT30   CLI   MYBYTE,1            CALL FROM FIRST PROCACC                      
         BNE   FILT50              NO, ENTER LOOP                               
         LA    R2,SAVEBANK         SPACE TO BUILD THE ACCOUNT                   
*        L     R3,ADACC                                                         
*        MVC   BKBANK,3(R3)        SAVE FIRST ACCOUNT NUMBER                    
         MVC   BKBANK,0(R7)        SAVE FIRST ACCOUNT NUMBER                    
*                                                                               
         MVI   TALVOID,C'N'                                                     
         CLC   ALPHAID(2),=C'DS'   TALENT PARTNERS REQ                          
         BNE   FILT40              NO                                           
         CLI   QOPT5,C'V'          VOIDS ONLY RUN?                              
         BNE   FILT40              NO                                           
         MVI   TALVOID,C'Y'                                                     
         B     FILT50                                                           
*                                                                               
FILT40   CLC   ALPHAID(2),=C'OM'   OGILVY REQ                                   
         BNE   FILT50              NO                                           
         CLI   QOPT5,C'O'          MORGAN CHECKS ONLY RUN                       
         BNE   FILT50              NO                                           
         CLC   BKBANK(5),=C'B001 ' SPOT CHECKS REQUEST                          
         BNE   FILT50              NO                                           
         MVI   TALVOID,C'Y'                                                     
*                                                                               
FILT50   CLI   0(R4),X'FF'                                                      
         BNE   *+12                NO MATCH                                     
         OI    FLAG,FLGTPE         SET BYTE TO NO                               
         B     FILTX                                                            
*                                                                               
         CLC   SPAGYCD,ALPHAID     COMPANY MATCH                                
         BNE   FILT110                                                          
         CLC   SPQACCT,SPACES      TABLE ACCOUNT SPACES FOR ALL                 
         BE    FILT60                                                           
         CLI   TALVOID,C'Y'                                                     
         BNE   *+8                                                              
         MVI   BKBANK+4,C'V'                                                    
         CLC   SPQACCT,BKBANK      REQUESTED ACCT- FROM SORT OR PROCACC         
         BNE   FILT110                                                          
*                                                                               
FILT60   CLI   QOPT9,C'T'          IS THIS A TEST RUN?                          
         BNE   *+12                                                             
         TM    SPSTAT,SPTSTFRM     X'08'-IS THIS A TEST FORMAT?                 
         BZ    FILT110             NO SKIP NEXT COMMAND                         
         TM    SPSTAT,SPPOSEQ      X'80'-IS THIS A POS PAY ACCOUNT?             
         BZ    FILT70              NO SKIP NEXT COMMAND                         
         CLI   QOPT7,C'N'          NON POSPAY RUN REQUESTED?                    
         BNE   *+12                                                             
         BAS   RE,NOPOS            CHECK IF POSPAY OVERRIDE AVAIL               
         BE    *+8                                                              
         OI    FLAG,FLGPOS         TURN ON POS PAY INDICATOR                    
*                                                                               
FILT70   CLI   MYBYTE,2            IS THIS CALL ON CHANGE OF ACCOUNT            
         BNE   FILT80              NO, SET UP DCB                               
         ST    R4,SPECS            RESET SPECS ADDRESS                          
         CLI   TALVOID,C'Y'                                                     
         BNE   *+8                                                              
         MVI   BKBANK+4,C' '                                                    
         SR    R2,R2                                                            
         ICM   R2,15,SPROUTE       ADDRESS OF THE ROUTINE                       
         L     R1,SVSPECS          ALWAYS CHECK ORIGINAL                        
         ICM   R3,15,SPROUTE-SPECD(R1)                                          
         CR    R3,R2               STILL USING THE SAME ROUTINE                 
         BE    FILTX               YES                                          
         OI    FLAG,FLGSWCH        NO, ERROR                                    
         B     FILTX                                                            
*                                                                               
FILT80   MVC   MYDTF+40(7),SPDTFNM                                              
         CLC   MYDTF+42(5),SPACES                                               
         BNE   *+10                                                             
         MVC   MYDTF+42(5),QACCOUNT                                             
*                                                                               
         LH    R3,SPBLKSZ                                                       
         LTR   R3,R3                                                            
         BZ    FILT90              NOT TAPE OUTPUT                              
         STH   R3,MYDTF+62         DTF BLOCKSIZE                                
         LH    R3,SPRECSZ                                                       
         STH   R3,MYDTF+82         RECORD LENGTH                                
         LA    R6,MYDTF                                                         
         MVC   DSNME,SPACES        PAD DD NAME WITH SPACES                      
         MVC   DSNME(7),SPDTFNM                                                 
*                                                                               
FILT82   DS    0H                                                               
         CLI   SVTYP,TYPMQ         DO WE WANT TO SEND MESSAGES TO MQ ?          
         JNE   FILT88              NO , PROCESS AS A TAPE FILE                  
*                                  YES,                                         
         MVC   DYDDTE,SVDATE       SAVE DATE DETAILS                            
         MVC   DYDTIM,SVTIME       SAVE TIME DETAILS                            
         MVC   DYDDSN5,SVDSN       SAVE DSN DETAILS                             
*                                                                               
* REMOVE SPACES FROM THE DETAILS                                                
*                                                                               
         MVC   WORK(DSNDSLNQ),DYDDSN3  SAVE AT TEMP WORK AREA                   
         MVC   DYDDSN3(DSNDSLNQ),SPACES CLEAR DATA AREA                         
         LA    R0,DSNDSLNQ         LEANGTH OF THE DATA AREA                     
         LA    RE,DYDDSN3          POINT START OF THE DATA AREA                 
         LA    RF,WORK             POINT TEMP WORK AREA                         
*                                                                               
FILT84   DS    0H                                                               
         CLI   0(RF),X'40'         SPACES ?                                     
         JNH   FILT86              YES, PROCESS NEXT BYTE                       
*                                  NO ,                                         
         MVC   0(1,RE),0(RF)       MOVE DATA                                    
         AHI   RE,1                POINT NEXT BYTE FROM DATA AREA               
*                                                                               
FILT86   DS    0H                                                               
         AHI   RF,1                POINT NEXT BYTE FROM TEMP WORK AREA          
         JCT   R0,FILT84           CHECK NEXT BYTE                              
*                                                                               
* DYNAMICALLY ALLOCATE THE FILE NAME                                            
*                                                                               
         LA    R5,DYDDSN           POINT DATASET NAME DETAILS                   
         MVI   BYTE,X'45'              CYLINDER (X'40')                         
*                                      CL44 DSN (X'04')                         
*                                      3RD PARM (X'01')                         
         MVC   DUB,=X'000005000001'    PRI=5,SEC=1                              
*                                                                               
         GOTO1 DYNALLOC,DMCB,(X'80',DSNME),(BYTE,DUB),(X'80',(R5))              
         J     FILT89              CONTINUE                                     
*                                                                               
FILT88   GOTO1 DYNALLOC,DMCB,(X'02',DSNME),SPDSNNM   GENERATE DD ST.            
*                                                                               
FILT89   DS    0H                                                               
         OPEN  ((R6),(OUTPUT))                                                  
         LINKX MF=(E,PARMLST),SF=(E,LINKLST)                                    
         LA    R1,RETAREA          GET ADDRESS OF RETAREA                       
         ST    R1,ARETAREA         SAVE OFF ADDRESS                             
*                                                                               
FILT90   CLI   TALVOID,C'Y'        DID I CHANGE BKBANK?                         
         BNE   *+8                 NO                                           
         MVI   BKBANK+4,C' '       YES, CHANGE IT BACK                          
         SR    R2,R2                                                            
         ICM   R2,15,SPROUTE       ADDRESS OF THE ROUTINE                       
         ST    R2,BRANCH                                                        
*        TM    FLAG,FLGFRM         IS THIS ACC A FORMAT IN FRMSPECS?            
*        BO    FILT92                                                           
*        TM    SPSTAT,SPFRM        IS THIS ACC A FORMAT IN ACCSPECS?            
*        BO    FILT92                                                           
*        CR    R2,R4               NO - MAKE SURE TABLE IN SYNC                 
*        BL    FILT94              ONLY FORMATS ARE > SPECS                     
*        DC    H'0'                                                             
FILT92   MVC   FORMAT,BRANCH       BRANCH IS REALLY FORMAT                      
         MVC   BRANCH,=A(BANK)     BRANCH NOW IS JUST THE DEFAULT               
FILT94   ST    R4,SPECS                                                         
         MVC   SVSPECS,SPECS       SAVE ORIGINAL SPECS ADDRESS FOR POW          
         B     FILTX                                                            
*                                                                               
FILT110  LA    R4,SPECLNQ(R4)                                                   
         B     FILT50                                                           
*                                                                               
FILTX    XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* CHECK TABLE FOR 2ND ENTRY IF NO POSPAY TRANSMISSION IS REQUESTED   *          
*       R2 = ABKBUF (ADDRESS OF BUFFER RECORD FROM SORT)             *          
*       R4 = A(SPEC TABLE - ACCSPECS)                                *          
**********************************************************************          
*                                                                               
         USING SPECD,R4                                                         
NOPOS    NTR1                                                                   
         LA    R4,SPECLNQ(R4)      BUMP TO NEXT TABLE ENTRY                     
*                                                                               
NOP10    CLI   0(R4),X'FF'                                                      
         BE    XITNO               NO ADDITIONAL MATCHES                        
         CLC   SPAGYCD,ALPHAID     COMPANY MATCH                                
         BNE   *+14                                                             
         CLC   SPQACCT,BKBANK      REQUESTED ACCT- FROM SORT OR PROCACC         
         BE    XITYES                                                           
*                                                                               
         LA    R4,SPECLNQ(R4)      BUMP TO NEXT TABLE ENTRY                     
         B     NOP10                                                            
*                                                                               
XITNO    SR    R1,R1                                                            
         CR    RC,R1               SET BRANCH TO NOT EQUAL                      
         XIT1                                                                   
*                                                                               
XITYES   CR    RC,RC               SET BRANCH TO EQUAL AND SAVE R4              
         XIT1  REGS=(R4)                                                        
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* LINK CONSTANTS                                                     *          
**********************************************************************          
*                                                                               
PARMLST  CALL  ,(DSNME,RETAREA),MF=L                                            
LINKLST  LINKX EP=FRGETDSN,SF=L                                                 
         EJECT                                                                  
**********************************************************************          
* LOCAL STORAGE                                                      *          
**********************************************************************          
*                                                                               
DSNME    DS    CL8                 DDNAME FOR LINKAREA                          
RETAREA  DS    CL44                RETURN AREA FOR DSN W/GENERATION #           
COMDSNNM DS    CL20                FOR DYNAMIC ALLOCATION                       
*                                                                               
SVBKACT  DS    CL12                SAVED AREA FOR BANK ACCOUNT                  
AREA     DS    CL(L'SVORIGNM)                                                   
         EJECT                                                                  
**********************************************************************          
* DCB                                                                *          
**********************************************************************          
*                                                                               
         PRINT GEN                                                              
MYDTF    DCB   DDNAME=MYDTF,           DOS SYS004                      X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00050,                                            X        
               BLKSIZE=00100,          DOS BLKSIZE=00100               X        
               MACRF=PM                                                         
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*********************************************************************           
* WRITE OUT MQ HEADER TO THE MQ FILE                                *           
*********************************************************************           
MQRPQ    NMOD1 0,**MQRP**                                                       
         L     RC,0(R1)            RESET RC                                     
*                                  CONTINUE                                     
         GOTO1 VMQRPT,DMCB,(0,=C'OPEN'),(0,MQFILID),(X'E0',0),0                 
*                                                                               
         CLI   DMCB+8,0            MQ OPEN FAILS ?                              
         JE    MQRPQ00             NO, CONTINUE                                 
*                                  YES ,                                        
         MVC   AUTOREAS,OPENFAIL   SEND EMAIL NOTIFICATION AND DUMP             
         J     MQERREND                                                         
*                                                                               
MQRPQ00  DS    0H                                                               
         MVC   MQMDSN(DYRTLNQ),SPACES CLEAR WORK AREA                           
         MVC   MQMRECTY,SVTRNKY    MQ MESSAGE (TRANS KEY FROM BANK REC)         
         MVC   MQMDSN(DSNDSLNQ),DYDDSN3 MOVE DATASET NAME                       
*                                                                               
         MVC   MQDATE,DYDDTE-2     DSECT DSNMD IS NOT IN SYNC WITH              
         MVC   MQTIME,DYDTIM-2     DYDDSN - TWO OFF                             
*                                                                               
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),MQMESS,MQMLNQ,0                          
*                                                                               
         CLI   DMCB+8,0            MQ PUT FAILS ?                               
         JE    MQRPQ02             NO, CONTINUE                                 
*                                  YES,                                         
         MVC   AUTOREAS,PUTFAIL    SEND EMAIL NOTIFICATION                      
         J     MQERREND            - AND DUMP                                   
*                                                                               
MQRPQ02  DS    0H                                                               
         GOTO1 VMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0            IF MQ PUT FAILS                              
*                                                                               
         JE    MQRPQX                                                           
         MVC   AUTOREAS,CLOSFAIL                                                
*                                                                               
MQERREND GOTO1 DATAMGR,DMCB,=C'OPMSG',(=AL1(AUTOLENG),AUTONOTE)                 
         DC    H'00'                                                            
*                                                                               
MQRPQX   XIT1                                                                   
*********************************************************************           
* MQ CONSTANTS                                                      *           
*********************************************************************           
MQFILID  DC    CL16'POSPAY**********'                                           
AUTONOTE DC    C'AUTONOTE*MNAS,JSHA,ABID'                                       
AUTOREAS DS    CL15                                                             
AUTOLENG EQU   *-AUTONOTE                                                       
OPENFAIL DC    CL(L'AUTOREAS)'MQ OPEN FAILED'                                   
PUTFAIL  DC    CL(L'AUTOREAS)'MQ PUT ERROR'                                     
CLOSFAIL DC    CL(L'AUTOREAS)'MQ CLOSE FAILED'                                  
SETUFAIL DC    CL(L'AUTOREAS)'NOT SETUP TO MQ'                                  
*********************************************************************           
* MQ TABLES                                                         *           
*********************************************************************           
MQMESS   DS    0C                                                               
         DC    CL6'DANOT1'         RECORD TYPE                                  
         DC    CL3'ACC'            MQ KEY                                       
MQMRECTY DC    CL4'TEST'                                                        
         DC    CL8'BILLING '                                                    
         DC    CL8'        '                                                    
MQDATE   DC    CL6' '                                                           
MQTIME   DC    CL6' '                                                           
         DC    CL64' '             EMPTY                                        
MQMDSN   DC    CL128' '            DATASET NAME                                 
MQMLNQ   EQU   *-MQMESS                                                         
         DC    AL1(EOF)                                                         
**********************************************************************          
* CONSTANTS                                                          *          
* THIS WILL BE THE DEFAULT DATASET NAME IF THERE IS NO OVERRIDE FOUND*          
* ON THE AFM/BANK RECORD.                                            *          
**********************************************************************          
*                                                                               
DYDDSN   DS    0C                                                               
DYDDSN1  DC    CL9'SFTPDISK.'                                                   
DYDDSN2  DC    CL5'PROD.'          PROD OR TEST BASED ON THE RUN                
DYDDSN3  DC    CL1'E'              E=EDI                                        
DYDDSN4  DC    CL4'POS.'           TRNSMISSION TYPE                             
DYDDSN5  DC    CL10'UUUUUUUU.L'    U'S=USER ID,L=LEDGER                         
         DC    C'.'                .                                            
         DC    C'D'                D PREFIX FOR TODAY'S DATE                    
DYDDTE   DC    CL6' '              TODAY'S DATE W/  D PREFIX                    
         DC    C'.'                .                                            
         DC    C'T'                T PREFIX FOR CURRENT TIME                    
DYDTIM   DC    CL7' '              CURRENT TIME                                 
*                                                                               
DSNDSLNQ EQU   *-DYDDSN3           LENGTH W/O DSN CONSTANTS                     
DSNLNQ   EQU   *-DYDDSN            TOTAL LENGTH                                 
*                                                                               
DYRTDSN  DC    CL25' '                                                          
DYRTGEN  DC    CL8' '        G0001V00                                           
DYRTND   DC    CL19' '       SPARE                                              
DYRTLNQ  EQU   *-DYRTDSN                                                        
*                                                                               
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* COMPANY TABLE - ACCOUNT SPECS                                      *          
* (10/96) DUE TO POSPAY CONFIGURATIONS THERE MAY BE TWO ENTRIES IN   *          
*         TABLE FOR THE SAME ACCPAK ACCOUNT.  THE DEFAULT(OR FIRST)  *          
*         TABLE ENTRY WILL BE THE POSPAY ENTRY.  THE SECOND ENTRY    *          
*         WILL ONLY BE USED IF QOPT7=N FOR NON POSPAY RUN            *          
**********************************************************************          
*                                                                               
ACCSPECS CSECT                                                                  
         DS    0CL(SPECLNQ)                                                     
       ++INCLUDE AC57TAB                                                        
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER DODSTABLE CHARGE OVERRIDE TABLE                     *          
**********************************************************************          
DODSTABD DSECT                                                                  
DODSID   DS    CL2                 COMPANY ALPHA ID                             
DODSBNK  DS    AL4                 BANK INVOLVED WITH CHARGE                    
DODSTYP  DS    CL7                 DODS??? OVERRIDE CHARGE TYPE                 
DODSTABQ EQU   *-DODSTABD                                                       
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER TABSPEC TABLE                                       *          
**********************************************************************          
TABSPECD DSECT                                                                  
TABALPHA DS    CL2                 COMPANY ALPHA ID                             
TABBACCT DS    CL12                BANK ACCOUNT                                 
TABSTAT  DS    XL1                 STATUS BYTE                                  
TABFRM   EQU   X'80'               BANK RUNNING ON FORMAT                       
TABSPECQ EQU   *-TABSPECD                                                       
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER ACTIVITY DATE OVERRIDE TABLE                        *          
**********************************************************************          
         SPACE 1                                                                
ACTTBD   DSECT                                                                  
ACTALPHA DS    CL2                 COMPANY ALPHA ID                             
ACTFRM   DS    AL4                 FORMAT                                       
ACTTBLNQ EQU   *-ACTTBD                                                         
         EJECT                                                                  
**********************************************************************          
* DDNAME RETURN AREA DSECT                                           *          
**********************************************************************          
*                                                                               
RTARD    DSECT                                                                  
RTDSN    DS    CL17                DSN NAME (ACCTAPE.A57BSB03.)                 
RTGEN    DS    CL8                 GENERATION NUMBER (G0046V00)                 
RTSPRE   DS    CL19                SPARE                                        
         EJECT                                                                  
**********************************************************************          
* TABLE DSECT                                                        *          
**********************************************************************          
*                                                                               
* LOCTABD                                                                       
       ++INCLUDE ACLOCTBD                                                       
         EJECT                                                                  
**********************************************************************          
* INCLUDE COUNTRY EQUS                                               *          
**********************************************************************          
*                                                                               
* DDCTRYEQUS                                                                    
       ++INCLUDE DDCTRYEQUS                                                     
         EJECT                                                                  
**********************************************************************          
* INCLUDE BIGPRINT                                                   *          
**********************************************************************          
*                                                                               
* ACBIGPRNTD                                                                    
       ++INCLUDE ACBIGPRNTD                                                     
         EJECT                                                                  
**********************************************************************          
* INCLUDE FASSBOFF                                                   *          
**********************************************************************          
* FASSBOFF                                                                      
       ++INCLUDE FASSBOFF                                                       
**********************************************************************          
* INCLUDE ACGETFORM                                                  *          
**********************************************************************          
*                                                                               
* ACGETFORMD                                                                    
       ++INCLUDE ACGETFORMD                                                     
**********************************************************************          
* INCLUDE AC57D GLOBAL STORAGE                                       *          
**********************************************************************          
*                                                                               
* AC57D                                                                         
       ++INCLUDE AC57D                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'181ACREP5703 02/12/20'                                      
         END                                                                    
