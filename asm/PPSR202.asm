*          DATA SET PPSR202    AT LEVEL 010 AS OF 10/15/12                      
*PHASE T42102A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T42102- ENHANCED SPACE DESCRIPTION - BUY READ'                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 10/12/12 ESR BY ESTIMATE PERIOD - BYPASS DELETED INSERTIONS              
*                                                                               
* KWAN 03/22/11 ESR BY ESTIMATE PERIOD                                          
*                                                                               
* KWAN 03/09/11 ALLOW ESR BY ESTIMATE OR BY MOS IN THE SAME MONTH               
*                                                                               
* KWAN 01/24/11 CHECK LAST ESR NUMBER AGAINST CURRENT EST NUMBER                
*                                                                               
* KWAN 04/11/06 STEWARDSHIP SPACE RESERVATION                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT NOGEN                                                            
T42102   CSECT                                                                  
         NMOD1 PPSR202X-PPSR202D,T42102,RR=R2                                   
*                                                                               
         LR    R6,RC                                                            
         USING PPSR202D,R6                                                      
*                                                                               
         ST    R2,RELO02                                                        
*                                                                               
         L     RC,0(R1)                                                         
         USING POLWRKD,RC                                                       
         USING ESRWORKD,R8                                                      
         LA    R7,1(R9)                                                         
         LA    R7,4095(R7)                                                      
         USING POLFILE,R9,R7                                                    
         USING T421FFD,RA                                                       
*                                                                               
         XC    ERR(2),ERR          FOR ERROR NUMBER                             
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         CLI   RCWRITE,C'Y'        UPDATING RECORDS?                            
         BNE   *+8                                                              
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
*                                                                               
         OC    KEY,KEY                                                          
         BNZ   RD6                                                              
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),PRQMED                                                  
         MVI   KEY+3,X'20'                                                      
         CLC   PRQCLT,=C'ALL'                                                   
         BE    *+10                                                             
         MVC   KEY+4(3),PRQCLT                                                  
*                                                                               
         CLC   PRQPRD,=C'ALL'                                                   
         BE    *+10                                                             
         MVC   KEY+7(3),PRQPRD                                                  
*                                                                               
         CLC   PRQEST,=C'ALL'                                                   
         BE    RD4                                                              
         PACK  DUB,PRQEST                                                       
         CVB   R0,DUB                                                           
         STH   R0,BEST                                                          
         MVC   KEY+19(2),BEST                                                   
*                                                                               
RD4      DS    0H                                                               
         CLC   PRQPUB(3),=C'ALL'                                                
         BE    *+10                                                             
         MVC   KEY+10(6),BPUB                                                   
*                                                                               
         MVC   KEY+16(3),BSTART                                                 
*                                                                               
         GOTOR HIGH                                                             
         B     RD6H                                                             
*                                                                               
RD6      GOTOR SEQ                                                              
*                                                                               
RD6H     CLC   KEY(4),KEYSAVE      SAME AGY/MED/RECORD CODE?                    
         BNE   RD40                                                             
         CLI   KEY+25,X'FF'                                                     
         BE    RD6                                                              
         CLC   PRQCLT,=C'ALL'                                                   
         BE    RD8                                                              
         CLC   KEY+4(3),PRQCLT                                                  
         BNE   RD40                                                             
*                                                                               
RD8      DS    0H                                                               
         CLC   PRQPRD,=C'ALL'                                                   
         BE    RD10                                                             
         CLC   KEY+7(3),PRQPRD                                                  
         BE    RD12                                                             
RD8B     CLC   PRQCLT,=C'ALL'                                                   
         BE    RD6                                                              
         B     RD40                                                             
*                                                                               
RD10     DS    0H                                                               
         CLC   KEY+7(3),=C'ZZZ'    BYPASS POL IF PRD=ALL                        
         BE    RD8B                                                             
*                                                                               
RD12     DS    0H                                                               
         OC    KEY+21(3),KEY+21    BYPASS PASSIVE                               
         BNZ   RD6                                                              
*                                                                               
         OC    BEST,BEST                                                        
         BZ    RD14                                                             
         CLC   KEY+19(2),BEST                                                   
         BNE   RD6                                                              
*                                                                               
RD14     DS    0H                                                               
         CLC   PRQPUB(3),=C'ALL'                                                
         BE    RD16                                                             
         CLC   KEY+10(6),BPUB                                                   
         BE    RD16                                                             
         CLC   QPUB+8(3),=C'ZZZ'   TEST MULTI ZONES/EDITS                       
         BNE   RD14B                                                            
         CLC   KEY+10(4),BPUB                                                   
         BE    RD16                                                             
*                                                                               
RD14B    DS    0H                                                               
         CLC   PRQPRD,=C'ALL'                                                   
         BE    RD6                                                              
         B     RD8B                                                             
*                                                                               
RD16     DS    0H                                                               
         CLC   KEY+16(3),BSTART                                                 
         BL    RD6                                                              
         CLC   KEY+16(3),BEND                                                   
         BNH   RD18                                                             
*                                                                               
         CLC   PRQPUB(3),=C'ALL'   IF ALL PUBS                                  
         BE    RD6                 CONTINUE                                     
         CLC   QPUB+8(3),=C'ZZZ'   OR IF MULTI Z/E                              
         BE    RD6                                                              
         B     RD14B                                                            
*                                                                               
RD18     DS    0H                                                               
         CLC   QBUYLIN,SPACES                                                   
         BE    RD20                                                             
         PACK  DUB,QBUYLIN                                                      
         CVB   R0,DUB                                                           
         STC   R0,BYTE                                                          
         CLC   BYTE,KEY+24         TEST LINE NUMBER                             
         BNE   RD6                                                              
*                                                                               
RD20     DS    0H                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTOR GETPRT                                                           
*                                                                               
         CLI   SRESVTYP,SRTSTEWQ   STEWARDSHIP RESERVATION REQUEST?             
         JE    RD20H                                                            
         CLI   SRESVTYP,SRTSTWEQ   STEWARDSHIP ESR BY ESTIMATE?                 
         JE    RD20H                                                            
         CLI   SRESVTYP,SRT1SWPQ   STEWARDSHIP ESR BY ESTIMATE PERIOD?          
         JE    RD20H                                                            
         J     RD20K                                                            
RD20H    TM    PBDSTAT2,X'40'      STEWARDSHIP INSERTION?                       
         BZ    RD6                 NO, SKIP                                     
         B     *+12                                                             
RD20K    CLI   PBDBFD,C'T'         TEST BUYS?                                   
         BE    RD6                 YES, SKIP                                    
*                                                                               
         TM    PBDSTAT,X'08'       HELD - NOT ON I/O OR CONTRACT?               
         BO    RD6                 YES, SKIP                                    
*                                                                               
         TM    PBDSTAT,X'20'       NO TRAFFIC?                                  
         BO    RD6                 YES, SKIP                                    
*                                                                               
         GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD,0                             
*                                                                               
         TM    ADBSW,AS_ADBRQ      ADBUYER?                                     
         BZ    RD22                NO - CONTINUE                                
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'99'        SERIAL NUMBER ELEMENT                        
         BRAS  RE,NXTEL                                                         
         BNE   RD6                 SKIP BUY - NO SERIAL NO ELEM IN REC          
         L     R3,ASER#TAB         POINT TO MAP ELEM OF SERIAL#S                
         USING PSERELMD,R2                                                      
         USING SER#TABD,R3                                                      
         SR    RF,RF                                                            
         ICM   RF,3,NUMSER#S       NUMBER OF SERIAL#S IN TABLE                  
*                                                                               
RD21LUP  BRAS  RE,CKESREBC         ESR BY EST, BACKWARD COMPATIBLE?             
         JNE   EXIT                                                             
         CP    S#SERIAL,PSERNUM    SERIAL# MATCH?                               
         BNE   RD21BMP             NO - TRY NEXT TABLE ENTRY                    
*                                                                               
         BRAS  RE,CKESRCAN         CK FOR CANCELLATIONS                         
         BE    RD21_OK                                                          
         OC    ERR(2),ERR                                                       
         JNZ   EXIT                ERROR OCCURED, EXIT WITH ERROR CODE          
         B     RD21BMP             ALREADY CANCELLED, SKIP INSERTION            
*                                                                               
RD21_OK  MVI   S#STATUS,S#USED_Q   INDICATE BUY REC "USED" HERE                 
         B     RD22                CONTINUE                                     
*                                                                               
RD21BMP  LA    R3,SER#TBLQ(R3)     BUMP TO NEXT TABLE ENTRY                     
         BCT   RF,RD21LUP                                                       
*                                                                               
         BRAS  RE,CKESRNUM         CK FOR ESR #                                 
         BE    RD21_M              NEED TO INCLUDE THIS INSERTION               
RD21_ERR OC    ERR(2),ERR                                                       
         JNZ   EXIT                ERROR OCCURED, EXIT WITH ERROR CODE          
         B     RD6                 NO ERROR, BUT NEED TO SKIP THIS BUY          
*                                                                               
RD21_M   BRAS  RE,CKSR_EST         CK FOR SPACE RESERVATION BY EST              
         BNE   RD21_ERR                                                         
*                                                                               
RD22     BRAS  RE,SETBUYDA                                                      
         JNE   EXIT                                                             
         CLI   RCWRITE,C'Y'        UPDATING RECORDS?                            
         BNE   RD22H               NO, THEN NO NEED TO CHECK LOCKS              
*                                                                               
         BRAS  RE,CKESRERR         CK FOR ERRORS IN WRITE MODE                  
         JNE   EXIT                                                             
         DROP  R2,R3                                                            
*                                                                               
         CLI   DATALKSW,X'FE'      LOCK ALREADY ISSUED?                         
         BE    RD22H                                                            
         BRAS  RE,ADDLOCK          CK DATA LOCK AND ISSUE ONE                   
         BE    RD22C                                                            
         MVI   ERRAREA,X'FE'       DATA IS ALREADY LOCKED!                      
         MVC   ERR(2),=AL2(DATALOCK)                                            
         J     EXIT                                                             
*                                                                               
RD22C    MVI   DATALKSW,X'FE'      LOCK HAS BEEN ADDED                          
         GOTOR HIGH                NEED TO RESTORE SEQUENCES                    
         CLC   KEY(25),KEYSAVE     SAME RECORD RETURNED?                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RD22H    XC    DUB,DUB             GET PUB                                      
         MVC   DUB(6),PBUYKPUB                                                  
         CLC   QPUB+8(3),=C'ZZZ'   IF MULTI ZONES/EDITS                         
         BE    RD23                                                             
         BRAS  RE,FNDPUB                                                        
         B     RD23F                                                            
*                                                                               
RD23     DS    0H                  MULTI Z/E                                    
         CLC   SVZON(2),PBUYKPUB+4 TEST NEW Z/E                                 
         BE    RD23B                                                            
         BRAS  RE,FNDPUB                                                        
         MVC   SVZON(2),PBUYKPUB+4 SAVE Z/E                                     
         MVC   SVZONNM,PUBZNAME    AND ZONE NAME                                
*                                                                               
RD23B    DS    0H                                                               
         MVC   DUB(6),BPUB         NOW READ BASE PUB                            
         BRAS  RE,FNDPUB                                                        
*                                                                               
RD23F    DS    0H                                                               
         MVC   PPGKEY,KEY                                                       
*                                  MAKE SURE HAVE PROPER HEADERS                
         XC    WORK(32),WORK                                                    
*                                                                               
         MVC   WORK(7),PBUYKEY                                                  
         MVI   WORK+3,2            CLIENT                                       
         CLC   WORK(25),PCLTKEY                                                 
         BE    RD24                                                             
         MVC   KEY,WORK                                                         
         GOTOR READ                                                             
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTOR GETPRT                                                           
*                                                                               
RD24     MVI   WORK+3,6            PRODUCT                                      
         MVC   WORK+7(3),PBUYKPRD                                               
         CLC   WORK(25),PPRDKEY                                                 
         BE    RD26                                                             
         MVC   KEY,WORK                                                         
         GOTOR READ                                                             
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         GOTOR GETPRT                                                           
         B     RD26                                                             
*                                                                               
RD26     B     EXXMOD                                                           
*                                                                               
RD40     MVI   PBUYKEY,X'FF'                                                    
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
EXIT     XIT1                                                                   
*                                                                               
NXTEL    ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R2),0                                                          
         JNE   NXTEL                                                            
         LTR   R2,R2               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FNDPUB   NTR1  BASE=*,LABEL=*      GET PUB RECORD - PUB CODE IN DUB             
*                                                                               
         XC    FULL,FULL                                                        
         MVI   PUBREC,0                                                         
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(1),PRQMED                                                    
         MVC   KEY+1(6),DUB                                                     
         MVC   KEY+7(2),AGYALPHA                                                
         GOTOR HIGHPB                                                           
         B     FP2K                                                             
FP2      GOTOR SEQPB                                                            
FP2K     CLC   KEY(7),KEYSAVE                                                   
         BE    FP3                                                              
         CLC   QPUB+8(3),=C'ZZZ'   TEST MULTI Z/E                               
         BNE   FP8                                                              
         CLC   KEY(5),KEYSAVE      TEST THRU Z/E                                
         BNE   FP8                                                              
*                                                                               
FP3      CLC   KEY+7(2),KEYSAVE+7                                               
         BNE   FP2                                                              
         CLI   KEY+9,X'81'                                                      
         BNE   FP2                                                              
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
         GOTOR GETPUB                                                           
*                                                                               
FP8      MVC   KEY(64),WORK                                                     
         CLI   PUBREC,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CK ADBUYER VERSION TO DETERMINE OKAY OR NOT TO CONTINUE RESERVATION           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKESREBC NTR1  BASE=*,LABEL=*      CK FOR BACKWARD COMPATIBILITY                
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         CLC   LIOBPCV1,=AL1(03,05,00,06)                                       
         BNL   CKEBC_X                                                          
         MVI   ELCODE,PESRELCQ     ESR ELEM CODE                                
         LA    R2,PBUYREC+33                                                    
         USING PESRELEM,R2                                                      
         CLI   SVESRESW,0                                                       
         BE    CKEBC_X                                                          
         CLI   SVESRESW,C'Y'       USEESRO BY EST STARTING MOS?                 
         BNE   CKEBC_30                                                         
         CLC   SVEACMOS,PBUYREC+(PBUYKDAT-PBUYKEY)                              
         BNH   CKEBC_ER                                                         
CKEBC_24 BRAS  RE,NXTEL                                                         
         BNE   CKEBC_X                                                          
         CLI   PESRMODC,C'D'       ALREADY CANCELLED?                           
         BE    CKEBC_24                                                         
         TM    PESRSTAT,PESRDBMQ   DEL'D BUY MOVE CHANGE RESV ISSUED?           
         BNZ   CKEBC_24                                                         
         TM    PESRSTAT,PESRESTQ   EIO BY ESTIMATE?                             
         BZ    CKEBC_24                                                         
         B     CKEBC_ER                                                         
*                                                                               
CKEBC_30 CLI   SVESRESW,C'N'       DON'T USE ESR BY EST STARTING MOS?           
         BNE   CKEBC_ER                                                         
         CLC   SVEACMOS,PBUYREC+(PBUYKDAT-PBUYKEY)                              
         BL    CKEBC_ER                                                         
         B     CKEBC_24                                                         
*                                                                               
CKEBC_X  J     SETCCEQ                                                          
*                                                                               
CKEBC_ER MVC   ERR(2),=AL2(NEWVERRQ)                                            
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB,R3,R2                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* IF INSERTION IS NOT IN ADBUYER REQUEST LIST, NEED TO CK IF ESR #              
* IS SAME (REVISION # COULD BE DIFFERENT)                                       
*                                                                               
* CC EQUAL     = NEED TO INCLUDE THIS INSERTION ON ORDER                        
* CC NOT EQUAL = OKAY TO SKIP THIS INSERTION                                    
*                ALSO CK "ERR" FOR POSSIBLE ERRORS                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKESRNUM NTR1  BASE=*,LABEL=*      CK FOR WEB IO#                               
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR?                                         
         JZ    SETCCNEQ                                                         
*                                                                               
         L     R4,AESRDSST         POINT TO ESR STORAGE BLOCK                   
         USING ESRDSD,R4           WEB IO STORAGE BLOCK                         
         LA    R0,ESR#RTAB                                                      
         LHI   R1,E#R_MAXQ*E#R_LENQ                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,PESRELCQ     ESR ELEM CODE                                
         BRAS  RE,NXTEL                                                         
         JNE   SETCCNEQ                                                         
*                                                                               
         USING PESRELEM,R2                                                      
CKE#10_T MVC   FULL,PESR#YER       SAVE YEAR AND SEQ#                           
CKE#20   LR    RF,R2               SAVE CURRENT ESR ELEM LOCATION               
         BRAS  RE,NXTEL                                                         
         BNE   CKE#30                                                           
         J     CKE#10_T            ALLOW DIFFERENT SR# TO GO THROUGH            
         CLC   FULL,PESR#YER                                                    
         BE    CKE#20                                                           
CKE#20_T MVC   ERR(2),=AL2(OAENRVER)                                            
         J     SETCCNEQ                                                         
*                                                                               
CKE#30   LR    R2,RF               POINT TO LAST ESR ELEM                       
         CLI   PESRMODC,C'D'       ALREADY CANCELLED?                           
         JE    CKE#40                                                           
         CLC   SVESR#,PESR#YER                                                  
         JE    CKE#40                                                           
         CLI   SRESVTYP,SRT1ESTQ   ESR BY ESTIMATE?                             
         JE    CKE#34                                                           
         CLI   SRESVTYP,SRTSTWEQ   STEWARDSHIP ESR BY ESTIMATE?                 
         JE    CKE#34                                                           
         CLI   SRESVTYP,SRT1ESPQ   ESR BY ESTIMATE PERIOD?                      
         JE    CKE#36                                                           
         CLI   SRESVTYP,SRT1SWPQ   STEWARDSHIP ESR BY ESTIMATE PERIOD?          
         JE    CKE#36                                                           
*                                                                               
         TM    PESRSTAT,PESRESTQ+PESREPRQ                                       
         JNZ   SETCCNEQ            SKIP, ALREADY ON DIFFERENT ESR               
         J     CKE#40                                                           
*                                                                               
CKE#34   TM    PESRSTAT,PESRESTQ   ESR BY ESTIMATE?                             
         JZ    SETCCNEQ            SKIP, ALREADY ON DIFFERENT ESR               
         J     CKE#38                                                           
CKE#36   TM    PESRSTAT,PESREPRQ   ESR BY ESTIMATE PERIOD?                      
         JZ    SETCCNEQ            SKIP, ALREADY ON DIFFERENT ESR               
         J     CKE#38                                                           
*                                                                               
CKE#38   TM    PBUYCNTL,X'80'      DELETED?                                     
         JNZ   SETCCNEQ            SKIP, ALREADY ON DIFFERENT ESR               
         J     CKE#20_T                                                         
*                                                                               
CKE#40   LR    R2,RF               POINT TO LAST ESR ELEM                       
         CLI   PESRMODC,C'D'       ALREADY CANCELLED?                           
         JE    SETCCNEQ            NEED TO SKIP ALREADY CANCELLED               
         TM    PESRSTAT,PESRDBMQ   DEL'D BUY MOVE CHANGE RESV ISSUED?           
         JNZ   SETCCNEQ            SKIP DELETED CHANGED BUY MOVE RESV           
*                                                                               
CKE#50   CLI   SRESVTYP,SRT1ESTQ   ESR BY ESTIMATE?                             
         JE    *+12                                                             
         CLI   SRESVTYP,SRTSTWEQ   STEWARDSHIP ESR BY ESTIMATE?                 
         JNE   CKE#52                                                           
         TM    PESRSTAT,PESRESTQ   ESR BY ESTIMATE?                             
         JNZ   CKW#80                                                           
         J     CKW#56                                                           
*                                                                               
CKE#52   CLI   SRESVTYP,SRT1ESPQ   ESR BY ESTIMATE PERIOD?                      
         JE    *+12                                                             
         CLI   SRESVTYP,SRT1SWPQ   STEWARDSHIP ESR BY ESTIMATE PERIOD?          
         JNE   CKW#60                                                           
         TM    PESRSTAT,PESREPRQ   ESR BY ESTIMATE PERIOD?                      
         JNZ   CKW#80                                                           
*                                                                               
CKW#56   XC    WORK(E#R_LENQ),WORK                                              
         MVC   WORK(L'E#R_ESTC),PBUYREC+(PBUYKEST-PBUYKEY)                      
         MVC   WORK+L'E#R_ESTC(L'E#R_ESR#),PESRNUMB                             
         MVC   WORK+L'E#R_ESTC+L'E#R_ESR#(L'E#R_DATE),PESRDATE                  
         LA    RE,ESR#RTAB                                                      
         LHI   RF,E#R_MAXQ                                                      
CKW#58   OC    0(E#R_LENQ,RE),0(RE)                                             
         BNZ   *+14                                                             
         MVC   0(E#R_LENQ,RE),WORK                                              
         B     CKW#80                                                           
         CLC   WORK(E#R_DATE-E#R_ESTC),0(RE)                                    
         BE    CKW#80                                                           
         LA    RE,E#R_LENQ(RE)                                                  
         BCT   RF,CKW#58                                                        
*                                                                               
CKW#60   TM    PESRSTAT,PESRESTQ   ESR BY ESTIMATE?                             
         JNZ   CKW#56                                                           
         TM    PESRSTAT,PESREPRQ   ESR BY ESTIMATE PERIOD?                      
         JNZ   CKW#56                                                           
                                                                                
*                                                                               
CKW#80   CLC   SVESR#,FULL         SAME WEB IO #?                               
         JE    SETCCEQ             NEED TO INCLUDE IT FOR SAME #                
*                                                                               
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB,R2,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* IF INSERTION IS NOT IN ADBUYER REQUEST LIST, NEED TO CK IF THIS               
* INSERTION IS RESERVED BY ESTIMATE                                             
*                                                                               
* CC EQUAL     = NEED TO INCLUDE THIS INSERTION ON ORDER                        
* CC NOT EQUAL = OKAY TO SKIP THIS INSERTION                                    
*                ALSO CK "ERR" FOR POSSIBLE ERRORS                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKSR_EST NTR1  BASE=*,LABEL=*      CK FOR SPACE RESERVATION BY EST              
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR?                                         
         JZ    SETCCNEQ                                                         
*                                                                               
         LA    R2,PBUYREC+33                                                    
         USING PESRELEM,R2                                                      
         MVI   ELCODE,PESRELCQ     ESR ELEM CODE                                
         BRAS  RE,NXTEL                                                         
         JNE   SETCCEQ                                                          
*                                                                               
CKES20   LR    RF,R2               SAVE CURRENT EIO ELEM LOCATION               
         BRAS  RE,NXTEL                                                         
         BE    CKES20                                                           
         LR    R2,RF               POINT TO LAST EIO ELEM                       
*                                                                               
         CLI   SRESVTYP,SRT1ESTQ   ESR BY ESTIMATE?                             
         JE    *+12                                                             
         CLI   SRESVTYP,SRTSTWEQ   ESR BY ESTIMATE?                             
         BNE   CKES30                                                           
         TM    PESRSTAT,PESRESTQ   ESR BY ESTIMATE?                             
         JZ    SETCCNEQ                                                         
         J     SETCCEQ                                                          
*                                                                               
CKES30   CLI   SRESVTYP,SRT1ESPQ   ESR BY ESTIMATE PERIOD?                      
         JE    *+12                                                             
         CLI   SRESVTYP,SRT1SWPQ   STEWARDSHIP ESR BY ESTIMATE PERIOD?          
         JNE   CKES40                                                           
         TM    PESRSTAT,PESREPRQ   ESR BY ESTIMATE PERIOD?                      
         JZ    SETCCNEQ                                                         
         J     SETCCEQ                                                          
*                                                                               
CKES40   TM    PESRSTAT,PESRESTQ   ESR BY ESTIMATE?                             
         JNZ   SETCCNEQ                                                         
         TM    PESRSTAT,PESREPRQ   ESR BY ESTIMATE PERIOD?                      
         JNZ   SETCCNEQ                                                         
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R2                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* IF ESR ALREADY CANCELLED, NEED TO SKIP THIS INSERTION                         
*                                                                               
* CC EQUAL     = NEED TO INCLUDE THIS INSERTION ON ESR                          
* CC NOT EQUAL = OKAY TO SKIP THIS INSERTION                                    
*                ALSO CK "ERR" FOR POSSIBLE ERRORS                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKESRCAN NTR1  BASE=*,LABEL=*      CK FOR ESR CANCELLATIONS                     
*                                                                               
         TM    ADBSW,AS_ESRUQ      ESR?                                         
         JZ    SETCCEQ                                                          
*                                                                               
         XC    ERR(2),ERR                                                       
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,PESRELCQ     ESR ELEM CODE                                
         LR    RF,R2                                                            
CKSRC30  BRAS  RE,NXTEL                                                         
         BNE   CKSRC40                                                          
         LR    RF,R2                                                            
         B     CKSRC30                                                          
*                                                                               
CKSRC40  LR    R2,RF                                                            
         USING PESRELEM,R2                                                      
         CLI   PESRELEM,PESRELCQ                                                
         BNE   CKSRC50                                                          
         OC    PESRDATE,PESRDATE                                                
         JZ    SETCCEQ                                                          
*                                                                               
         CLI   PESRMODC,C'D'       CANCELLATION ORDER?                          
         JE    SETCCNEQ                                                         
         TM    PESRSTAT,PESRDBMQ   DEL'D BUY MOVE CHANGE RESV ISSUED?           
         JNZ   SETCCNEQ            SKIP DELETED CHANGED BUY MOVE RESV           
         J     SETCCEQ                                                          
*                                                                               
CKSRC50  TM    PBUYCNTL,X'80'      DELETED AND NO ESR ELEMS?                    
         JNZ   SETCCNEQ            IGNORE THIS INSERTION                        
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R2                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CK FOR VARIOUS ERROR CONDITIONS FOR ESR OR OTHER ERROS                        
*                                                                               
* CC EQUAL     = NO ERROR FOUND, OKAY TO WRITE                                  
* CC NOT EQUAL = CANNOT WRITE, RECORD IS BAD, "ERR" HAS ERROR NUMBER            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKESRERR NTR1  BASE=*,LABEL=*      CK FOR ERRORS IN WRITE MODE                  
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,PBUYLEN                                                     
         CHI   RE,2800             RECORD IS GETTING TOO BIG?                   
         BH    CKESR91                                                          
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
CKESR91  MVC   ERR(2),=AL2(RECMAXER)                                            
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* TEST DATA LOCKED BY OFFLINE APPLICATION                                       
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS               
* ARE AGREED (LOCKUP/LOCKET DSECTS ARE IDENTICAL)                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ADDLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,KEY+2                                                  
         MVC   L.LOCKCLT,KEY+4                                                  
*                                                                               
ADDLK2   L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK2              BUSY, TRY AGAIN                              
         CLI   4(R1),1                                                          
         JE    SETCCNEQ            ALREADY LOCKED?                              
         CLI   4(R1),0                                                          
         JNE   SETCCNEQ            ERROR OCCURED                                
*                                                                               
         CLC   PRQPUB(3),=C'ALL'   PUB IS ALL?                                  
         BE    ADDLK4              YES, GO AHEAD AND ISSUE CLIENT LOCK          
         OC    KEY+10(4),KEY+10    NOTHING IN BASE PUB NUMBER?                  
         BZ    ADDLK4              YES, GO AHEAD AND ISSUE CLIENT LOCK          
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BP'    CLIENT LOCK                                  
         MVC   L.LOCKMED,KEY+02                                                 
         MVC   L.LOCKCLT,KEY+04                                                 
         MVC   L.LOCKPUB,KEY+10    BASE PUB NUMBER                              
*                                                                               
         XC    L.LOCKPUB,=4X'FF'   COMPLEMENT PUB (NO BINARY ZERO)              
*                                                                               
ADDLK3   L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK3              BUSY, TRY AGAIN                              
         CLI   4(R1),1                                                          
         JE    SETCCNEQ            ALREADY LOCKED?                              
         CLI   4(R1),0                                                          
         JNE   SETCCNEQ            ERROR OCCURED                                
*                                                                               
ADDLK4   L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKLOCKQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK4              BUSY, TRY AGAIN                              
         CLI   4(R1),1                                                          
         JE    SETCCNEQ            ALREADY LOCKED                               
         CLI   4(R1),0                                                          
         JNE   SETCCNEQ            ERROR OCCURED                                
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,L                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETBUYDA NTR1  BASE=*,LABEL=*      SET BUY DISK ADDRESSES                       
*                                                                               
         XC    ERR(2),ERR                                                       
*                                                                               
         ICM   RE,15,INSCNT                                                     
         AHI   RE,1                                                             
         STCM  RE,15,INSCNT                                                     
*                                                                               
         LA    RE,BUYDALST         POINT TO BUY ADDRESS TABLE                   
         LHI   RF,1                COUNTER                                      
SETBD12  CHI   RF,BUYDAMXQ                                                      
         BNL   SETBD91                                                          
         CLC   KEY+27(4),0(RE)     ALREADY IN TABLE?                            
         JE    SETCCEQ                                                          
         OC    0(4,RE),0(RE)                                                    
         BZ    SETBD16                                                          
         LA    RE,4(RE)                                                         
         AHI   RF,1                                                             
         B     SETBD12                                                          
SETBD16  MVC   0(4,RE),KEY+27                                                   
         J     SETCCEQ                                                          
*                                                                               
SETBD91  MVC   ERR(2),=AL2(MAXLSTSZ)                                            
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPSR202D DSECT                                                                  
*                                                                               
RELO02   DS    F                                                                
*                                                                               
WRKFULL1 DS    F                                                                
WRKFULL2 DS    F                                                                
WRKDUB_1 DS    D                                                                
WRKDUB_2 DS    D                                                                
*                                                                               
PPSR202X EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPSR2WRK1                                                      
*                                                                               
       ++INCLUDE PPSR2WRK2                                                      
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010PPSR202   10/15/12'                                      
         END                                                                    
