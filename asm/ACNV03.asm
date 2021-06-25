*          DATA SET ACNV03     AT LEVEL 005 AS OF 05/01/02                      
*PHASE ACNV03A                                                                  
         TITLE 'ACCPAK CONVERSION - ACCOUNT/HISTORY MERGE'                      
         PRINT NOGEN                                                            
ACNVMRG  CSECT                                                                  
         NMOD1 0,*MERGE*,RA                                                     
         USING ACNVD,R9                                                         
         L     R8,CPRINT                                                        
         USING DPRINT,R8                                                        
         ZAP   MAXLINE,=P'57'                                                   
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         EJECT                                                                  
***********************************************************************         
* GET RECORDS FROM INPUT TAPE                                         *         
***********************************************************************         
                                                                                
         BAS   RE,OPNF             OPEN FILES                                   
*                                                                               
         L     R3,AINPL                                                         
         XC    0(4,R3),0(R3)                                                    
         L     R2,AOUTL                                                         
ACCNVM3  ST    R2,AINPUT           SET INPUT ADDRESS                            
         BAS   RE,GETIN            GET NEXT RECORD                              
         BZ    ACCNVM13                                                         
         LA    RF,4(R2)                                                         
         USING TRNRECD,RF                                                       
         CLI   TRNKCPY,X'40'       TEST SPECIAL RECORDS                         
         BNH   ACCNVM7                                                          
         CLI   TRNRFST,TRNELQ      TRANSACTION                                  
         BE    ACCNVM7                                                          
         NI    TRNKSBR,X'FE'       TURNOFF POSSIBLE 01 BIT                      
         DROP  RF                                                               
*                                                                               
ACCNVM7  L     R2,AINPL            TEST FIRST TIME                              
         OC    0(4,R3),0(R3)                                                    
         BZ    ACCNVM3                                                          
*                                                                               
ACCNVM9  L     R2,AOUTL                                                         
         GOTO1 ACRECTYP,DMCB,(C'D',4(R2))                                       
         CLI   0(R1),0             TEST BAD RECORD                              
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   RECTYP,0(R1)                                                     
*                                                                               
         L     R3,AINPL                                                         
         CLC   4(42,R2),4(R3)      TEST SAME KEY                                
         BE    ACCNVM11                                                         
         BAS   RE,POUT             PUT OUT TAPE                                 
*                                                                               
         BAS   RE,RPT              PRINT A REPORT                               
         LR    RE,R3               MOVE INP (R3)                                
         LR    R0,R2               TO   OUT (R2)                                
         SR    R1,R1                                                            
         ICM   R1,3,0(R3)                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R2,AINPL                                                         
         B     ACCNVM3                                                          
*                                                                               
ACCNVM11 BAS   RE,MRGR             MERGE INPUT (R3) INTO OUTPUT (R2)            
         L     R2,AINPL                                                         
         B     ACCNVM3                                                          
*                                                                               
ACCNVM13 L     R2,AOUTL                                                         
         BAS   RE,POUT             PUT LAST RECORD                              
         CLOSE (TOUT)                                                           
*                                                                               
         TM    OUTPUT,DIRO         DIRECTORY OUTPUT                             
         BNO   ACCNVM15                                                         
         CLOSE (DOUT)                                                           
*                                                                               
ACCNVM15 MVI   LASTACC,X'FF'                                                    
         BAS   RE,RPT              PRINT FINAL TOTAL                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PUT RECORDS TO OUTPUT FILE                                          *         
***********************************************************************         
                                                                                
POUT     NTR1  ,                                                                
         CLC   LASTKEY,4(R2)                                                    
         BL    *+6                                                              
         DC    H'0'                FILE OUT OF SEQUENCE                         
         MVC   LASTKEY,4(R2)                                                    
*                                                                               
         TM    HOOKSW,HOOKIT       TEST HOOK                                    
         BNO   PUT3                                                             
         MVI   MODE,PROCOUT        LAST CHANCE TO FIX                           
         GOTO1 ACNVHOOK            CALL USER HOOK                               
*                                                                               
PUT3     SR    RF,RF                                                            
         ICM   RF,3,ACCRLEN-ACCRECD+4(R2)                                       
         LA    RF,4(RF)                                                         
         STCM  RF,3,0(R2)                                                       
*                                                                               
         PUT   TOUT,(R2)                                                        
*                                                                               
         TM    OUTPUT,DIRO         CREATE DIRECTORY TEST                        
         BNO   XIT                                                              
         MVC   DIRKEY,4(R2)        KEY FILE                                     
         PUT   DOUT,DIRIO                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MERGE RECORD IN INP INTO RECORD THE RECORD IN OUT                   *         
***********************************************************************         
                                                                                
MRGR     NTR1  ,                                                                
         L     R2,AOUT                                                          
*        BAS   RE,PRNTDUP                                                       
*                                                                               
         CLI   RECTYP,ACRTBUD      BUDGET RECORDS                               
         BE    BUDG                                                             
         CLI   RECTYP,ACRTLDG      LEDGER                                       
         BE    LDGM                                                             
         CLI   RECTYP,ACRTACTH     ACCOUNT HIGH                                 
         BE    ACHI                                                             
         CLI   RECTYP,ACRTACTL     ACCOUNT LOW                                  
         BE    ACLO                                                             
         CLI   RECTYP,ACRTOFA      OFFICE/ACCOUNT                               
         BE    ACLO                                                             
         CLI   RECTYP,ACRTCHDH     CONTRA HEADER                                
         BE    CONB                                                             
         CLI   RECTYP,ACRTCAC      CONTRA BUCKETS                               
         BE    CONB                                                             
         CLI   RECTYP,ACRTPCR      PRODUCTION CHARGE RATES                      
         BE    ACHI                                                             
         CLI   RECTYP,ACRTCPY      COMPANY RECORD                               
         BE    ACHI                                                             
         CLI   RECTYP,ACRTUNT      UNIT RECORD                                  
         BE    ACHI                                                             
         CLI   RECTYP,ACRTPOP      PRODUCTION OPTIONS                           
         BE    ACHI                                                             
         B     ACHI                EVERYTHING ELSE DROP DUPLICATE               
**       DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* MERGE BUDGET RECORDS                                                *         
***********************************************************************         
                                                                                
BUDG     L     R2,AINP                                                          
R        USING BUDRECD,R2                                                       
         LA    R3,R.BUDRFST                                                     
E        USING BAMELD,R3                                                        
         SR    R0,R0                                                            
*                                                                               
BUDG3    CLI   E.BAMEL,0              FIND BUDGET ELEMENTS                      
         BE    XIT                                                              
         CLI   E.BAMEL,BAMELQ                                                   
         BE    BUDG7                                                            
BUDG5    IC    R0,E.BAMLN                                                       
         AR    R3,R0                                                            
         B     BUDG3                                                            
*                                                                               
BUDG7    L     R4,AOUT                                                          
         USING BUDRECD,R4                                                       
         LA    R5,BUDRFST                                                       
         USING BAMELD,R5                                                        
*                                                                               
BUDG9    CLI   BAMEL,0                MATCH TO ELEMENT IN RECORD 1              
         BE    BUDG15                                                           
         CLI   BAMEL,BAMELQ                                                     
         BNE   BUDG11                                                           
         CLC   BAMMNTH,E.BAMMNTH TEST SAME MONTH                                
         BNE   *+14                                                             
         AP    BAMBUDG,E.BAMBUDG                                                
         B     BUDG5                                                            
BUDG11   IC    R0,BAMLN                                                         
         AR    R5,R0                                                            
         B     BUDG9                                                            
*                                                                               
BUDG15   GOTO1 HELLO,DMCB,(C'P',ACCMST),AOUT,E.BAMELD                           
         CLI   12(R1),0                                                         
         BE    BUDG5                                                            
         DC    H'0'                CANT ADD NEW 1D ELEMENT                      
         DROP  R,E,R4,R5                                                        
         EJECT                                                                  
***********************************************************************         
* MERGE LEDGER RECORDS                                                *         
***********************************************************************         
                                                                                
LDGM     L     RE,AINPL            MOVE INP (RE) 'COPIED' RECORD                
         L     R0,AOUTL            TO   OUT (R0) THE ONE TO KEEP                
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MERGE HIGH LEVEL ACCOUNT RECORDS                                    *         
***********************************************************************         
                                                                                
ACHI     DS    0H                                                               
         TM    HOOKSW,HOOKIT       TEST HOOK                                    
         BNO   XIT                                                              
         MVI   MODE,MRGEPRO        MERGE PROFILES                               
         GOTO1 ACNVHOOK            CALL USER HOOK                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MERGE LOW LEVEL ACCOUNT RECORDS                                     *         
***********************************************************************         
                                                                                
ACLO     TM    HOOKSW,HOOKIT       TEST HOOK                                    
         BNO   ACL1                                                             
         MVI   MODE,MRGEPRO        MERGE PROFILES                               
         GOTO1 ACNVHOOK            CALL USER HOOK                               
ACL1     LA    R6,ELLIST           LIST OF ELEMENTS TO MERGE                    
         SR    R0,R0                                                            
ACL3     L     R2,AINP                                                          
R        USING ACTRECD,R2                                                       
*&&US                                                                           
         CLC   R.ACTKUNT(2),=C'SJ'   TEST JOB                                   
         BNE   *+6                                                              
         DC    H'0'                  CAN'T ADD JOBS                             
         CLC   R.ACTKUNT(2),=C'1J'   TEST PROJECT CONTRAOL                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         LA    R3,R.ACTRFST                                                     
*                                                                               
ACL5     CLI   0(R3),0             TEST EOR                                     
         BE    ACL21                                                            
         CLC   0(1,R3),0(R6)       RECORD VS. LIST                              
         BE    ACL11                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     ACL5                                                             
*                                                                               
ACL11    L     R4,AOUT                                                          
         USING ACTRECD,R4                                                       
         LA    R5,ACTRFST                                                       
*                                                                               
ACL13    CLI   0(R5),0             TEST EOR                                     
         BE    ACL21                                                            
         CLC   0(1,R5),0(R3)       TEST ELEMENT MATCH                           
         BE    ACL15                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     ACL13                                                            
*                                                                               
ACL15    SR    RF,RF                                                            
         ICM   RF,7,1(R6)          GET ADDRESS OF ROUTINE                       
         BASR  RE,RF               AND DO IT                                    
*                                                                               
ACL21    LA    R6,L'ELLIST(R6)                                                  
         CLI   0(R6),EOT                                                        
         BNE   ACL3                                                             
         DROP  R,R4                                                             
*                                                                               
*        BAS   RE,PRNTFNL                                                       
*                                                                               
         CLI   RECTYP,ACRTOFA      OFFICE/ACCOUNT                               
         BNE   XIT                                                              
         L     R4,AOUT                                                          
         USING OFARECD,R4                                                       
         L     R2,AINP                                                          
R        USING OFARECD,R2                                                       
         CLC   OFARLMOS,R.OFARLMOS                                              
         BL    *+10                                                             
         MVC   OFARLMOS,R.OFARLMOS                                              
         CLC   OFARHMOS,R.OFARHMOS                                              
         BH    *+10                                                             
         MVC   OFARHMOS,R.OFARHMOS                                              
         B     XIT                                                              
         DROP  R,R4                                                             
*                                                                               
ELLIST   DS    0F                                                               
         DC    AL1(ABLELQ),AL3(ACLABL)                                          
         DC    AL1(ASTELQ),AL3(ACLAST)                                          
         DC    AL1(APOELQ),AL3(ACLAPO)                                          
         DC    AL1(NAMELQ),AL3(ACLNAM)                                          
         DC    AL1(ADRELQ),AL3(ACLADR)                                          
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* MERGE ACCOUNT BALANCE ELEMENTS                                      *         
***********************************************************************         
                                                                                
E        USING ABLELD,R3                                                        
         USING ABLELD,R5                                                        
ACLABL   AP    ABLFRWD,E.ABLFRWD                                                
         AP    ABLDR,E.ABLDR       ADD BALANCE ELEMENT                          
         AP    ABLCR,E.ABLCR                                                    
         BR    RE                                                               
         DROP  E,R5                                                             
         EJECT                                                                  
***********************************************************************         
* MERGE ACCOUNT STATUS ELEMENTS                                       *         
***********************************************************************         
                                                                                
E        USING ASTELD,R3                                                        
         USING ASTELD,R5                                                        
ACLAST   SR    RF,RF                                                            
         ICM   RF,7,E.ASTDRAFT     ADD NUMBER OF DRAFT TRANSACTIONS             
         SR    R0,R0                                                            
         ICM   R0,7,ASTDRAFT                                                    
         AR    R0,RF                                                            
         STCM  R0,7,ASTDRAFT                                                    
         SR    R0,R0                                                            
         BR    RE                                                               
         DROP  E,R5                                                             
         EJECT                                                                  
***********************************************************************         
* MERGE ACCOUNT PEEL ELEMENTS                                         *         
***********************************************************************         
                                                                                
E        USING APOELD,R3                                                        
         USING APOELD,R5                                                        
ACLAPO   DS    0H                                                               
         AP    APODR,E.APODR       ADD PEELED DEBITS \ CREDITS                  
         AP    APOCR,E.APOCR                                                    
         CLC   APOPLDT,E.APOPLDT GET BEST DATES                                 
         BH    *+10                                                             
         MVC   APOPLDT,E.APOPLDT                                                
         CLC   APOLBDT,E.APOLBDT                                                
         BH    *+10                                                             
         MVC   APOLBDT,E.APOLBDT                                                
         CLI   APOLN,APOLN2Q       TEST NEW ELEMENT LENGTHS                     
         BLR   RE                                                               
         CLI   E.APOLN,APOLN2Q                                                  
         BNL   ACLAPO5                                                          
         BR    RE                                                               
ACLAPO5  OC    APOLMOS,APOLMOS                                                  
         BZ    *+14                                                             
         CLC   APOLMOS,E.APOLMOS                                                
         BL    *+10                                                             
         MVC   APOLMOS,E.APOLMOS                                                
         CLC   APOCMOS,E.APOCMOS                                                
         BH    *+10                                                             
         MVC   APOCMOS,E.APOCMOS                                                
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* MERGE ACCOUNT NAME / ADDRESS ELEMENTS                               *         
***********************************************************************         
                                                                                
E        USING NAMELD,R3                                                        
         USING NAMELD,R5                                                        
ACLADR   DS    0H                  REPLACE NAME AND ADDRESS                     
ACLNAM   LR    R0,RE                                                            
         MVI   0(R5),X'FF'         DELETE FROM OUTPUT                           
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',AOUT),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  ADD TO OUTPUT                                
         GOTO1 HELLO,DMCB,(C'P',ACCMST),AOUT,(R3),0                             
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         DROP  E,R5                                                             
         EJECT                                                                  
***********************************************************************         
* MERGE CONTRA HEADERS                                                *         
***********************************************************************         
                                                                                
CONH     DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MERGE CONTRA BUCKETS                                                *         
***********************************************************************         
                                                                                
CONB     ZAP   INPDR,=P'0'                                                      
         ZAP   INPCR,=P'0'                                                      
         ZAP   OUTDR,=P'0'                                                      
         ZAP   OUTCR,=P'0'                                                      
         GOTO1 ADDIN,DMCB,AINP,INPDR                                            
         GOTO1 ADDIN,DMCB,AOUT,INPDR                                            
         L     R2,AINP                                                          
R        USING CACRECD,R2                                                       
         LA    R3,R.CACRFST                                                     
E        USING BUKELD,R3                                                        
         SR    R0,R0                                                            
*                                                                               
CONB3    CLI   E.BUKEL,0              FIND BUCKET ELEMENTS                      
         BE    CONB21                                                           
         CLI   E.BUKEL,BUKELQ                                                   
         BE    CONB7                                                            
CONB5    IC    R0,E.BUKLN                                                       
         AR    R3,R0                                                            
         B     CONB3                                                            
*                                                                               
CONB7    L     R4,AOUT                                                          
         USING CACRECD,R4                                                       
         LA    R5,CACRFST                                                       
         USING BUKELD,R5                                                        
         ZAP   XDR,=P'0'                                                        
         ZAP   XCR,=P'0'                                                        
*                                                                               
CONB9    CLI   BUKEL,0                MATCH TO ELEMENT IN RECORD 1              
         BE    CONB15                                                           
         CLI   BUKEL,BUKELQ                                                     
         BNE   CONB11                                                           
         CLC   BUKYEAR(2),E.BUKYEAR    TEST SAME MONTH                          
         BNE   CONB11                                                           
         CP    BUKDR,BHI           TEST BUCKET AT MAX                           
         BE    CONB11                                                           
         CP    BUKDR,BLO                                                        
         BE    CONB11                                                           
         CP    BUKCR,BHI                                                        
         BE    CONB11                                                           
         CP    BUKCR,BLO                                                        
         BE    CONB11                                                           
         ZAP   DR,BUKDR                                                         
         ZAP   CR,BUKCR                                                         
         AP    DR,E.BUKDR                                                       
         AP    CR,E.BUKCR                                                       
         CP    DR,BHI              TEST ELEMENT OVERFLOWED                      
         BNH   *+16                                                             
         SP    DR,BHI              DEBIT OVERFLOW                               
         ZAP   XDR,BHI                                                          
         CP    DR,BLO                                                           
         BNL   *+16                                                             
         SP    DR,BLO                                                           
         ZAP   XDR,BLO                                                          
*                                                                               
         CP    CR,BHI              CREDIT OVERFLOW                              
         BNH   *+16                                                             
         SP    CR,BHI                                                           
         ZAP   XCR,BHI                                                          
         CP    CR,BLO                                                           
         BNL   *+16                                                             
         SP    CR,BLO                                                           
         ZAP   XCR,BLO                                                          
*                                                                               
         ZAP   BUKDR,DR                                                         
         ZAP   BUKCR,CR                                                         
         CP    XDR,=P'0'                                                        
         BNE   *+14                                                             
         CP    XCR,=P'0'                                                        
         BE    CONB5                                                            
*                                                                               
         MVI   XBEL,BUKELQ         BUILD EXTRA MONTH BUCKET                     
         MVI   XBEL+(BUKLN-BUKEL),BUKLNQ                                        
         MVC   XBEL+(BUKYEAR-BUKEL)(2),BUKYEAR                                  
         ZAP   XBEL+(BUKDR-BUKEL)(L'BUKDR),XDR                                  
         ZAP   XBEL+(BUKCR-BUKEL)(L'BUKCR),XCR                                  
         GOTO1 HELLO,DMCB,(C'P',ACCMST),AOUT,XBEL                               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,PRNTFNL                                                       
         B     CONB5                                                            
*                                                                               
CONB11   IC    R0,BUKLN                                                         
         AR    R5,R0                                                            
         B     CONB9                                                            
*                                                                               
CONB15   GOTO1 HELLO,DMCB,(C'P',ACCMST),AOUT,E.BUKELD                           
         CLI   12(R1),0                                                         
         BE    CONB5                                                            
         DC    H'0'                                                             
         DROP  R,E,R4,R5                                                        
         EJECT                                                                  
***********************************************************************         
* MERGE CONTRA PRIOR BUCKETS                                          *         
***********************************************************************         
                                                                                
CONB21   L     R2,AINP                                                          
R        USING CACRECD,R2                                                       
         LA    R3,R.CACRFST                                                     
E        USING PBKEL,R3                                                         
         SR    R0,R0                                                            
*                                                                               
CONB23   CLI   E.PBKEL,0             FIND BUCKET ELEMENTS                       
         BE    CONB41                                                           
         CLI   E.PBKEL,PBKELQ                                                   
         BE    CONB25                                                           
         IC    R0,E.PBKLN                                                       
         AR    R3,R0                                                            
         B     CONB23                                                           
*                                                                               
CONB25   L     R4,AOUT                                                          
         USING CACRECD,R4                                                       
         LA    R5,CACRFST                                                       
         USING PBKELD,R5                                                        
*                                                                               
CONB27   CLI   PBKEL,0                MATCH TO ELEMENT IN RECORD 1              
         BE    CONB31                                                           
         CLI   PBKEL,PBKELQ                                                     
         BE    CONB29                                                           
         IC    R0,PBKLN                                                         
         AR    R5,R0                                                            
         B     CONB27                                                           
*                                                                               
CONB29   CLC   PBKLOW,E.PBKLOW        TEST LOW MONTH                            
         BL    *+10                                                             
         MVC   PBKLOW,E.PBKLOW                                                  
         CLC   PBKHI,E.PBKHI          TEST HIGH MONTH                           
         BH    *+10                                                             
         MVC   PBKHI,E.PBKHI                                                    
         AP    PBKDR,E.PBKDR                                                    
         AP    PBKCR,E.PBKCR                                                    
         B     CONB41                                                           
*                                                                               
CONB31   GOTO1 HELLO,DMCB,(C'P',ACCMST),AOUT,E.PBKELD                           
         CLI   12(R1),0                                                         
         BE    CONB41                                                           
         DC    H'0'                                                             
         DROP  R,E,R4,R5                                                        
         EJECT                                                                  
***********************************************************************         
* CREATE PRIOR BUCKET ELEMENT - IF RECORD IS TOO BIG                  *         
***********************************************************************         
                                                                                
CONB41   L     R4,AOUT                                                          
         USING CACRECD,R4                                                       
         CLC   CACRLEN,=Y(MXRLEN)  TEST RECORD LENGTH                           
         BL    CONBX                                                            
         LA    R5,CACRFST                                                       
         USING PBKELD,R5                                                        
*                                                                               
CONB43   CLI   PBKEL,0             FIND PRIOR BUCKET ELEMENT                    
         BE    CONB45                                                           
         CLI   PBKEL,PBKELQ                                                     
         BE    CONB47                                                           
         IC    R0,PBKLN                                                         
         AR    R5,R0                                                            
         B     CONB43                                                           
*                                                                               
CONB45   LA    R5,ELEMENT          NO PRIOR BUCKET                              
         XC    ELEMENT,ELEMENT                                                  
         MVI   PBKEL,PBKELQ        ADD ONE                                      
         MVI   PBKLN,PBKLNQ                                                     
         MVC   PBKLOW,EFFS                                                      
         ZAP   PBKDR,=P'0'                                                      
         ZAP   PBKCR,=P'0'                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCMST),AOUT,ELEMENT                            
         B     CONB41              LOOP BACK TO THE BEGINNING                   
*                                                                               
CONB47   LA    R3,CACRFST          FIRST BUCKET ELEMENT                         
         USING BUKELD,R3                                                        
CONB49   CLI   BUKEL,BUKELQ        TEST BUCKET ELEMENT                          
         BE    CONB50                                                           
         CLI   0(R3),CACELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CONB49                                                           
CONB50   CLC   PBKLOW,BUKYEAR      GET LOW MONTH                                
         BL    *+10                                                             
         MVC   PBKLOW,BUKYEAR                                                   
         CLC   PBKHI,BUKYEAR       GET HIGH MONTH                               
         BH    *+10                                                             
         MVC   PBKHI,BUKYEAR                                                    
         AP    PBKDR,BUKDR         ADD BUCKETS                                  
         AP    PBKCR,BUKCR                                                      
         MVI   BUKEL,X'FF'         DELETE TO BUCKET                             
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',AOUT),0,0                        
         CLI   12(R1),0                                                         
         BE    CONB41              LOOP BACK TO BEGINNING                       
         DC    H'0'                                                             
*                                                                               
CONBX    DS    0H                                                               
         GOTO1 ADDIN,DMCB,AOUT,OUTDR                                            
         CP    INPDR,OUTDR                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CP    INPCR,OUTCR                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* ADD ALL DEBITS AND CREDITS                                          *         
***********************************************************************         
                                                                                
ADDIN    NTR1  ,                                                                
         L     R2,0(R1)            A(RECORD)                                    
         L     R5,4(R1)            A(ACCUMS)                                    
         USING CACRECD,R2                                                       
         LA    R3,CACRFST                                                       
         SR    R0,R0                                                            
*                                                                               
ADDIN3   CLI   0(R3),0                                                          
         BE    XIT                                                              
         CLI   0(R3),BUKELQ        BUCKET ELEMENT                               
         BNE   ADDIN5                                                           
         USING BUKELD,R3                                                        
         AP    0(L'INPDR,R5),BUKDR                                              
         AP    L'INPDR(L'INPDR,R5),BUKCR                                        
         B     ADDIN7                                                           
*                                                                               
ADDIN5   CLI   0(R3),PBKELQ                                                     
         BNE   ADDIN7                                                           
         USING PBKELD,R3                                                        
         AP    0(L'INPDR,R5),PBKDR                                              
         AP    L'INPDR(L'INPDR,R5),PBKCR                                        
*                                                                               
ADDIN7   IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     ADDIN3                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
                                                                                
RPT      NTR1  ,                                                                
         L     R2,AOUT                                                          
         LA    R7,P                                                             
         USING PLD,R7                                                           
         CLI   LASTACC,X'FF'       TEST EOF                                     
         BNE   RPT3                                                             
         BAS   RE,LDGT             PRINT LAST LEDGER TOTALS                     
         B     XIT                                                              
*                                                                               
RPT3     CLI   RECTYP,ACRTLDG      TEST NEW LEDGER                              
         BNE   RPT5                                                             
         BAS   RE,LDGT             PRINT LEDGER TOTALS                          
         ZAP   LINE,=P'99'                                                      
         BAS   RE,LDGR                                                          
         B     XIT                                                              
*                                                                               
RPT5     CLI   RECTYP,ACRTACTH     TEST ACCOUNT HI                              
         BE    *+12                                                             
         CLI   RECTYP,ACRTACTL     TEST ACCOUNT LO                              
         BNE   XIT                                                              
         BAS   RE,LEVL             TEST LEVEL RECORDS                           
         BAS   RE,ACCT                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* UNIT/LEDGER TOTALS                                                  *         
***********************************************************************         
                                                                                
LDGT     NTR1  ,                                                                
         CLC   LASTACC,SPACES        TEST FIRST LEDGER                          
         BE    LDGTX                                                            
         MVC   PLNME(16),=C'TOTAL FOR LEDGER'                                   
         MVC   PLNME+17(2),LASTACC+1                                            
         LA    R1,LTOT                                                          
         BAS   RE,TOT                                                           
         CLC   LASTACC(2),0(R2)      TEST SAME UNIT                             
         BE    LDGTX                                                            
         MVC   PLNME(14),=C'TOTAL FOR UNIT'                                     
         MVC   PLNME+15(1),LASTACC+1                                            
         LA    R1,UTOT                                                          
         BAS   RE,TOT                                                           
         CLI   LASTACC,X'FF'         TEST EOF                                   
         BNE   LDGTX                                                            
         MVC   PLNME(14),=C'TOTAL FOR FILE'                                     
         LA    R1,CTOT                                                          
         BAS   RE,TOT                                                           
         B     *+10                                                             
LDGTX    MVC   LASTACC,0(R2)         SAVE LEDGER CODE                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ACCOUNT INFO AND BALANCES                                           *         
***********************************************************************         
                                                                                
ACCT     NTR1  ,                                                                
         BAS   RE,SETBOX                                                        
         USING ACTRECD,R2                                                       
         MVC   PLACC,ACTKULA       ACCOUNT CODE TO PRINT                        
         CLI   LEVEL,X'FF'                                                      
         BNE   *+10                                                             
         MVC   PLOTHR(5),=C'ERROR'                                              
         SR    R0,R0                                                            
         LA    R3,ACTRFST                                                       
*                                                                               
ACCT3    CLI   0(R3),0                                                          
         BE    ACCTX                                                            
         CLI   0(R3),NAMELQ        NAME TO PRINT                                
         BNE   ACCT5                                                            
         USING NAMELD,R3                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   PLNME(0),NAMEREC                                                 
         B     ACCT11                                                           
*                                                                               
ACCT5    DS    0H                                                               
*                                                                               
ACCT7    CLI   0(R3),RSTELQ        STATUS INFO                                  
         BNE   ACCT9                                                            
         USING RSTELD,R3                                                        
         MVC   PLFLT1,RSTFILT1                                                  
         MVC   PLFLT2,RSTFILT2                                                  
         MVC   PLFLT3,RSTFILT3                                                  
         MVC   PLFLT4,RSTFILT4                                                  
         MVC   PLFLT5,RSTFILT5                                                  
         OC    PLFLT1(5),SPACES                                                 
         TM    RSTSTAT1,RSTSACIL   LOCKED                                       
         BNO   *+8                                                              
         MVI   PLFLTL,C'L'                                                      
         B     ACCT11                                                           
*                                                                               
ACCT9    CLI   0(R3),ABLELQ        BALANCE INFO                                 
         BNE   ACCT11                                                           
         USING ABLELD,R3                                                        
         MVC   WORK(L'EDMSK),EDMSK                                              
         ED    WORK(L'EDMSK),ABLFRWD                                            
         MVC   PLBF,WORK+1                                                      
         MVC   WORK(L'EDMSK),EDMSK                                              
         ED    WORK(L'EDMSK),ABLDR                                              
         MVC   PLDR,WORK+1                                                      
         MVC   WORK(L'EDMSK),EDMSK                                              
         ED    WORK(L'EDMSK),ABLCR                                              
         MVC   PLCR,WORK+1                                                      
         LA    R1,LTOT                                                          
         AP    0(8,R1),ABLFRWD                                                  
         AP    8(8,R1),ABLDR                                                    
         AP    16(8,R1),ABLCR                                                   
         B     ACCT11                                                           
*                                                                               
ACCT11   SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     ACCT3                                                            
*                                                                               
ACCTX    GOTO1 PRINTER                                                          
         MVC   LASTACC,0(R2)         SAVE ACCOUNT CODE                          
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT TOTALS                                                        *         
***********************************************************************         
                                                                                
TOT      NTR1  ,                                                                
         BAS   RE,SETBOX                                                        
         LA    RE,CTOT                                                          
         CR    R1,RE                                                            
         BE    TOT3                                                             
         LA    R2,L'XTOT(R1)                                                    
         AP    0(8,R2),0(8,R1)     ADD TO NEXT LEVEL                            
         AP    8(8,R2),8(8,R1)                                                  
         AP    16(8,R2),16(8,R1)                                                
*                                                                               
TOT3     MVC   WORK(L'EDMSK),EDMSK                                              
         ED    WORK(L'EDMSK),0(R1)                                              
         MVC   PLBF,WORK+1                                                      
         MVC   WORK(L'EDMSK),EDMSK                                              
         ED    WORK(L'EDMSK),8(R1)                                              
         MVC   PLDR,WORK+1                                                      
         MVC   WORK(L'EDMSK),EDMSK                                              
         ED    WORK(L'EDMSK),16(R1)                                             
         MVC   PLCR,WORK+1                                                      
         ZAP   0(8,R1),=P'0'                                                    
         ZAP   8(8,R1),=P'0'                                                    
         ZAP   16(8,R1),=P'0'                                                   
         GOTO1 PRINTER                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET LEDGER LENGTHS                                                  *         
***********************************************************************         
                                                                                
LDGR     NTR1  ,                                                                
         L     R2,AOUT                                                          
         USING LDGRECD,R2                                                       
         LA    R4,LDGRFST                                                       
         SR    R0,R0                                                            
                                                                                
LDGR5    CLI   0(R4),ACLELQ        X'16' ELEMENT                                
         BE    LDGR9                                                            
         CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     LDGR5                                                            
                                                                                
         USING ACLELD,R4                                                        
LDGR9    MVC   LEVELA,ACLVLEN      LEVEL LENGTHS                                
         MVC   LEVELB,ACLVLEN+(L'ACLVALS)                                       
         MVC   LEVELC,ACLVLEN+(L'ACLVALS*2)                                     
         MVC   LEVELD,ACLVLEN+(L'ACLVALS*3)                                     
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* GET LEVEL FOR THIS ACCOUNT                                          *         
***********************************************************************         
                                                                                
LEVL     NTR1  ,                                                                
         L     R2,AOUT                                                          
         USING ACTRECD,R2                                                       
         LA    R1,L'ACTKACT        GET LENGTH OF ACCOUNT                        
         LA    RF,ACTKACT+L'ACTKACT-1                                           
                                                                                
LEVL3    CLI   0(RF),C' '                                                       
         BH    LEVL5                                                            
         BCTR  RF,0                                                             
         BCT   R1,LEVL3                                                         
         DC    H'0'                                                             
                                                                                
LEVL5    STC   R1,BYTE                                                          
         LA    RF,LEVELA           GET LEVEL FOR THIS ACCOUNT                   
         LA    R0,4                                                             
         CLC   0(1,RF),BYTE        LEDGER LENGTH TO ACCOUNT LENGTH              
         BNL   LEVL7                                                            
         LA    RF,1(RF)                                                         
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
                                                                                
LEVL7    MVC   LEVEL,0(RF)         CURRENT LEVEL LENGTH                         
         CH    R0,=H'4'            FIRST LEVEL                                  
         BE    XIT                                                              
         BCTR  RF,0                GET LENGTH OF HIGHER LEVEL                   
         SR    R1,R1                                                            
         IC    R1,0(RF)            LENGTH OF PREVIOUS                           
         LA    R1,2(R1)            PLUS CUL                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LASTACC(0),ACTKEY   KEY FOR PREVIOUS                             
         BE    XIT                                                              
         MVI   LEVEL,X'FF'         LEVEL ERROR                                  
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* OPEN INPUT FILES AND GET FIRST RECORDS                              *         
***********************************************************************         
                                                                                
OPNF     NTR1  ,                                                                
         OPEN  (TOUT,(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    OUTPUT,DIRO         DIRECTORY OUTPUT                             
         BNO   OPNF1                                                            
         OPEN  (DOUT,(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
OPNF1    LA    R3,FILES            FILE LIST                                    
         SR    R4,R4                                                            
         IC    R4,NFLS             NUMBER OF INPUT FILES                        
         LA    R7,MXFLS            MAX NUMBER OF FILES                          
OPNF3    LM    R5,R6,0(R3)         R5=A(DCB),R6=A(IO AREA)                      
         OPEN  ((R5),(INPUT))                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         GET   (R5),(R6)           GET THE FIRST RECORD                         
         BCTR  R7,0                                                             
         LA    R3,L'FILES(R3)                                                   
         BCT   R4,OPNF3                                                         
         LTR   R7,R7               MAX INPUT FILES                              
         BZ    XIT                                                              
         XC    0(L'FILES,R3),0(R3) CLEAR REMAINING ENTRIES                      
         LA    R3,L'FILES(R3)                                                   
         BCT   R7,*-10                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET NEXT INPUT RECORD                                               *         
***********************************************************************         
                                                                                
GETIN    NTR1  ,                                                                
         LA    R3,FILES            FILE LIST                                    
         SR    R4,R4                                                            
         IC    R4,NFLS             NUMBER OF FILES                              
         SR    R2,R2                                                            
GETIN3   OC    0(4,R3),0(R3)       TEST FILE OPEN                               
         BZ    GETIN9                                                           
         LTR   R2,R2               TEST A(LOW RECORD)                           
         BNZ   GETIN5                                                           
         LR    R2,R3               R2 = A(FILE LIST WITH LOWEST KEY)            
         B     GETIN9                                                           
*                                                                               
GETIN5   ICM   RE,15,4(R2)         RE=A(PREVIOUS LOW KEY)                       
         ICM   RF,15,4(R3)         RF=A(NEW KEY)                                
         CLC   4(L'ACCKEY,RE),4(RF)                                             
         BL    GETIN9                                                           
         LR    R2,R3               R2 = A(FILE LIST WITH NEW LOW KEY)           
*                                                                               
GETIN9   LA    R3,L'FILES(R3)                                                   
         BCT   R4,GETIN3                                                        
         LTR   R2,R2               ANY RECORD FOUND                             
         BZ    XIT                                                              
         ICM   RE,15,4(R2)         MOVE RECORD TO INPUT AREA                    
         SR    R1,R1                                                            
         ICM   R1,3,0(RE)                                                       
         LR    RF,R1                                                            
         L     R0,AINPUT                                                        
         MVCL  R0,RE                                                            
         LM    R5,R6,0(R2)                                                      
         GET   (R5),(R6)                                                        
         LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
GETINX   CLOSE ((R5))                                                           
         XC    0(L'FILES,R2),0(R2)                                              
         LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PRINT AND BOX                                            *         
***********************************************************************         
                                                                                
SETBOX   NTR1  ,                                                                
         L     R6,BOXAREA                                                       
         USING BOXD,R6                                                          
         MVI   MID1,0                                                           
         MVI   MID3,0                                                           
         MVI   MID4,0                                                           
         MVC   MID2,SPACES                                                      
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(L'REPTIT),REPTIT                                           
         LA    R0,L'TITLE                                                       
         GOTO1 CENTER,DMCB,TITLE,(R0)                                           
         LA    R3,MID2+(TITLE-HEADDATE)                                         
         MVC   0(L'COMPNME,R3),COMPNME     COMPANY NAME                         
         LA    R0,L'TITLE                                                       
         GOTO1 CENTER,DMCB,(R3),(R0)                                            
         LA    R7,SUB1             SET HEADLINE                                 
         MVC   PLACC(12),=C'ACCOUNT CODE'                                       
         MVC   PLNME(12),=C'ACCOUNT NAME'                                       
         MVC   PLBF(16),=C'  BALANCE FRWD  '                                    
         MVC   PLDR(16),=C'    DEBITS      '                                    
         MVC   PLCR(16),=C'    CREDITS     '                                    
         MVC   PLFLT1(6),=C'12345L'                                             
         MVC   PLOTHR(5),=C'OTHER'                                              
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXMAXL,57                                                       
         MVI   BOXROWS+7,C'T'                                                   
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+99,C'B'                                                  
         TM    OPTSW,STRIPES                                                    
         BNO   *+12                                                             
         MVI   BOXSHADE,4                                                       
         MVI   BOXSHCH1,X'42'                                                   
*                                                                               
         LA    R7,BOXCOLS                                                       
         MVI   PLL,C'L'                                                         
         MVI   PLC1,C'C'                                                        
         MVI   PLC2,C'C'                                                        
         MVI   PLC3,C'C'                                                        
         MVI   PLC4,C'C'                                                        
         MVI   PLC5,C'C'                                                        
         MVI   PLC6,C'C'                                                        
         MVI   PLR,C'R'                                                         
         B     XIT                                                              
         DROP  R7,R8                                                            
         EJECT                                                                  
***********************************************************************         
* PUT OUT DUPLICATE RECORDS                                           *         
***********************************************************************         
PRNTDUP  NTR1                                                                   
         L     R3,AINPL                                                         
         STCM  R3,15,PDMP2         SET RECORD ADDRESS                           
         SR    RF,RF                                                            
         ICM   RF,3,0(R3)                                                       
         STCM  RF,15,PDMP4         SET RECORD LENGTH                            
         SR    RE,RE                                                            
         IC    RE,RECTYP                                                        
         MH    RE,=Y(RDMPLNQ)                                                   
         A     RE,ARECT            RF=A(RECORD TABLE)                           
         CLI   RDDMPOK-RDMPS(RE),0                                              
         BE    PDUP10                                                           
         SR    RF,RF                                                            
         IC    RF,RDDMPOK-RDMPS(RE)                                             
         BCTR  RF,0                                                             
         STC   RF,RDDMPOK-RDMPS(RE)                                             
         MVC   PCAP1,RDNME-RDMPS(RE)                                            
         MVC   PCAP2,=CL8'* INP * '                                             
         GOTO1 PRNTBL,PDMP                                                      
*                                                                               
PDUP10   L     R3,AOUTL                                                         
         STCM  R3,15,PDMP2         SET RECORD ADDRESS                           
         SR    RF,RF                                                            
         ICM   RF,3,0(R3)                                                       
         STCM  RF,15,PDMP4         SET RECORD LENGTH                            
         SR    RE,RE                                                            
         IC    RE,RECTYP                                                        
         MH    RE,=Y(RDMPLNQ)                                                   
         A     RE,ARECT            RF=A(RECORD TABLE)                           
         CLI   RDDMPOK-RDMPS(RE),0                                              
         BE    XIT                                                              
         SR    RF,RF                                                            
         IC    RF,RDDMPOK-RDMPS(RE)                                             
         BCTR  RF,0                                                             
         STC   RF,RDDMPOK-RDMPS(RE)                                             
         MVC   PCAP1,RDNME-RDMPS(RE)                                            
         MVC   PCAP2,=CL8'* OUT * '                                             
         GOTO1 PRNTBL,PDMP                                                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
PRNTFNL  NTR1                                                                   
         L     R3,AOUTL                                                         
         STCM  R3,15,PDMP2         SET RECORD ADDRESS                           
         SR    RF,RF                                                            
         ICM   RF,3,0(R3)                                                       
         STCM  RF,15,PDMP4         SET RECORD LENGTH                            
         SR    RE,RE                                                            
         IC    RE,RECTYP                                                        
         MH    RE,=Y(RDMPLNQ)                                                   
         A     RE,ARECT            RF=A(RECORD TABLE)                           
         CLI   RDDMPOK-RDMPS(RE),0                                              
         BE    XIT                                                              
         SR    RF,RF                                                            
         IC    RF,RDDMPOK-RDMPS(RE)                                             
         BCTR  RF,0                                                             
         STC   RF,RDDMPOK-RDMPS(RE)                                             
         MVC   PCAP1,RDNME-RDMPS(RE)                                            
         MVC   PCAP2,=CL8'* FNL * '                                             
         GOTO1 PRNTBL,PDMP                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONTANTS, LITERAL POOL, ETC.                                        *         
***********************************************************************         
                                                                                
AINPUT   DC    A(0)                A(CURRENT INPUT AREA)                        
FILES    DS    0XL8                                                             
         DC    A(TACC),A(TIO1)                                                  
         DC    A(TACC2),A(TIO2)                                                 
         DC    A(TACC3),A(TIO3)                                                 
         DC    A(TACC4),A(TIO4)                                                 
         DC    A(TACC5),A(TIO5)                                                 
MXFLS    EQU   (*-FILES)/(L'FILES)                                              
*                                                                               
MXRLEN   EQU   950                 MAX RECORD LENGTH                            
REPTIT   DC    C'ACCOUNT CONVERSION - MERGE REPORT'                             
*                                                                               
LASTKEY  DC    XL42'00'                                                         
LASTACC  DC    CL15' '                                                          
*                                                                               
XTOT     DS    0CL24                                                            
LTOT     DC    3PL8'0'                                                          
UTOT     DC    3PL8'0'                                                          
CTOT     DC    3PL8'0'                                                          
*                                                                               
LEVEL    DC    XL1'00'             LENGTH OF CURRENT LEVEL                      
LEVELA   DC    XL1'00'             LENGTH OF LEVEL A                            
LEVELB   DC    XL1'00'             LENGTH OF LEVEL B                            
LEVELC   DC    XL1'00'             LENGTH OF LEVEL C                            
LEVELD   DC    XL1'00'             LENGTH OF LEVEL D                            
*                                                                               
EDMSK    DC    X'40202020202020202020202020214B202060'                          
*                                                                               
BHI      DC    PL6'99999999999'                                                 
BLO      DC    PL6'-99999999999'                                                
DR       DC    PL8'0'                                                           
CR       DC    PL8'0'                                                           
XDR      DC    PL6'0'                                                           
XCR      DC    PL6'0'                                                           
XBEL     DS    XL(BUKLNQ)          EXTRA BUCKET ELEMENT                         
*                                                                               
INPDR    DC    PL10'0'                                                          
INPCR    DC    PL10'0'                                                          
OUTDR    DC    PL10'0'                                                          
OUTCR    DC    PL10'0'                                                          
*                                                                               
DIRIO    DS    0F                  DIRECTORY IO                                 
         DC    H'60'               RECORD LENGTH                                
         DC    H'0'                                                             
DIRKEY   DS    XL56                DIRECTORY KEY                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DCB'S / IO AREAS                                                    *         
***********************************************************************         
                                                                                
* INPUT ACCOUNT FILE(S)                                                         
                                                                                
TACC     DCB   DDNAME=TACC,DSORG=PS,MACRF=(GM),EODAD=GETINX            *        
               RECFM=VB,LRECL=2048                                              
                                                                                
TACC2    DCB   DDNAME=TACC2,DSORG=PS,MACRF=(GM),EODAD=GETINX           *        
               RECFM=VB,LRECL=2048                                              
                                                                                
TACC3    DCB   DDNAME=TACC3,DSORG=PS,MACRF=(GM),EODAD=GETINX           *        
               RECFM=VB,LRECL=2048                                              
                                                                                
TACC4    DCB   DDNAME=TACC4,DSORG=PS,MACRF=(GM),EODAD=GETINX           *        
               RECFM=VB,LRECL=2048                                              
                                                                                
TACC5    DCB   DDNAME=TACC5,DSORG=PS,MACRF=(GM),EODAD=GETINX           *        
               RECFM=VB,LRECL=2048                                              
                                                                                
* OUTPUT ACCOUNT FILE                                                           
                                                                                
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        *        
               RECFM=VB,LRECL=2048,BLKSIZE=32760                                
*                                                                               
* OUTPUT TEST DIRECTORY                                                         
                                                                                
DOUT     DCB   DDNAME=DOUT,DSORG=PS,MACRF=(PM),                        *        
               RECFM=VB,LRECL=4004,BLKSIZE=32760                                
*                                                                               
         DS    0F                                                               
TIO1     DS    XL2004                                                           
TIO2     DS    XL2004                                                           
TIO3     DS    XL2004                                                           
TIO4     DS    XL2004                                                           
TIO5     DS    XL2004                                                           
         EJECT                                                                  
***********************************************************************         
* COMMON STORAGE                                                      *         
***********************************************************************         
                                                                                
ACNVD    DSECT                                                                  
       ++INCLUDE ACNVWORK                                                       
         EJECT                                                                  
       ++INCLUDE ACNVDSECT                                                      
         EJECT                                                                  
**********************************************************************          
* DSECT FOR A REPORT LINE                                            *          
**********************************************************************          
                                                                                
PLD      DSECT                                                                  
PLL      DS    X                                                                
PLACC    DS    CL14                UNIT/LEDGER ACCOUNT CODE                     
PLC1     DS    X                                                                
PLNME    DS    CL36                NAME                                         
PLC2     DS    X                                                                
PLBF     DS    CL17                BALANCE FORWARD                              
PLC3     DS    X                                                                
PLDR     DS    CL17                DEBITS                                       
PLC4     DS    X                                                                
PLCR     DS    CL17                CREDITS                                      
PLC5     DS    X                                                                
PLFLT1   DS    CL1                 FILTERS                                      
PLFLT2   DS    CL1                                                              
PLFLT3   DS    CL1                                                              
PLFLT4   DS    CL1                                                              
PLFLT5   DS    CL1                                                              
PLFLTL   DS    CL1                 LOCKED                                       
PLC6     DS    X                                                                
PLOTHR   DS    CL17                OTHER INFO                                   
PLR      DS    X                                                                
          EJECT                                                                 
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACNV03    05/01/02'                                      
         END                                                                    
