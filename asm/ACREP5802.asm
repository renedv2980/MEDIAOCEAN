*          DATA SET ACREP5802  AT LEVEL 072 AS OF 05/01/02                      
*PHASE AC5802A,*                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE DATVAL                                                                 
*INCLUDE ACLIST                                                                 
*INCLUDE CONVMOS                                                                
         TITLE 'FILE EMULATION ROUTINES FOR ACREP5802'                          
AC5802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**EM58**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         MVC   ESMODE,MODE                                                      
         MVC   ESADTRAN,ADTRANS                                                 
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    EMREQF                                                           
         CLI   MODE,SBACFRST                                                    
         BE    EMSBAF                                                           
         CLI   MODE,PROCTRNS                                                    
         BE    EMPROT                                                           
         CLI   MODE,SBACLAST                                                    
         BE    EMSBAL                                                           
*                                                                               
         GOTO1 APRGAC58                                                         
*                                                                               
EMEXIT   MVC   MODE,ESMODE                                                      
         MVC   ADTRANS,ESADTRAN                                                 
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SET START/END ACTIVITY DATES                                        *         
***********************************************************************         
         SPACE 1                                                                
EMREQF   XC    EMMONST,EMMONST     LOW START MONTH (DISBURSED)                  
         MVI   EMMONEN,X'FF'       HIGH END MONTH (DISBURSED)                   
         MVC   EMMONEN+1(L'EMMONEN-1),EMMONEN                                   
         XC    EMMONSB,EMMONSB                                                  
         MVI   EMMONEB,X'FF'                                                    
         MVC   EMMONEB+1(L'EMMONEB-1),EMMONEB                                   
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,EMDATEN)                               
         CLC   QSTART,SPACES       TEST START DATE GIVEN                        
         BE    EMREQF02                                                         
         MVC   EMWORK(4),QSTART    SET START YEAR MONTH                         
         MVC   EMWORK+4(2),=C'01'  SET A DAY FOR DATCON                         
         GOTO1 DATCON,DMCB,(0,EMWORK),(1,EMWORK+8)                              
         GOTO1 (RF),(R1),(0,EMWORK),(2,EMMONSB)                                 
         MVC   EMMONST,EMWORK+8                                                 
         MVC   EMMONEN,EMMONST     DEFAULT ONE MONTH START-END                  
         GOTO1 ADDAY,DMCB,EMWORK,EMWORK+8,F'31'                                 
         MVC   EMWORK+12(2),=C'01'                                              
         GOTO1 (RF),(R1),EMWORK+8,EMWORK,F'-1'                                  
         GOTO1 DATCON,DMCB,(0,EMWORK),(2,EMMONEB)                               
*                                                                               
EMREQF02 CLC   QEND,SPACES         TEST END DATE GIVEN                          
         BE    EMREQF04                                                         
         MVC   EMWORK(4),QEND      SET END YEAR/MONTH                           
         MVC   EMWORK+4(2),=C'01'  SET A DAY FOR DATCON                         
         GOTO1 DATCON,DMCB,(0,EMWORK),(1,EMWORK+8)                              
         MVC   EMMONEN,EMWORK+8                                                 
         GOTO1 ADDAY,DMCB,EMWORK,EMWORK+8,F'31'                                 
         MVC   EMWORK+12(2),=C'01'                                              
         GOTO1 (RF),(R1),EMWORK+8,EMWORK,F'-1'                                  
         GOTO1 DATCON,DMCB,(0,EMWORK),(2,EMMONEB)                               
*                                                                               
EMREQF04 CLI   QOPT1,C'U'          TEST UNDISBURSED                             
         BNE   EMREQF06                                                         
         CLC   QEND,SPACES         TEST 'AS AT' DATE GIVEN                      
         BE    EMREQF06                                                         
         GOTO1 DATCON,DMCB,(0,QEND),(2,EMDATEN)                                 
*                                                                               
EMREQF06 GOTO1 APRGAC58            CALL AC58 REPORT                             
         B     EMEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* CLEAR TRANSACTION STACK COUNT                                       *         
***********************************************************************         
         SPACE 1                                                                
EMSBAF   XC    EMTRNSN,EMTRNSN                                                  
         GOTO1 APRGAC58            CALL AC58 REPORT                             
         B     EMEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* STACK CREDIT, OR ADD DEBIT INFORMATION TO CORRECT CREDIT ENTRY      *         
***********************************************************************         
         SPACE 1                                                                
EMPROT   L     R2,ESADTRAN         PROCESS TRANSACTION FROM MONACC              
         GOTO1 SETELS,(R2)         SET A(TRANSACTION ELEMENTS)                  
         SH    R2,=Y(ACCORFST)     R2=A(THIS TRANSACTION)                       
         USING TRNRECD,R2                                                       
         L     RF,ESADTRAN         R1=A(TRANSACTION ELEMENT)                    
         TM    TRNSTAT-TRNELD(RF),TRNSDR TEST DEBIT                             
         BNZ   EMPROT01                                                         
         ICM   RE,15,ATRSELCR      IGNORE CONTRA'D CREDITS                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   TRSMARK-TRSELD(RE),TRSMCOQ                                       
         BE    EMPROTX                                                          
         TM    TRSSTAT-TRSELD(RE),TRSSOFFS                                      
         BNZ   EMPROTX                                                          
         ICM   RF,15,AMPYELCR      TEST/SET A(CR MPYEL)                         
         BZ    EMPROT01                                                         
         USING MPYELD,RF                                                        
         OC    MPYDTE,MPYDTE       TEST PAYMENT DATE SET                        
         BNZ   EMPROT01                                                         
         MVC   MPYDTE,TRNRECD+ACCOUSED                                          
         DROP  RF                                                               
*                                                                               
EMPROT01 OC    ESTRNKEY,ESTRNKEY   TEST FIRST TIME                              
         BZ    *+10                CC EQU                                       
         CLC   ESTRNKEY,TRNKEY     TEST MAJOR KEY CHANGED                       
         MVC   ESTRNKEY,TRNKEY     SAVE THIS KEY                                
         BE    EMPROT06            FIRST TIME, OR MAJOR KEY UNCHANGED           
*                                                                               
         SR    R0,R0               MAJOR KEY CHANGED - PROCESS STACK            
         ICM   R0,1,EMTRNSN                                                     
         BZ    EMPROT06                                                         
         LA    R5,EMTRNS           R5=A(FIRST CR TRANSACTION IN STACK)          
*                                                                               
EMPROT02 CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),X'FF'         TEST CREDIT WANTED                           
         BE    EMPROT05                                                         
         LA    R1,ACCORFST(R5)                                                  
         ST    R1,ADTRANS          SET A(TRANSACTION ELEMENT) FOR AC58          
         USING XVXELD,R1                                                        
         SR    RF,RF                                                            
EMPROT03 IC    RF,XVXLN                                                         
         AR    R1,RF                                                            
         CLI   XVXEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   XVXEL,XVXELQ                                                     
         BNE   EMPROT03                                                         
         TM    XVXIND1,XVXIDROK    TEST THIS IS A SOLO CREDIT                   
         BNZ   EMPROT04                                                         
         OC    ACCOUSED(ACCOULEN,R5),ACCOUSED(R5)                               
         BZ    EMPROT04            NOT USED                                     
         CLC   ACCOUSED(ACCOULEN,R5),EMMONSB                                    
         BL    EMPROT05            USED SOLO CREDIT TOO EARLY                   
         CLI   QOPT1,C'U'          TEST UNDISBURSED                             
         BE    EMPROT04                                                         
         CLC   ACCOUSED(ACCOULEN,R5),EMMONEB                                    
         BH    EMPROT05            USED SOLO CREDIT TOO LATE                    
         DROP  R1                                                               
EMPROT04 MVI   MODE,PROCTRNS       SET MODE FOR AC58                            
         GOTO1 APRGAC58            CALL AC58 REPORT                             
EMPROT05 SR    R1,R1                                                            
         ICM   R1,3,ACCORLEN(R5)                                                
         AR    R5,R1               R5=A(NEXT CR TRANSACTION IN STACK)           
         BCT   R0,EMPROT02                                                      
*                                                                               
         XC    EMTRNSN,EMTRNSN     CLEAR TRANSACTION STACK COUNT                
*                                                                               
EMPROT06 L     R1,ESADTRAN         R1=A(TRANSACTION ELEMENT)                    
         USING TRNELD,R1                                                        
         TM    TRNSTAT,TRNSDR      TEST DEBIT                                   
         BO    EMPROT20                                                         
*                                                                               
         CLC   TRNRECD+ACCOUSED(ACCOULEN),EMDATEN                               
         BE    EMPROT08                                                         
         CLC   TRNRECD+ACCOUSED(ACCOULEN),=X'B644'                              
         BL    EMPROT08                                                         
         XC    TRNRECD+ACCOUSED(ACCOULEN),TRNRECD+ACCOUSED                      
EMPROT08 LA    R5,EMTRNS           CREDIT TRANSACTION PROCESSING                
         SR    R0,R0                                                            
         ICM   R0,1,EMTRNSN        BUMP THROUGH TRANSACTION STACK               
         BZ    EMPROT09                                                         
         SR    R1,R1                                                            
         ICM   R1,3,ACCORLEN(R5)                                                
         AR    R5,R1               R5=A(NEXT TRANSACTION IN STACK)              
         BCT   R0,*-8                                                           
         LR    R0,R5               R0=A(NEXT TRANSACTION IN STACK)              
         SR    R1,R1               SET R1=L'THIS RECORD                         
         ICM   R1,3,TRNRECD+ACCORLEN                                            
         LA    R1,XVXLNQ(R1)       ADD L'(INTERNAL) VOID ELEMENT                
         LA    R1,MPYLNQ+1(R1)     ADD POSSIBLE L'MPYEL +1                      
         AR    R0,R1                                                            
         LA    RF,EMTRNS                                                        
         SR    R0,RF               RE=MAXIMUM LENGTH AFTER THIS ADD             
         C     R0,=A(EMTRNSL)      CHECK THIS WILL FIT                          
         BL    EMPROT09                                                         
         DC    H'0'                CREDIT TRANSACTION STACK FULL                
*                                                                               
EMPROT09 SR    R1,R1               BUMP CREDIT TRANSACTION COUNT                
         IC    R1,EMTRNSN                                                       
         LA    R1,1(R1)                                                         
         STC   R1,EMTRNSN                                                       
         LR    R0,R5               R0=A(NEXT TRANSACTION IN STACK)              
         ICM   R1,3,TRNRECD+ACCORLEN                                            
         LA    RE,TRNRECD          RE=A(THIS TRANSACTION)                       
         LR    RF,R1                                                            
         MVCL  R0,RE               ADD CREDIT TRANSACTION TO STACK              
         LR    R1,R0               R1=A(NEXT STACK POSITION)                    
         BCTR  R1,0                OVERWRITE BINARY ZERO (OLD EOR)              
*                                                                               
         USING XVXELD,R1           BUILD (INTERNAL) VOID ELEMENT                
         XC    XVXEL(XVXLNQ),XVXEL                                              
         MVI   XVXEL,XVXELQ                                                     
         MVI   XVXLN,XVXLNQ                                                     
         ICM   RF,15,ATRSELCR      TEST/SET A(CR TRSEL)                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    TRSSTAT-TRSELD(RF),TRSSVOID                                      
         BZ    *+8                                                              
         OI    XVXIND1,XVXIM0BV    SET MARKER BANK/VOID                         
         ICM   RF,15,AMPYELCR      TEST/SET A(CR MPYEL)                         
         BZ    EMPROT10                                                         
         USING MPYELD,RF                                                        
         CLC   MPYNO,SPACES                                                     
         BNH   *+8                                                              
         OI    XVXIND1,XVXIMPCR    SET PAYMENT DETAILS FROM CREDIT              
         DROP  RF                                                               
EMPROT10 LA    R1,XVXLNQ(R1)                                                    
         MVI   0(R1),0             SET EOR                                      
         SR    RF,RF               ADJUST RECORD LENGTH                         
         ICM   RF,3,ACCORLEN(R5)                                                
         LA    RF,XVXLNQ(RF)                                                    
         STCM  RF,3,ACCORLEN(R5)                                                
*                                                                               
         OC    AMPYELCR,AMPYELCR   TEST MPYEL PRESENT ON CREDIT                 
         BNZ   EMPROT12                                                         
         USING MPYELD,R1                                                        
         XC    MPYELD(MPYLNQ),MPYELD                                            
         MVI   MPYEL,MPYELQ                                                     
         MVI   MPYLN,MPYLNQ                                                     
         MVC   MPYNO,SPACES                                                     
         MVC   MPYDTE,TRNRECD+ACCOUSED                                          
         ZAP   MPYAMNT,=P'0'                                                    
         MVI   MPYELD+MPYLNQ,0     SET EOR                                      
         SR    R1,R1               ADJUST RECORD LENGTH                         
         ICM   R1,3,ACCORLEN(R5)                                                
         LA    R1,MPYLNQ(R1)                                                    
         STCM  R1,3,ACCORLEN(R5)                                                
*                                                                               
EMPROT12 DS    0H                                                               
         B     EMPROTX                                                          
*                                                                               
EMPROT20 SR    R0,R0               DEBIT TRANSACTION PROCESSING                 
         ICM   R0,1,EMTRNSN                                                     
         BZ    EMPROTX             NO CREDITS IN STACK                          
         SR    RF,RF                                                            
         ICM   R1,15,AMPYELDR      TEST/SET A(DEBIT MPYEL)                      
         BZ    EMPROT22                                                         
         USING MPYELD,R1                                                        
         CLI   MPYLN,MPYLN2Q                                                    
         BNE   EMPROT22                                                         
         MVC   EMWORK(L'MPYSUB),MPYSUB                                          
         B     EMPROT24                                                         
         DROP  R1                                                               
*                                                                               
EMPROT22 IC    R1,TRNKSBR          NO MPYSUB - TAKE DEBIT TRNKSBR               
         BCTR  R1,0                REDUCE BY ONE                                
         STC   R1,EMWORK           TO GET PREVIOUS TRANSACTION (CR)             
*                                                                               
EMPROT24 LA    R5,EMTRNS           R5=A(CREDIT TRANSACTION IN STACK)            
EMPROT26 CLC   TRNKSBR-TRNRECD(,R5),EMWORK                                      
         BE    EMPROT38                                                         
         ICM   RF,3,ACCORLEN(R5)                                                
         AR    R5,RF                                                            
         BCT   R0,EMPROT26                                                      
*                                                                               
         LA    R5,EMTRNS                                                        
         ICM   R0,1,EMTRNSN                                                     
EMPROT28 GOTO1 SETELS,ACCORFST(R5)                                              
         OC    ACCOUSED(ACCOULEN,R5),ACCOUSED(R5)                               
         BZ    EMPROT36                                                         
         CLC   ACCOUSED(ACCOULEN,R5),TRNRECD+ACCOUSED                           
         BNL   EMPROT36                                                         
         ICM   RF,15,AXVXELCR                                                   
         TM    XVXIND1-XVXELD(RF),XVXIDROK                                      
         BNZ   EMPROT36                                                         
         ICM   RF,15,AMPYELCR                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   MPYNO-MPYELD(,RF),SPACES                                         
         BH    EMPROT36                                                         
         ICM   RE,15,ATRNELCR                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,ATRNELDR                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CP    TRNAMNT-TRNELD(,RE),TRNAMNT-TRNELD(,RF)                          
         BNE   EMPROT36                                                         
         ICM   RE,15,ATRSELCR                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,ATRSELDR                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TRSDATE-TRSELD(,RE),TRSDATE-TRSELD(RF)                           
         BL    EMPROT38                                                         
EMPROT36 SR    RF,RF                                                            
         ICM   RF,3,ACCORLEN(R5)                                                
         AR    R5,RF                                                            
         BCT   R0,EMPROT28                                                      
         B     EMPROTX                                                          
*                                                                               
EMPROT38 ICM   R4,15,ATRSELDR      R4=A(DEBIT TRSEL)                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING TRSELD,R4                                                        
         GOTO1 SETELS,ACCORFST(R5)                                              
         L     R3,AXVXELCR         R3=A(CREDIT XVXEL)                           
         USING XVXELD,R3                                                        
         CLC   TRSDATE,XVXDATE     TEST LATEST DEBIT ADDED DATE                 
         BL    EMPROTX             DEBIT IS OUT OF DATE                         
         CLC   TRSDATE,EMDATEN     IGNORE DEBITS AFTER 'AS AT' DATE             
         BH    EMPROTX                                                          
         CLI   QOPT1,C'U'          TEST UNDISBURSED VERSION                     
         BE    EMPROT40                                                         
         CLC   TRSPMOS,EMMONST     TEST DEBIT IN RANGE                          
         BL    *+14                                                             
         CLC   TRSPMOS,EMMONEN                                                  
         BNH   EMPROT40                                                         
         TM    XVXIND1,XVXIDROK    TEST PREVIOUS DEBIT MATCHED                  
         BNZ   EMPROTX                                                          
         MVI   TRNKCPY-TRNRECD(R5),X'FF'                                        
         B     EMPROTX             DEBIT OUT OF MOA RANGE                       
*                                                                               
EMPROT40 MVC   XVXDATE,TRSDATE     SET LATEST DEBIT ADDED DATE                  
         CLC   ACCOUSED(ACCOULEN,R2),ACCOUSED(R5)                               
         BNH   EMPROT50                                                         
         ICM   RF,15,ATRNELDR                                                   
         CLI   TRNTYPE-TRNELD(RF),37 TEST BANK/VOID                             
         BNE   *+14                                                             
         XC    ACCOUSED(ACCOULEN,R5),ACCOUSED(R5)                               
         B     EMPROT50                                                         
         GOTO1 DATCON,DMCB,(2,TRSDATE),(0,EMWORK)                               
         GOTO1 ADDAY,DMCB,EMWORK,EMWORK+6,F'-1'                                 
         GOTO1 DATCON,DMCB,(0,EMWORK+6),(2,EMWORK)                              
         MVC   ACCOUSED(ACCOULEN,R5),EMWORK                                     
*                                                                               
EMPROT50 L     R4,ATRSELCR                                                      
         MVC   TRSUDAT,ACCOUSED(R5)                                             
         CLI   QOPT1,C'U'          TEST UNDISBURSED VERSION                     
         BE    EMPROTX                                                          
*                                                                               
EMPROT52 OI    XVXIND1,XVXIDROK    SET DEBIT FOUND AND OK                       
         MVC   TRNKCPY-TRNRECD(,R5),TRNKCPY                                     
         TM    XVXIND1,XVXIMPCR    TEST PAYMENT DETAILS FROM CREDIT             
         BO    EMPROT54                                                         
         CLI   TRSMARK,TRSMBVQ     TEST MARKER BANK/VOID                        
         BE    *+12                                                             
         CLI   TRSMARK,TRSMSBVQ    OR SUBSIDIARY MARKER BANK/VOID               
         BNE   EMPROT54                                                         
         L     RF,AMPYELCR         CLEAR CREDIT MPYEL (EXCEPT DATE)             
         MVC   MPYNO-MPYELD(,RF),SPACES                                         
         ZAP   MPYAMNT-MPYELD(,RF),=P'0'                                        
         MVC   MPYBNK-MPYELD(,RF),SPACES                                        
EMPROT54 NI    TRSMARK,255-TRSMUMQ CLEAR NEGATIVE ACTION BIT                    
         CLI   TRSMARK,TRSMBVQ     TEST VOID/UNVOID                             
         BE    *+12                                                             
         CLI   TRSMARK,TRSMSBVQ    OR SUBSIDIARY VOID/UNVOID                    
         BNE   EMPROT56                                                         
         OI    XVXIND1,XVXIM0BV    SET MARKER VOID                              
         TM    XVXIND1,XVXIMPCR    TEST PAYMENT DETAILS FROM CREDIT             
         BZ    EMPROT58                                                         
         B     EMPROT60                                                         
*                                                                               
EMPROT56 L     R4,ATRNELDR         R4=A(DEBIT TRNEL)                            
         USING TRNELD,R4                                                        
         CLI   TRNTYPE,X'81'       TEST CHEQUE                                  
         BNE   EMPROT60                                                         
         CLI   TRNNARR+40,C'V'     TEST VOID (HISTORIC)                         
         BNE   *+8                                                              
         OI    XVXIND1,XVXIB37V    SET CREDIT IS BT37 VOID                      
         TM    XVXIND1,XVXIMPCR    TEST PAYMENT DETAILS FROM CR MPYEL           
         BO    EMPROT60                                                         
         L     R3,AMPYELCR         R3=A(CREDIT MPYEL)                           
         USING MPYELD,R3                                                        
         CLC   TRNNARR+27(13),SPACES  EXTRACT BANK (HISTORIC)                   
         BNH   *+14                                                             
         MVC   MPYBNK,TRNNARR+27                                                
         MVI   MPYBNK+L'MPYBNK-1,C' '                                           
         CLC   TRNNARR(6),SPACES   EXTRACT CHEQUE NUMBER (HISTORIC)             
         BNH   *+10                                                             
         MVC   MPYNO,TRNNARR                                                    
         CLC   TRNNARR+6(8),SPACES EXTRACT CHEQUE DATE (HISTORIC)               
         BNH   EMPROT58                                                         
         LA    RF,TRNNARR+6                                                     
         GOTO1 =V(DATVAL),DMCB,(0,(RF)),EMWORK                                  
         OC    DMCB(4),DMCB                                                     
         BZ    EMPROT58            INVALID DATE                                 
         GOTO1 DATCON,DMCB,(0,EMWORK),(2,MPYDTE)                                
*                                                                               
EMPROT58 ICM   RF,15,AMPYELDR      EXTRACT FROM DEBIT MPYEL, IF FOUND           
         BZ    EMPROT60                                                         
         L     R3,AMPYELCR         R3=A(CREDIT MPYEL)                           
         CLC   MPYNO-MPYELD(,RF),SPACES                                         
         BNH   *+10                                                             
         MVC   MPYNO,MPYNO-MPYELD(RF)                                           
         OC    MPYDTE-MPYELD(,RF),MPYDTE-MPYELD(RF)                             
         BZ    *+10                                                             
         MVC   MPYDTE,MPYDTE-MPYELD(RF)                                         
         CLC   MPYBNK-MPYELD(,RF),SPACES                                        
         BNH   *+10                                                             
         MVC   MPYBNK,MPYBNK-MPYELD(RF)                                         
*                                                                               
EMPROT60 DS    0H                                                               
*                                                                               
EMPROTX  B     EMEXIT              EXIT FROM EMULATOR TO MONACC                 
*                                                                               
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTION STACK AT LAST FOR SUB-ACCOUNT                   *         
***********************************************************************         
         SPACE 1                                                                
EMSBAL   SR    R0,R0                                                            
         ICM   R0,1,EMTRNSN                                                     
         BZ    EMSBALX                                                          
         LA    R3,EMTRNS           R3=A(FIRST CR TRANSACTION IN STACK)          
*                                                                               
EMSBAL02 CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'FF'         TEST CREDIT WANTED                           
         BE    EMSBAL04                                                         
         LA    R1,ACCORFST(R3)                                                  
         ST    R1,ADTRANS          SET A(TRANSACTION ELEMENT) FOR AC58          
         MVI   MODE,PROCTRNS       SET MODE FOR AC58                            
         GOTO1 APRGAC58            CALL AC58 REPORT                             
EMSBAL04 SR    R1,R1                                                            
         ICM   R1,3,ACCORLEN(R3)                                                
         AR    R3,R1               R3=A(NEXT CR TRANSACTION IN STACK)           
         BCT   R0,EMSBAL02                                                      
*                                                                               
         XC    EMTRNSN,EMTRNSN     CLEAR TRANSACTION STACK COUNT                
*                                                                               
EMSBALX  B     EMEXIT              EXIT FROM EMULATOR TO MONACC                 
         EJECT                                                                  
***********************************************************************         
* SET ELEMENT ADDRESSES FOR CREDIT OR DEBIT TRANSACTION               *         
*                                                                     *         
* NTRY - R1=A(TRANSACTION ELEMENT)                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R1                                                        
SETELS   STM   RE,R1,SAVERER1                                                   
         CLI   TRNEL,TRNELQ        ENSURE THIS IS A TRANSACTION                 
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,AELSCR                                                        
         TM    TRNSTAT,TRNSDR      TEST DEBIT/CREDIT                            
         BZ    *+8                                                              
         LA    RF,AELSDR                                                        
         XC    0(AELSCRL,RF),0(RF) CLEAR CREDIT/DEBIT ELEMENTS                  
         ST    R1,TRNELDSP(RF)                                                  
*                                                                               
         SR    R0,R0                                                            
SETELS02 IC    R0,TRNLN            PASS TRANSACTION ELEMENT                     
         AR    R1,R0                                                            
         CLI   TRNEL,0             TEST EOR                                     
         BE    SETELSX                                                          
*                                                                               
         CLI   TRNEL,TRSELQ        STATUS ELEMENT                               
         BNE   *+12                                                             
         ST    R1,TRSELDSP(RF)                                                  
         B     SETELS02                                                         
*                                                                               
         CLI   TRNEL,MPYELQ        PAYMENT ELEMENT                              
         BNE   *+12                                                             
         ST    R1,MPYELDSP(RF)                                                  
         B     SETELS02                                                         
*                                                                               
         CLI   TRNEL,SCIELQ        SUBSIDIARY CASH ELEMENT                      
         BNE   *+12                                                             
         ST    R1,SCIELDSP(RF)                                                  
         B     SETELS02                                                         
*                                                                               
         CLI   TRNEL,XVXELQ        (INTERNAL) VOID ELEMENT                      
         BNE   *+12                                                             
         ST    R1,XVXELDSP(RF)                                                  
         B     SETELS02                                                         
*                                                                               
         B     SETELS02                                                         
*                                                                               
SETELSX  LM    RE,R1,SAVERER1                                                   
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS, CONSTANTS, STORAGE                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
APRGAC58 DC    A(PRGAC58)          A(AC5802)                                    
SAVERER1 DC    4A(0)               SAVED RE,RF,R0,R1                            
*                                                                               
AELSCR   DS    0A                  A(CREDIT TRANSACTION ELEMENTS)               
TRNELDSP EQU   *-AELSCR                                                         
ATRNELCR DC    A(0)                A(TRNEL)                                     
MPYELDSP EQU   *-AELSCR                                                         
AMPYELCR DC    A(0)                A(MPYEL)                                     
SCIELDSP EQU   *-AELSCR                                                         
ASCIELCR DC    A(0)                A(SCIEL)                                     
TRSELDSP EQU   *-AELSCR                                                         
ATRSELCR DC    A(0)                A(TRSEL)                                     
XVXELDSP EQU   *-AELSCR                                                         
AXVXELCR DC    A(0)                A(XVXEL)                                     
AELSCRL  EQU   *-AELSCR                                                         
*                                                                               
AELSDR   DS    0A                  A(DEBIT TRANSACTION ELEMENTS)                
ATRNELDR DC    A(0)                A(TRNEL)                                     
AMPYELDR DC    A(0)                A(MPYEL)                                     
ASCIELDR DC    A(0)                A(SCIEL)                                     
ATRSELDR DC    A(0)                A(TRSEL)                                     
AXVXELDR DC    A(0)                A(XVXEL)                                     
AELSDRL  EQU   *-AELSDR                                                         
*                                                                               
ESADTRAN DC    A(0)                SAVED MONACC ADTRANS                         
ESMODE   DC    X'00'               SAVED MONACC MODE                            
*                                                                               
ESTRNKEY DC    XL(TRNKEND-1)'00'   LAST MONACC PROCTRNS KEY                     
*                                                                               
EMMONST  DC    XL2'00'             START MONTH FOR DEBITS                       
EMMONEN  DC    XL2'00'             END MONTH FOR DEBITS                         
EMMONSB  DC    XL2'00'             START MONTH BINARY COMPRESSED                
EMMONEB  DC    XL2'00'             END MONTH BINARY COMPRESSED                  
EMDATEN  DC    XL2'00'             END DATE BINARY COMPRESSED                   
*                                                                               
EMWORK   DC    24X'00'             WORK AREA                                    
*                                                                               
EMTRNSN  DC    X'00'               NUMBER OF TRANSACTIONS STACKED               
EMTRNS   DC    (128*800)X'00'      ALLOW 128*800 BYTE TRANSACTIONS              
EMTRNSL  EQU   *-EMTRNS                                                         
         TITLE 'CASH DISBURSED AND UNDISBURSED REPORTS'                         
PRGAC58  NMOD1 0,**AC58**,R9,R8,RR=R5                                           
         LA    RC,SPACEND                                                       
         USING AC58D,RC                                                         
         ST    R5,RELO                                                          
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LEDF                                                             
         CLI   MODE,LEVAFRST                                                    
         BE    LEVAF                                                            
         CLI   MODE,PROCACC                                                     
         BE    PROCA                                                            
         CLI   MODE,SBACFRST                                                    
         BE    SBACF                                                            
         CLI   MODE,PROCTRNS                                                    
         BE    PROCTR                                                           
         CLI   MODE,ACCLAST                                                     
         BE    ACCL                                                             
         CLI   MODE,LEVALAST                                                    
         BE    LEVAL                                                            
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              RUN FIRST                                              *         
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
         ZIC   R2,MAXLINES                                                      
         SH    R2,=H'4'                                                         
         STC   R2,MYMAX                                                         
         L     R2,=A(SAVERC)       SAVE REG C                                   
         A     R2,RELO                                                          
         ST    RC,0(R2)                                                         
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     R2,=V(HOOK)                                                      
         A     R2,RELO                                                          
         ST    R2,HEADHOOK         STORE ADDR OF BOX SET-UP                     
*                                                                               
         USING OFFLD,R7                                                         
RUNF0    L     R7,OFFLIST                                                       
         L     R2,ADCOMP           BUILD OFFICE LIST FROM 2D                    
         MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(1),0(R2)                                                   
         MVC   MYKEY+1(2),=C'2D'                                                
*                                                                               
         LR    R3,R2               ADDR OF COMP RECORD INTO R3                  
         MVI   ELCODE,X'10'        GET COMPANY ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,MYKEY+3                                                       
         TM    ACMPSTA4-ACCOMPD(R3),X'01' TEST TWO BYTE OFFICE CODE             
         BZ    *+8                                                              
         LA    R4,MYKEY+4                                                       
         MVC   SAVEKEY,MYKEY                                                    
*                                                                               
RUNF10   MVC   OFFL(OFFLLNQ),SPACES                                             
         ZIC   R3,0(R4)                   ADD 1 TO 1ST BYTE (R4) IF             
         AH    R3,=H'1'                   1 BYTE OFFICE CODE, OR                
         STC   R3,0(R4)                   TO 2ND BYTE (R4) IF 2 BYTE            
*                                         CODE                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',MYKEY,MYKEY                      
         CLC   SAVEKEY(3),MYKEY                                                 
         BNE   RUNF20                                                           
         CLC   MYKEY+5(37),SPACES                                               
         BNE   RUNF20                                                           
         MVC   OFFL,MYKEY+3               OFFICE CODE                           
         LA    R2,MYKEY                                                         
         BAS   RE,NAMOUT                                                        
         MVC   OFFLNAME,WORK       OFFICE NAME                                  
         LA    R7,OFFLLNQ(R7)                                                   
         B     RUNF10              NEXT OFFICE                                  
*                                                                               
RUNF20   MVI   OFFL,X'FE'                                                       
         MVC   OFFLNAME,=CL36'OTHERS'                                           
         LA    R7,OFFLLNQ(R7)                                                   
         MVI   OFFL,X'FF'                                                       
         MVC   OFFLNAME,SPACES                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              REQUEST FIRST                                          *         
***********************************************************************         
*                                                                               
REQF     DS    0H                                                               
         MVC   PZR(64),=8PL8'0'                                                 
         MVI   MEDSW,C'N'                                                       
         MVI   LISTSW,C'N'                                                      
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         ICM   R3,15,VLISTREC      IS THERE A LIST RECORD                       
         BZ    REQF2                                                            
         MVI   LISTSW,C'Y'         YES                                          
         MVI   ELCODE,X'1E'        IS IT A CLIENT OR A LEDGER LIST              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                BAD LIST RECORD                              
         USING ACLISTD,R3                                                       
         CLI   ACLITYPE,C'L'                                                    
         BNE   *+8                                                              
         MVI   LISTSW,C'L'         MARK LEDGER LIST                             
*                                                                               
REQF2    GOTO1 DATCON,DMCB,(4,RCDATE),(0,TODAY)                                 
         GOTO1 (RF),(R1),,(2,TODAY2)                                            
         GOTO1 (RF),(R1),,(1,TODAYP)                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
         LA    R3,BUCK             CLEAR ACCUMULATOR BUCKETS                    
         LA    R5,BUCKEND                                                       
         BAS   RE,CLRACM                                                        
*                                                                               
         L     R2,ADCOMP           READ LEDGER FOR PRODUCTION                   
         MVC   MYKEY,SPACES        TO GET LEVEL A LENGTH                        
         MVC   MYKEY(1),0(R2)      COMPANY CODE                                 
         L     R2,ADCMPEL                                                       
         USING ACCOMPD,R2                                                       
         MVC   MYKEY+1(2),ACMPJOB   UNIT/LEDGER FOR PRODUCTION                  
         MVC   SAVEKEY,MYKEY                                                    
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',MYKEY,MYKEY                      
         CLC   SAVEKEY,MYKEY                                                    
         BNE   CL2                                                              
*                                                                               
         LA    R3,MYKEY                                                         
         MVI   ELCODE,X'16'        HEIRARCHY ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R2,2(R3)            LENGTH OF ACCOUNT LEVEL 1                    
         BCTR  R2,0                FOR EX                                       
         STC   R2,STCLTLNG                                                      
*                                                                               
CL2      XC    MEDCNT,MEDCNT       MEDIA COUNT                                  
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         L     R7,OFFMED           OFFICE/MEDIA TABLE                           
         XC    0(8,R7),0(R7)                                                    
         CLI   QOPT1,C'U'                                                       
         BE    UD102                                                            
         MVC   ACTVST,=X'0000'     DATES FOR DISBURSED                          
         MVC   ACTVED,=X'FFFF'                                                  
         CLC   QSTART,SPACES                                                    
         BE    CD101                                                            
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WRK2)                                    
         MVC   ACTVST,WRK2                                                      
         CLC   QEND,SPACES                                                      
         BNE   CD102                                                            
         MVC   ACTVED,ACTVST       DEFAULT - REPORT 1 MONTH                     
         B     EXIT                                                             
*                                                                               
CD101    CLC   QEND,SPACES                                                      
         BE    EXIT                                                             
CD102    MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(1,WRK2)                                    
         MVC   ACTVED,WRK2                                                      
         B     EXIT                                                             
*                                  DATES FOR UNDISBURSED                        
*                                                                               
*     END DATE FOR UNDISBURSED MUST BE MM/DD/YY(IF USED)                        
*     AND MEANS AS AT DAY                                                       
*                                                                               
UD102    CLI   QSTART,C' '                                                      
         BNE   *+10                                                             
         MVC   QSTART(4),QEND      IF NO START DATE - USE AS AT YYMM            
         CLC   QEND,SPACES                                                      
         BE    UD103                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(2,TODAY2)                                  
         GOTO1 DATCON,DMCB,(0,QEND),(1,TODAYP)                                  
*                                                                               
UD103    LA    R5,32                                                            
         ST    R5,INCR                                                          
         LA    R5,9                OUTPUT FORMAT FOR DATCON MMM/YY              
         CLC   QSRTAREA(2),SPACES  INCREMENT BLANK                              
         BE    UD105                                                            
         PACK  DUB,QSRTAREA(2)                                                  
         CVB   R7,DUB                                                           
         ST    R7,INCR             SAVE DAYS TO INCREMENT                       
         BCTR  R5,0                OUTPUT FORMAT 8 FOR DATCON MMMDD/YY          
         B     UD110                                                            
         SPACE 1                                                                
UD105    CLC   QSTART+4(2),SPACES  DAY SPECIFIED                                
         BE    UD110               NO, DAY FORMAT IS MMM/YY                     
         BCTR  R5,0                ELSE, MMMDD/YY                               
UD110    MVC   WORK(6),QSTART                                                   
         CLC   WORK(6),SPACES                                                   
         BNE   UD115                                                            
         MVC   WORK(4),TODAY                                                    
         SPACE 1                                                                
UD115    CLC   WORK+4(2),SPACES    DD                                           
         BNE   *+10                                                             
         MVC   WORK+4(2),=C'01'                                                 
         PACK  DUB,WORK+4(2)                                                    
         CVB   R7,DUB                                                           
         ST    R7,FULL             DAY OF MONTH                                 
         SPACE 1                                                                
         LA    R3,DATPK            PACKED DATES                                 
         LA    R2,4                                                             
         L     R4,INCR                                                          
         CLC   QSRTAREA(2),SPACES  WAS INCREMENT INPUT                          
         BNE   UD116               BRANCH IF IT WAS                             
         CLC   QSTART+4(2),SPACES                                               
         BE    UD117               NO DAY INPUT                                 
         B     UD118                                                            
         SPACE 1                                                                
*INCREMENT INPUT                                                                
UD116    GOTO1 DATCON,DMCB,(0,WORK),(1,(R3))                                    
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R4)      ADD INCR DAYS                   
         MVC   WORK(6),WORK+6                                                   
         LA    R3,3(R3)                                                         
         BCT   R2,UD116                                                         
         B     UD120                                                            
         SPACE 1                                                                
*NO INCREMENT - DAY NOT INPUT-  ALWAYS STARTS AT DAY 1 OF MONTH                 
UD117    GOTO1 DATCON,DMCB,(0,WORK),(1,(R3))                                    
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'31'     ADD 31 DAYS                     
         MVC   WORK(6),WORK+6                TO GET TO NEXT MONTH               
         MVC   WORK+4(2),=C'01'               MAKE IT FIRST OF MONTH            
         LA    R3,3(R3)                                                         
         BCT   R2,UD117                                                         
         B     UD120                                                            
         SPACE 1                                                                
*NO INCREMENT - DAY INPUT                                                       
UD118    GOTO1 DATCON,DMCB,(0,WORK),(1,(R3))                                    
         MVC   WORK+4(2),=C'01'    SET FIRST OF MONTH                           
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'31'   GET NEXT MONTH                    
         GOTO1 DATCON,DMCB,(0,WORK+6),(8,WORK+12) CONVERT TO MMMDD/YY           
         MVC   WORK+15(2),QSTART+4  REQUEST DAY                                 
*                                                                               
UD118B   GOTO1 DATVAL,DMCB,(0,WORK+12),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BNZ   UD119               VALID DATE                                   
         PACK  DUB,WORK+15(2)      REQUEST DAY                                  
         SP    DUB,=P'1'           BACK OFF 1 DAY                               
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   WORK+15(2),DUB+1    NEW DAY TO MMMDD/YY FIELD                    
         B     UD118B              NOW SEE IF THIS IS VALID                     
*                                                                               
UD119    LA    R3,3(R3)                                                         
         BCT   R2,UD118                                                         
         SPACE 1                                                                
UD120    MVC   DATELN,SPACES                                                    
         LA    R3,DATELN                                                        
         LA    R4,DATPK            OUTPUT LINE                                  
         LA    R2,4                                                             
         SPACE 1                                                                
UD127    ST    R3,DMCB+4                                                        
         STC   R5,DMCB+4           OUTPUT FORMAT                                
         CLI   DMCB+4,8            MMMDD/YY                                     
         BE    UD130               YES, OK TO CONVERT                           
         ST    R3,DMCB+4           LINE+2                                       
         MVI   DMCB+4,9            FORMAT IS MMM/YY                             
*                                                                               
UD130    GOTO1 DATCON,DMCB,(1,(R4))                                             
         LA    R3,10(R3)                                                        
         CLI   DMCB+4,8       MMMDD/YY                                          
         BE    UD132                                                            
         MVI   2(R4),X'FF'    FOR MMM/YY FORCE DAY HIGH                         
         LA    R3,2(R3)                                                         
         B     UD134                                                            
*                                                                               
UD132    LA    R3,2(R3)                                                         
UD134    LA    R4,3(R4)                                                         
         BCT   R2,UD127                                                         
         MVC   DATELN2,SPACES                                                   
         CLI   DMCB+4,8            IF NO DAY                                    
         BNE   EXIT                                                             
         MVC   DATELN2(24),DATELN+11                                            
         MVC   DATELN+11(26),SPACES                                             
         LA    R3,2                                                             
         LA    R4,DATPK                                                         
         LA    R5,DATELN+10                                                     
*                                                                               
UPD136   GOTO1 DATCON,DMCB,(1,0(R4)),(0,WORK)                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,1                                         
         GOTO1 DATCON,DMCB,(0,WORK+6),(4,0(R5))                                 
         MVC   6(2,R5),=C'TO'                                                   
         LA    R5,12(R5)                                                        
         LA    R4,3(R4)                                                         
         BCT   R3,UPD136                                                        
         MVC   DATELN+32(12),SPACES                                             
         GOTO1 DATCON,DMCB,(1,0(R4)),(0,WORK)                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,1                                         
         GOTO1 DATCON,DMCB,(0,WORK+6),(8,DATELN+32)                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              LEDGER FIRST                                           *         
***********************************************************************         
*                                                                               
         USING ACKEYD,RF                                                        
LEDF     DS    0H                                                               
         L     RF,ADLEDGER                                                      
         MVI   EXPLEDG,C'Y'                                                     
         CLI   ACKEYACC+2,C'X'     SET FLAG FOR EXPENSE LEDGER                  
         BE    LEDF10                                                           
         CLI   ACKEYACC+2,C'Y'                                                  
         BE    LEDF10                                                           
         MVI   EXPLEDG,C'N'                                                     
*                                                                               
         USING ACHEIRD,RF                                                       
LEDF10   L     RF,ADLDGHIR         HEIRARCHY ELEMENT                            
         MVI   ONELEVEL,C'N'                                                    
         CLI   ACHRLEVA,12         SET SWITCH IF LEDGER IS 1 LEVEL              
         BNE   EXIT                                                             
         MVI   ONELEVEL,C'Y'                                                    
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
*              LEVEL A FIRST                                          *         
***********************************************************************         
*                                                                               
         USING MEDD,R4                                                          
LEVAF    DS    0H                                                               
         L     R4,MEDTAB           FIND CORRECT OFFSET FOR NEXT ENTRY           
         L     R1,MEDCNT                                                        
         MH    R1,=Y(MEDLNQ)                                                    
         AR    R4,R1                                                            
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BNE   LEVAF2                                                           
         DC    H'0'                                                             
         DC    C'MEDTAB FULL'                                                   
*                                                                               
LEVAF2   L     R2,ADHEIRA                                                       
         BAS   RE,NAMOUT                                                        
         MVC   MEDCODE,3(R2)       MEDIA CODE                                   
         MVC   MEDNAME,WORK        NAME                                         
         MVC   MEDACCUM(64),PZR    AMOUNT FIELDS                                
         MVC   OFFLN+2(1),3(R2)    MEDIA CODE TO OFFICE LINE                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PROC ACCOUNT                                           *         
***********************************************************************         
*                                                                               
PROCA    DS    0H                                                               
         L     R2,ADACC                                                         
         BAS   RE,NAMOUT                                                        
         MVC   ACCNAM,WORK                                                      
         MVC   ACCNO,SPACES                                                     
         L     R2,ADACC                                                         
         MVC   ACCNO(12),3(R2)     ACCOUNT NUMBER                               
         MVC   PAYEE,SPACES                                                     
         MVC   PAYEE(49),ACCNO     NUMBER AND NAME                              
         GOTO1 SQUASHER,DMCB,PAYEE,50                                           
         MVI   ACCTFRST,C'Y'                                                    
         MVI   SUBAFRST,C'Y'                                                    
         MVI   REPNAME,C'Y'                                                     
         MVI   ACT,C'N'                                                         
         MVC   S,SPACES                                                         
         XC    CHACTL,CHACTL       NO BREAK ON FIRST                            
         ZAP   PRTCOUNT,=P'0'      COUNT OF # OF PRNTED LINES FOR VEND          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              SUB ACCOUNT FIRST                                      *         
***********************************************************************         
*                                                                               
SBACF    DS    0H                                                               
         L     R2,ADSUBAC                                                       
         MVC   SBACCNO,SPACES                                                   
         MVC   SBACCNM,SPACES                                                   
         USING TRSUBHD,R2                                                       
         MVC   SBACCNO(15),TRSBACNT                                             
         ZIC   R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         BM    EXIT                                                             
         EXMVC R1,SBACCNM,TRSBNAME                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              SUB ACCOUNT FIRST                                      *         
***********************************************************************         
*                                                                               
PROCTR   DS    0H                                                               
         CLI   QOPT1,C'U'                                                       
         BE    UD500                                                            
*                                                                               
* PROCTRNS FOR DISBURSED                                                        
*                                                                               
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   0(R4),X'44'                                                      
         BNE   EXIT                                                             
*                                                                               
         MVC   OFFLN+03(64),PZR                                                 
         OC    TRNSANAL,SPACES                                                  
         MVC   OFFLN(2),TRNSANAL                                                
*                                                                               
         MVC   RQOFFICE,SPACES                                                  
         MVC   RQOFFICE(1),QOPT5                                                
         CLI   QOPT5,C' '          OFFICE FILTERING                             
         BNE   PROCTRA5            NO FILTERING                                 
         MVC   RQOFFICE,QTRNSFLT                                                
         CLC   QTRNSFLT,SPACES                                                  
         BNE   PROCTRA5                                                         
         MVC   RQOFFICE,QOFFICE                                                 
         CLC   QOFFICE,SPACES                                                   
         BNE   PROCTRA5                                                         
         MVC   RQOFFICE,SPACES                                                  
PROCTRA5 CLC   RQOFFICE,SPACES                                                  
         BE    PROCTRD             NO OFFICE SPECIFIED                          
         CLC   RQOFFICE,TRNSANAL                                                
         BE    PROCTRD             DID NOT MATCH OFFICE                         
*                                                                               
         TM    RQOFFICE,X'40'                                                   
         BO    EXIT                DID NOT MATCH OFFICE                         
         MVC   BYTE2,RQOFFICE                                                   
         OI    BYTE2,X'40'                                                      
         CLC   BYTE2,TRNSANAL                                                   
         BE    EXIT                IF OFFICE MATCHES - EXCLUDE ITEM             
*                                                                               
PROCTRD  CLI   LISTSW,C'L'                                                      
         BE    PROCTR2             SKIP QSELECT LOGIC IF LEDGER LIST            
         CLC   QSELECT(3),SPACES                                                
         BE    PROCTR2                                                          
         L     R2,ADSUBAC                                                       
         MVC   THREE,TRSBACNT+3                                                 
         CLI   QLEDGER,C'W'                                                     
         BE    PROCTR1                                                          
         CLI   QLEDGER,C'V'                                                     
         BE    PROCTR1                                                          
         MVC   THREE,TRSBACNT+12                                                
*                                                                               
PROCTR1  CLI   LISTSW,C'Y'         IS THERE A CLIENT LIST                       
         BNE   PROCTR1B                                                         
         L     RF,VEXTRAS          YES                                          
         USING RUNXTRAD,RF                                                      
         GOTO1 ACLIST,DMCB,VLISTREC,THREE DO WE WANT THIS ONE                   
         CLI   DMCB,0                                                           
         BNE   *+6                                                              
         DC    H'0'                BAD LIST RECORD                              
         CLI   DMCB,C'E'                                                        
         BE    EXIT                EXCLUDE THIS ONE                             
         B     PROCTR2                                                          
*                                                                               
PROCTR1B CLC   QSELECT(3),THREE    NO LIST RECORD - IT'S A CLIENT               
         BE    PROCTR2                                                          
         TM    QSELECT,X'40'                                                    
         BO    EXIT                                                             
         NI    THREE,X'BF'                                                      
         CLC   QSELECT(3),THREE                                                 
         BE    EXIT                                                             
*                                                                               
PROCTR2  DS    0H                                                               
         LR    R3,R4                                                            
         MVI   ELCODE,X'60'        TRANSACTION STATUS ELEMENT                   
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R3,AD60EL                                                        
         USING TRSTATD,R3                                                       
         TM    TRSTATUS,X'08'      SKIP OFFSETS                                 
         BO    EXIT                                                             
*                                                                               
         LR    R3,R4               LOOK FOR (INTERNAL) VOID ELEMENT             
         MVI   ELCODE,XVXELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   *+8                                                              
         ST    R3,ADXVEL                                                        
*                                                                               
         MVI   SW64,C'N'           LOOK FOR MANUAL CHEQUE ELEMENT               
         LR    R3,R4                                                            
         MVI   ELCODE,X'64'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   PROCTR3                                                          
         USING TRMAPD,R3                                                        
         OC    TRMAPDTE,TRMAPDTE                                                
         BZ    PROCTR3                                                          
         DROP  R3                                                               
         MVI   SW64,C'Y'                                                        
         ST    R3,AD64EL                                                        
*                                                                               
PROCTR3  LR    RF,R4                                                            
         SH    RF,DATADISP                                                      
         USING ACKEYD,RF                                                        
         OC    ACDTUSED,ACDTUSED   DISBURSED                                    
         BZ    EXIT                NO, EXIT                                     
         TM    TRNSSTAT,X'20'      REVERSAL                                     
         BO    EXIT                                                             
         CLI   TRNSTYPE,X'81'      CHECK ELEMENT                                
         BE    CD550               PRINT CHECK INFO                             
         TM    TRNSSTAT,X'80'      MUST BE A CREDIT TO BE                       
         BNZ   EXIT                A DISBURSEMENT                               
         L     R1,AD60EL                                                        
         TM    TRSSTAT-TRSELD(R1),TRSSACHQ TEST CHECK PROGRAM                   
         BZ    PROCTR4                                                          
*        CLI   SW64,C'Y'           SKIP NEXT TEST IF MANUAL CHECK               
*        BE    PROCTR4                                                          
         CLC   ACDTUSED,TODAY2     IF NOT OLD I WON'T HAVE A CHECK              
         BNL   EXIT                ITEM TO USE FOR MONTH OF SERVICE.            
*                                                                               
PROCTR4  CLI   QOPT4,C'A'          APPROVED ITEMS ONLY                          
         BNE   PROCTR0                                                          
         TM    TRNSSTAT,X'02'                                                   
         BO    *+12                                                             
         MVI   CKNXT,C'N'          DON'T WANT CHECK ELEMENT                     
         B     EXIT                                                             
*                                                                               
PROCTR0  CLI   CHACTL,0            ANYTHING IN CONTROL                          
         BE    CD510               NO BREAK                                     
         BAS   RE,CHA              GET A CHANGE CODE IN CHACD                   
         CLI   CHACD,0                                                          
         BE    CD520               NO CHANGE                                    
*                                                                               
         TM    CHACD,1             CLIENT CHANGE                                
         BZ    *+8                                                              
         BAS   RE,CLTL             DO CLIENT TOTAL                              
*                                                                               
         TM    CHACD,2             PUB CHANGE PRINT                             
         BZ    *+8                                                              
         BAS   RE,PUBTL                                                         
*                                                                               
         TM    CHACD,4             STA CHANGE SPOT                              
         BZ    *+8                                                              
         BAS   RE,STATL            STATION TOTAL                                
*                                                                               
         TM    CHACD,1             CLIENT CHANGE                                
         BZ    CD510                                                            
         LA    R3,ACCUM            CLEAR ACCUM AND CLITOT                       
         LA    R5,STATOT-1                                                      
         BAS   RE,CLRACM                                                        
*                                                                               
CD510    BAS   RE,BLDCNTL          BUILD NEW CONTROL                            
*                                                                               
CD520    MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   STLINE,SPACES                                                    
         L     R3,ADACC                                                         
         L     R4,ADTRANS                                                       
         CLI   2(R3),C'V'          PRODUCTION                                   
         BE    PRODUC                                                           
         CLI   2(R3),C'W'          CANADIAN PRODUCTION                          
         BE    PRODUC                                                           
         CLI   2(R3),C'X'                                                       
         BE    PRODUC                                                           
         CLI   2(R3),C'Y'                                                       
         BE    PRODUC                                                           
         CLC   CHACTL+3(11),SPACES                                              
         BE    SPTRG               SPOT/PRINT REG                               
         B     PRTREP              REP                                          
*                                                                               
PRODUC   DS    0H                                                               
         MVC   P+1(20),SBACCNM        CLIENT NAME TO REPORT                     
         BAS   RE,LK23             GET 23 ELEMENT - SET DESCRIPTION             
         BAS   RE,LK50             GET CASH DISCOUNT FROM 50                    
         BAS   RE,DISBLI           PRINT DISBURSEMENT LINE                      
         CLI   CKNXT,C'N'          IF NO CHECK                                  
         BE    CD555               PRINT IT NOW                                 
         B     EXIT                                                             
*                                                                               
SPTRG    DS    0H                                                               
         BAS   RE,LK46             SET UP PRINT LINE                            
         CLC   STLINE,SPACES                                                    
         BNE   *+10                                                             
         MVC   STLINE(6),TRNSREF                                                
         BAS   RE,DISBLI           PRINT DISB. LINE                             
         CLI   CKNXT,C'N'                                                       
         BE    CD555                                                            
         B     EXIT                                                             
*                                                                               
PRTREP   DS    0H                                                               
         BAS   RE,LK46                                                          
         CLI   REPNAME,C'Y'                                                     
         BNE   SPTRG                                                            
         MVC   S,SPACES                                                         
         L     R2,ADSUBAC                                                       
         CLI   TRSBACNT+2,C'C'     BANK ACCOUNT                                 
         BE    BANKAC                                                           
         MVC   S+1(3),=C'PUB'                                                   
         MVC   S+5(14),SBACCNO+1                                                
         MVC   S+22(36),SBACCNM                                                 
         CLC   CHACTL+3(3),SPACES  IS IT PUB                                    
         BNE   DOPRT               YES                                          
*                                                                               
         MVC   S,SPACES                                                         
         MVC   S+1(7),=C'STATION'                                               
         MVC   S+9(5),SBACCNO+3                                                 
         MVC   S+14(1),SBACCNO+8                                                
         CLI   SBACCNO+8,C'F'                                                   
         BNE   *+14                                                             
         MVC   S+14(3),=C'-FM'                                                  
         B     DOPRT                                                            
         CLI   SBACCNO+8,C'A'                                                   
         BNE   *+10                                                             
         MVC   S+14(3),=C'-AM'                                                  
*                                                                               
DOPRT    MVI   REPNAME,C'N'                                                     
         B     SPTRG                                                            
*                                                                               
BANKAC   DS    0H                                                               
         MVC   S+1(7),=C'ACCOUNT'                                               
         MVC   S+9(11),SBACCNO+1   ACCOUNT                                      
         MVC   S+22(36),SBACCNM    ACCOUNT NAME                                 
         B     SPTRG                                                            
*                                                                               
CD550    CLI   CKNXT,C'Y'          DO I WANT CHECK ITEM                         
         BNE   CD556                                                            
         CLC   ACTVST,=X'0000'     START MONTH OF ACTIVITY REQUESTED            
         BE    CD551               NO, SO DONT CHECK                            
         L     R4,ADTRANS                                                       
         GOTO1 =V(CONVMOS),DMCB,(X'FE',(R4)),MOS                                
         CLC   MOS,ACTVST                                                       
         BL    CD556                                                            
*                                                                               
CD551    CLC   ACTVED,=X'FFFF'     END MONTH OF ACTIVITY REQUESTED              
         BE    CD553               NO, SO KEEP IT                               
*                                                                               
         L     R4,ADTRANS                                                       
         GOTO1 =V(CONVMOS),DMCB,(X'FE',(R4)),MOS                                
         CLC   MOS,ACTVED                                                       
         BH    CD556               SKIP IT                                      
*                                                                               
CD553    CP    STNET,TRNSAMNT      SAME AMOUNT                                  
         BNE   CD556                                                            
         MVC   PS1+81(6),TRNSNARR       CHECK NUMBER                            
         MVC   PS1+89(8),TRNSNARR+6     CHECK DATE                              
         MVC   PS1+98(11),TRNSNARR+29 BANK ACCOUNT                              
         CLI   TRNSNARR+40,C'V'    VOID CHECK                                   
         BNE   CD555                                                            
         MVI   PS1+80,C'*'                                                      
         MVI   PS1+87,C'*'                                                      
         ZAP   OFFLN+19(8),STCD                                                 
         ZAP   OFFLN+27(8),STNET                                                
*                                                                               
CD555    DS    0H                                                               
         CLC   S,SPACES                                                         
         BE    CD55A                                                            
         MVC   P,S                                                              
         CLI   CHACTL,X'FE'        TYPE 37                                      
         BE    *+8                 DON'T DOUBLE SPACE                           
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
         MVC   S,SPACES                                                         
*                                                                               
CD55A    MVC   P,PS1                                                            
         MVC   PSECOND,PS2                                                      
         BAS   RE,REPORT                                                        
         ZAP   OFFLN+03(8),STCD                                                 
         ZAP   OFFLN+11(8),STNET                                                
*                                                                               
         BAS   RE,BINADD                ADD OFFICE/MEDIA LINE                   
         LA    R3,4                                                             
         LA    R4,OFFLN+3                                                       
         LA    R5,ACCUM                                                         
         ZAP   0(8,R5),0(8,R4)                                                  
         LA    R4,8(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R3,*-14                                                          
         GOTO1 ADDBK,DMCB,ACCUM,CLITOT,5                                        
         MVI   ACT,C'Y'                                                         
*                                                                               
CD556    MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* PROCTRNS  - UNDISBURSED                                                       
*                                                                               
UD500    CLI   QOPT2,C'Y'          SKIP MARKER MANUAL CHECKS IF                 
         BNE   UD500AA             RUNNING BEFORE CHECKS                        
         MVI   ELCODE,X'60'                                                     
         L     R3,ADTRANS                                                       
         BAS   RE,NEXTEL                                                        
         BNE   UD500AA                                                          
         USING TRSELD,R3                                                        
         TM    TRSSTAT2,TRSSMCHQ                                                
         BO    EXIT                                                             
*                                                                               
UD500AA  L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   0(R4),X'44'                                                      
         BNE   EXIT                                                             
*                                                                               
         TM    TRNSSTAT,X'20'      EXCLUDE REVERSALS                            
         BO    EXIT                                                             
*                                                                               
         CLI   QOPT4,C'A'          APPROVED ITEMS ONLY                          
         BNE   *+12                                                             
         TM    TRNSSTAT,X'02'                                                   
         BZ    EXIT                                                             
*                                                                               
         MVC   OFFLN+3(64),PZR                                                  
         MVC   OFFLN(2),TRNSANAL                                                
*                                                                               
         MVC   RQOFFICE,SPACES                                                  
         MVC   RQOFFICE(1),QOPT5                                                
         CLI   QOPT5,C' '          OFFICE FILTERING                             
         BNE   UD5OFFC             NO FILTERING                                 
         MVC   RQOFFICE,QTRNSFLT                                                
         CLC   QTRNSFLT,SPACES                                                  
         BNE   UD5OFFC                                                          
         MVC   RQOFFICE,QOFFICE                                                 
         CLC   QOFFICE,SPACES                                                   
         BNE   UD5OFFC                                                          
         MVC   RQOFFICE,SPACES                                                  
UD5OFFC  CLC   RQOFFICE,SPACES                                                  
         BE    UD5000              NO OFFICE SPECIFIED                          
         CLC   RQOFFICE,TRNSANAL                                                
         BE    UD5000              DID NOT MATCH OFFICE                         
*                                                                               
         TM    RQOFFICE,X'40'                                                   
         BO    EXIT                DID NOT MATCH OFFICE                         
         MVC   BYTE2,RQOFFICE                                                   
         OI    BYTE2,X'40'                                                      
         CLC   BYTE2,TRNSANAL                                                   
         BE    EXIT                IF OFFICE MATCHES - EXCLUDE ITEM             
*                                                                               
UD5000   CLI   LISTSW,C'L'         SKIP QSELECT LOGIC IF LEDGER LIST            
         BE    UD500A                                                           
         CLC   QSELECT(3),SPACES   OR IF NOTHING THERE                          
         BE    UD500A                                                           
         L     R2,ADSUBAC                                                       
         MVC   THREE,TRSBACNT+3                                                 
         CLI   QLEDGER,C'W'                                                     
         BE    UD501                                                            
         CLI   QLEDGER,C'V'                                                     
         BE    UD501                                                            
         MVC   THREE,TRSBACNT+12                                                
*                                                                               
UD501    CLI   LISTSW,C'Y'         IS THERE A CLIENT LIST                       
         BNE   UD501B                                                           
         L     RF,VEXTRAS          YES                                          
         USING RUNXTRAD,RF                                                      
         GOTO1 ACLIST,DMCB,VLISTREC,THREE DO WE WANT THIS ONE                   
         CLI   DMCB,0                                                           
         BNE   *+6                                                              
         DC    H'0'                BAD LIST RECORD                              
         CLI   DMCB,C'E'                                                        
         BE    EXIT                EXCLUDE THIS ONE                             
         B     UD500A                                                           
*                                                                               
UD501B   CLC   QSELECT(3),THREE    NO LIST RECORD - IT'S A CLIENT               
         BE    UD500A                                                           
         TM    QSELECT,X'40'                                                    
         BO    EXIT                                                             
         NI    THREE,X'BF'                                                      
         CLC   QSELECT(3),THREE                                                 
         BE    EXIT                                                             
*                                                                               
UD500A   DS    0H                                                               
         CLI   TRNSTYPE,X'81'      DO NOT WANT CHECKS                           
         BE    EXIT                                                             
         CLI   QEND,C' '          SPECIAL AS AT DATE                            
         BE    UD500B              NOT USED                                     
         CLI   TRNSTYPE,9        BA TRANSFERS - USE TRANACTION DATE             
         BE    *+12                                                             
         CLI   TRNSTYPE,50       TALENT TRANSFERS - USE TRANACTION DATE         
         BNE   UD500A1                                                          
         CLI   QLEDGER,C'V'      CHECK ONLY FOR LEDGERS V AND W                 
         BE    *+12                                                             
         CLI   QLEDGER,C'W'                                                     
         BNE   UD500A1                                                          
         CLC   TRNSDATE,TODAYP                                                  
         BH    EXIT                                                             
         CLI   QOPT2,C'B'          SPECIAL AS OF RUN DATE -                     
         BNE   UD500B              AFTER CHECKS ONLY ONE DAY                    
         CLC   TRNSDATE,TODAYP                                                  
         BNE   EXIT                                                             
         B     UD500B                                                           
*                                                                               
UD500A1  MVI   ELCODE,X'60'                                                     
         L     R3,ADTRANS                                                       
         BAS   RE,NEXTEL                                                        
         BNE   UD500B                                                           
*                                                                               
         USING TRSELD,R3                                                        
         CLI   QOPT2,C'Y'          SKIP MARKER MANUAL CHECKS IF                 
         BNE   *+12                RUNNING BEFORE CHECKS                        
         TM    TRSSTAT2,TRSSMCHQ                                                
         BO    EXIT                                                             
*                                                                               
         USING TRSTATD,R3                                                       
         CLC   TRSTDATE,TODAY2                                                  
         BH    EXIT                EXCLUDE THOSE ADDED AFTER RUN DATE           
         CLI   QOPT2,C'B'                                                       
         BNE   UD500B                                                           
         CLC   TRSTDATE,TODAY2     IF OPTION B                                  
         BNE   EXIT                EXCLUDE THOSE ADDED BEFORE RUN DATE          
*                                                                               
UD500B   LR    RF,R4                                                            
         SH    RF,DATADISP                                                      
         USING ACKEYD,RF                                                        
         OC    ACDTUSED,ACDTUSED                                                
         BZ    UD500D              STILL UNDISBURSED                            
*                                                                               
UD500C   CLC   ACDTUSED,TODAY2                                                  
         BL    EXIT                DISBURSED BEFORE RUN DATE - SKIP IT          
         BH    UD500D              DISBURSED AFTER RUN DATE - KEEP IT           
         CLI   QOPT2,C'Y'          RUN BEFORE                                   
         BNE   EXIT                IF NOT SKIP - ELSE KEEP IT                   
*                                                                               
UD500D   CLI   QOPT6,C'Y'          URGENT ONLY                                  
         BNE   UD500F                                                           
         TM    TRNSSTAT,X'40'                                                   
         BNO   EXIT                                                             
*                                                                               
UD500F   CLI   QOPT7,C'U'          IGNORE UNAUTHORISED                          
         BNE   UD500X                                                           
         TM    TRNSSTAT,X'08'                                                   
         BZ    EXIT                                                             
*                                                                               
UD500X   CLI   CHACTL,0            ANYTHING IN CONTROL                          
         BE    UD502                                                            
         BAS   RE,CHA              GET CHANGE CODE                              
         CLI   CHACD,0                                                          
         BE    UD505               NO CHANGE                                    
         TM    CHACD,1             CLIENT CHANGE                                
         BZ    *+8                                                              
         BAS   RE,CLTLU            UNDISBURSED CLIENT TOTAL                     
         TM    CHACD,6             PUB CHANGE                                   
         BZ    *+8                                                              
         BAS   RE,PUBTLU                                                        
*                                                                               
UD502    BAS   RE,BLDCNTL          BUILD CONTROL FIELD                          
         LA    R3,CLITOT           CLEAR CLITOT                                 
         LA    R5,STATOT-1                                                      
         BAS   RE,CLRACM                                                        
*                                                                               
UD505    LA    R2,ACCUM                                                         
         LA    R3,DATPK                                                         
         L     R4,ADTRANS                                                       
         LA    R7,OFFLN+3                                                       
         LA    R5,3                                                             
UD510    CLC   TRNSDATE,0(R3)      COMPARE TO DATES REQUESTED                   
         BNH   UD515                                                            
         LA    R2,8(R2)                                                         
         LA    R3,3(R3)                                                         
         LA    R7,8(R7)                                                         
         BCT   R5,UD510                                                         
*                                                                               
UD515    ZAP   0(8,R2),TRNSAMNT         MONTH BUCKET                            
         ZAP   ACCUM+32(8),TRNSAMNT     AND TOTAL                               
         ZAP   0(8,R7),TRNSAMNT                                                 
         ZAP   OFFLN+35(8),TRNSAMNT                                             
         ZAP   ACCUM+40(8),=P'0'                                                
         ZAP   OFFLN+43(8),=P'0'                                                
*                                                                               
         TM    TRNSSTAT,X'02'      APPROVED ITEM                                
         BZ    *+16                                                             
         ZAP   ACCUM+48(8),TRNSAMNT                                             
         ZAP   OFFLN+51(8),TRNSAMNT                                             
*                                                                               
         TM    TRNSSTAT,X'04'      HELD ITEM                                    
         BZ    *+16                                                             
         ZAP   ACCUM+56(8),TRNSAMNT                                             
         ZAP   OFFLN+59(8),TRNSAMNT                                             
*                                                                               
         CLI   PROGPROF,C'Y'                                                    
         BNE   UD517               DEFAULT SHOW CASH DISCOUNT                   
         CLI   QOPT6,C'Y'                                                       
         BE    UD550               DONT SHOW URGENT FOR URGENT OPTION           
         TM    TRNSSTAT,X'40'                                                   
         BNO   UD550                                                            
         AP    ACCUM+40(8),TRNSAMNT                                             
         AP    OFFLN+43(8),TRNSAMNT                                             
         B     UD550                                                            
*                                                                               
UD517    LR    R3,R4                                                            
         MVI   ELCODE,X'46'             GET CASH DISCOUNT                       
         BAS   RE,NEXTEL                                                        
         BE    UD520                                                            
*                                                                               
         LR    R3,R4                                                            
         MVI   ELCODE,X'50'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   UD550                    NO   CASH DISCOUNT                      
         CLI   2(R3),C'D'                                                       
         BNE   UD550                                                            
         ZAP   ACCUM+40(8),3(6,R3)                                              
         ZAP   OFFLN+43(8),3(6,R3)                                              
         B     UD550                                                            
*                                                                               
UD520    ZAP   ACCUM+40(8),2(6,R3)                                              
         ZAP   OFFLN+43(8),2(6,R3)                                              
*                                                                               
UD550    GOTO1 ADDBK,DMCB,ACCUM,CLITOT,5    ADD TO ALL LEVELS                   
         BAS   RE,BINADD                                                        
*                                                                               
         LA    R3,ACCUM            CLEAR ACCUM                                  
         LA    R5,CLITOT-1                                                      
         BAS   RE,CLRACM                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              ACCOUNT LAST                                           *         
***********************************************************************         
*                                                                               
ACCL     DS    0H                                                               
         CLI   QOPT1,C'U'                                                       
         BE    ACCLU100                                                         
*                                                                               
*              ACCLAST   -DISBURSED                                             
*                                                                               
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   CHACTL,0            NO ACTIVITY                                  
         BE    CD720                                                            
         LA    R2,8                                                             
         LA    R3,ACCTOT                                                        
CD705    CP    0(8,R3),=P'0'                                                    
         BNE   CD710                                                            
         LA    R3,8(R3)                                                         
         BCT   R2,CD705                                                         
         B     CD720                                                            
*                                                                               
CD710    BAS   RE,CHA              GET A CHANGE CODE                            
*                                                                               
         TM    CHACD,1             CLIENT CHANGE                                
         BZ    *+8                                                              
         BAS   RE,CLTL             CLIENT TOTAL                                 
*                                                                               
         TM    CHACD,2             PUBCHANGE                                    
         BZ    *+8                                                              
         BAS   RE,PUBTL            DO PUB TOTAL                                 
*                                                                               
         TM    CHACD,4             STATION CHANGE                               
         BZ    *+8                                                              
         BAS   RE,STATL            DO STATION TOTAL                             
*                                                                               
         BAS   RE,REPORT                                                        
         MVC   P+22(16),=C'TOTALS FOR PAYEE'                                    
         LA    R2,ACCTOT                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
*                                                                               
CD720    LA    R3,BUCK             CLEAR BUCKS UP TO MEDIA TOTAL                
         LA    R5,MEDTOT-1                                                      
         BAS   RE,CLRACM                                                        
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* ACCLAST   UNDISBURSED                                                         
*                                                                               
ACCLU100 DS    0H                                                               
         CLI   CHACTL,0            ANY ACTIVITY                                 
         BE    UDCLB                                                            
         LA    R2,24                                                            
         LA    R3,CLITOT                                                        
*                                                                               
UD805    CP    0(8,R3),=P'0'                                                    
         BNE   UD815                                                            
         LA    R3,8(R3)                                                         
         BCT   R2,UD805                                                         
         CP    PRTCOUNT,=P'0'      AT LEAST 1 PRNTED LINE FOR VEND              
         BH    UD820                YES - TRY FOR TOTAL                         
         B     UDCLB                                                            
*                                                                               
UD815    BAS   RE,CHA              GET A CHANGE CODE                            
         TM    CHACD,1                                                          
         BZ    *+8                                                              
         BAS   RE,CLTLU            UNDISBURSED CLIENT TOTAL                     
*                                                                               
         TM    CHACD,6                                                          
         BZ    *+8                                                              
         BAS   RE,PUBTLU           UNDISBURSED PUB TOTAL                        
*                                                                               
         L     R3,ADACC                                                         
         CLI   2(R3),C'V'                                                       
         BE    UD820                                                            
         CLI   2(R3),C'X'                                                       
         BE    UD820                                                            
         CLI   2(R3),C'Y'                                                       
         BE    UD820                                                            
         CLI   2(R3),C'W'                                                       
         BE    UD820                                                            
         CLI   CHACTL,X'FE'        TYPE 37                                      
         BE    UD820                                                            
         CLI   2(R3),C'P'                                                       
         BNE   *+16                                                             
         CLI   7(R3),C' '                                                       
         BE    UDREP               MUST BE A REP                                
         B     UD820                                                            
         CLI   CHACTL+6,X'40'                                                   
         BNE   UDREP               MUST BE A REP                                
*                                                                               
UD820    DS    0H                                                               
         CLI   PROGPROF+2,C'Y'                                                  
         BNE   UD821                                                            
         LA    R2,ACCTOT                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         BAS   RE,REPORT                                                        
         B     UDCLB                                                            
*                                                                               
UD821    ZAP   TMPHELD,ACCTOT+56                                                
         ZAP   TMPAPPR,ACCTOT+48                                                
         MVI   AHSW,1                                                           
         BAS   RE,AHTOT            PRINT APPROVED/HELD TOTALS                   
         CP    PRTCOUNT,=P'2'      AT LEAST 2 PRNTED LINE FOR VEND              
         BL    UD822               YES - TRY FOR TOTAL                          
         MVC   P+1(7),=C'*TOTAL*'                                               
         LA    R2,ACCTOT                                                        
         BAS   RE,FORMAT                                                        
*                                                                               
UD822    DS    0H                                                               
         BAS   RE,TOTBOX           UNDERLINE                                    
         BAS   RE,REPORT                                                        
         BAS   RE,REPORT                                                        
         B     UDCLB                                                            
*                                                                               
UDREP    BAS   RE,REPORT                                                        
         ZAP   TMPHELD,ACCTOT+56                                                
         ZAP   TMPAPPR,ACCTOT+48                                                
         MVI   AHSW,1                                                           
         BAS   RE,AHTOT            PRINT APPROVED/HELD TOTALS                   
         MVC   P+1(18),=C'**TOTALS FOR REP**'                                   
         MVI   SPACING,2                                                        
         LA    R2,ACCTOT                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         BAS   RE,TOTBOX           UNDERLINE                                    
         BAS   RE,REPORT                                                        
*                                                                               
UDCLB    LA    R3,ACCTOT                                                        
         LA    R5,CRCK             CREDIT CHECKS                                
         LA    R4,5                                                             
*                                                                               
ACCLU950 CP    0(8,R3),=P'0'                                                    
         BH    *+10                                                             
         AP    0(8,R5),0(8,R3)     ADD ONLY MINUS AMOUNTS                       
         LA    R3,8(R3)                                                         
         LA    R5,8(R5)                                                         
         BCT   R4,ACCLU950                                                      
*                                                                               
         LA    R3,BUCK             CLEAR ALL BUCKETS                            
         LA    R5,MEDTOT-1         UP TO ACCOUNT                                
         BAS   RE,CLRACM                                                        
         ZAP   PRTCOUNT,=P'0'                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              LEVEL A LAST                                           *         
***********************************************************************         
*                                                                               
LEVAL    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'ADD',ABUFF,1,(X'80',2)                           
         CLI   QACCOUNT,C' '                                                    
         BNE   CD603                                                            
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   BUFKY,SPACES                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,BUFKY,1                              
         TM    DMCB+8,X'80'                                                     
         BO    CD603                                                            
*                                                                               
CD602    GOTO1 ADDBK,DMCB,BUFAC,ACCUM,1                                         
         MVC   P+1(6),BUFKY                                                     
         MVC   WORK(36),BUFKY+6                                                 
         GOTO1 SQUASHER,DMCB,WORK,36                                            
         MVC   P+8(32),WORK                                                     
         LA    R2,BUFAC                                                         
         MVI   SPACING,2                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
*                                                                               
CD602A   GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,BUFKY,1                               
         TM    DMCB+8,X'80'                                                     
         BNO   CD602                                                            
*                                                                               
CD603    GOTO1 BUFFALO,DMCB,=C'CLEAR',ABUFF,(X'80',1)                           
         L     R3,MEDCNT                                                        
         MH    R3,=H'101'         X LENGTH                                      
         L     R5,MEDTAB                                                        
         LA    R3,0(R5,R3)                                                      
         LA    R3,37(R3)           GET TO BUCKETS                               
         LA    R5,8                                                             
         LA    R4,MEDTOT           TOTAL THIS MEDIA                             
*                                                                               
CD610    ZAP   0(8,R3),0(8,R4)     MOVE MEDIA TOTALS TO TABLE                   
         ZAP   0(8,R4),=P'0'                                                    
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R5,CD610                                                         
*                                                                               
         L     R3,MEDCNT           ADD TO NUMBER IN TABLE                       
         LA    R3,1(R3)                                                         
         ST    R3,MEDCNT                                                        
         CLI   QACCOUNT,C' '                                                    
         BNE   CD612                                                            
         BCTR  R3,0                                                             
         MH    R3,=H'101'                                                       
         L     R2,MEDTAB                                                        
         LA    R3,0(R2,R3)                                                      
         LA    R2,37(R3)                                                        
         CLI   FORCEHED,C'Y'       NOTHING TO PRINT                             
         BE    CD612                                                            
         ZAP   TMPHELD,56(8,R2)                                                 
         ZAP   TMPAPPR,48(8,R2)                                                 
         MVI   AHSW,2                                                           
         BAS   RE,AHTOT            PRINT APPROVED/HELD TOTALS                   
         MVC   P+8(10),=C'TOTALS FOR'                                           
         MVC   P+19(36),1(R3)                                                   
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
CD612    LA    R3,BUCK                                                          
         LA    R5,MEDTOT-1                                                      
         BAS   RE,CLRACM                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              REQUEST LAST                                           *         
***********************************************************************         
*                                                                               
REQL     DS    0H                                                               
         CLI   ONELEVEL,C'N'                                                    
         BE    REQL01                                                           
         BAS   RE,REPORT                                                        
         GOTO1 BUFFALO,DMCB,=C'ADD',ABUFF,1,(X'80',2)                           
*                                                                               
         LA    R2,MEDTOT                                                        
         ZAP   TMPHELD,56(8,R2)                                                 
         ZAP   TMPAPPR,48(8,R2)                                                 
         MVI   AHSW,2                                                           
         BAS   RE,AHTOT            PRINT APPROVED/HELD TOTALS                   
         MVC   P+8(18),=C'TOTALS FOR REQUEST'                                   
         BAS   RE,FORMAT                                                        
         BAS   RE,TOTBOX                                                        
         BAS   RE,REPORT                                                        
*                                                                               
REQL01   MVI   FORCEHED,C'Y'                                                    
         MVC   BUFKY,SPACES                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,BUFKY,2                              
         TM    DMCB+8,X'80'                                                     
         BO    EXIT                                                             
*                                                                               
UD955    GOTO1 ADDBK,DMCB,BUFAC,ACCUM,1                                         
         MVC   P+1(6),BUFKY                                                     
         MVC   WORK(36),BUFKY+6                                                 
         GOTO1 SQUASHER,DMCB,WORK,36                                            
         MVC   P+8(32),WORK                                                     
         LA    R2,BUFAC                                                         
         MVI   SPACING,2                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,BUFKY,2                               
         TM    DMCB+8,X'80'                                                     
         BNO   UD955                                                            
*                                                                               
         LA    R2,ACCUM                                                         
         ZAP   TMPHELD,56(8,R2)                                                 
         ZAP   TMPAPPR,48(8,R2)                                                 
         BAS   RE,AHTOT            PRINT APPROVED/HELD TOTALS                   
         MVC   P+8(18),=C'TOTALS FOR REQUEST'                                   
         LA    R2,ACCUM                                                         
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         CLI   PROGPROF+1,C'Y'                                                  
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   MEDSW,C'Y'                                                       
         MVI   OFCNT,0                                                          
         L     R7,OFFMED          OFFICE MEDIA TABLE                            
         L     R3,0(R7)            NUMBER IN TABLE                              
         LA    R7,12(R7)                                                        
         CLI   0(R7),X'FE'                                                      
         BE    MEDSUMY                                                          
         LR    R2,R3                                                            
         LA    R3,BUCK             CLEAR ACCUMULATOR BUCKETS                    
         LA    R5,MEDTOT-1                                                      
         BAS   RE,CLRACM                                                        
         LR    R3,R2                                                            
         CLI   PROGPROF+1,C'Y'     PROFILE TO SKIP TOTALS                       
         BE    UD920                                                            
*                                                                               
OFFSUM   CLI   ONELEVEL,C'N'       ONE LEVEL PAYABLE?                           
         BE    OFFSUM1             NO - DO OFFICE/MEDIA SUMMARY                 
         BAS   RE,GETOFF           GET OFFICE NAME                              
         LA    R2,3(R7)            TOTALS FOR OFFICE                            
         GOTO1 ADDBK,DMCB,(R2),ACCUM,1      ADD TO OFFICE TOTAL                 
         MVC   P+1(2),0(R6)        OFFICE CODE                                  
         MVC   P+4(36),2(R6)       OFFICE NAME                                  
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         LA    R7,67(R7)                                                        
         BCT   R3,OFFSUM                                                        
         BAS   RE,REPORT                                                        
         B     UD920                                                            
*                                  **OFFICE / MEDIA SUMMARY**                   
OFFSUM1  BAS   RE,GETOFF           GET OFFICE NAME                              
OFFSUM2  BAS   RE,GETME            GET MEDIA NAME                               
         LA    R2,3(R7)            TOTALS FOR OFFICE/MEDIA                      
         MVI   SPACING,2                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         GOTO1 ADDBK,DMCB,(R2),ACCUM,1      ADD TO OFFICE TOTAL                 
         CLC   0(2,R7),67(R7)           LOOK AT NEXT OFFICE                     
         BNE   OFFTOT                   DO OFFICE TOTAL                         
         LA    R7,67(R7)                                                        
         BCT   R3,OFFSUM2                                                       
*                                                                               
OFFTOT   DS    0H                                                               
         ZIC   R5,OFCNT           COUNT OFFICE TOTAL LINES                      
         AH    R5,=H'1'                                                         
         STC   R5,OFCNT                                                         
         LA    R2,ACCUM                                                         
         ZAP   TMPHELD,56(8,R2)                                                 
         ZAP   TMPAPPR,48(8,R2)                                                 
         MVI   AHSW,3                                                           
         BAS   RE,AHTOT            PRINT APPROVED/HELD TOTALS                   
         MVC   P+1(10),=C'TOTALS FOR'                                           
         MVC   P+12(36),2(R6)      OFFICE NAME                                  
*                                                                               
         LA    R2,ACCUM                                                         
         BAS   RE,FORMAT                                                        
         BAS   RE,TOTBOX           UNDERLINE                                    
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         LR    R2,R3                                                            
         LA    R3,BUCK             CLEAR ACCUMULATOR BUCKETS                    
         LA    R5,MEDTOT-1                                                      
         BAS   RE,CLRACM                                                        
         LR    R3,R2                                                            
*                                                                               
         LA    R7,67(R7)                                                        
         LTR   R3,R3                                                            
         BZ    *+8                                                              
         BCT   R3,OFFSUM1                                                       
         CLI   OFCNT,2                                                          
         BL    UD950                                                            
*                                                                               
         BAS   RE,REPORT                                                        
         MVC   P+1(11),=C'ALL OFFICES'                                          
         MVC   PSECOND+1(11),=22C'-'                                            
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
MEDSUMY  LA    R3,BUCK             CLEAR ACCUMULATOR BUCKETS                    
         LA    R5,MEDTOT-1                                                      
         BAS   RE,CLRACM                                                        
         L     R3,MEDCNT                                                        
         LTR   R3,R3                                                            
         BZ    EXIT                                                             
         L     R2,MEDTAB                                                        
*                                                                               
UD910    MVC   P+1(36),1(R2)                                                    
         LA    R2,37(R2)                                                        
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
         GOTO1 ADDBK,DMCB,(R2),ACCUM,1                                          
         LA    R2,64(R2)                                                        
         BCT   R3,UD910                                                         
*                                                                               
UD920    MVC   P,SPACES                                                         
         CLI   PROGPROF+1,C'Y'                                                  
         BE    UD950                                                            
         LA    R2,ACCUM                                                         
         ZAP   TMPHELD,56(8,R2)                                                 
         ZAP   TMPAPPR,48(8,R2)                                                 
         MVI   AHSW,3                                                           
         BAS   RE,AHTOT            PRINT APPROVED/HELD TOTALS                   
         MVC   P+1(18),=C'TOTALS FOR REQUEST'                                   
         LA    R2,ACCUM                                                         
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
UD950    CLI   QOPT1,C'U'                                                       
         BNE   EXIT                                                             
         LA    R4,CRCK                                                          
         LA    R3,5                                                             
         CP    0(8,R4),=P'0'                                                    
         BNE   *+16                                                             
         LA    R4,8(R4)                                                         
         BCT   R3,*-14                                                          
         B     EXIT                                                             
*                                                                               
         BAS   RE,REPORT                                                        
         MVC   P+1(13),=C'CREDIT PAYEES'                                        
         LA    R2,CRCK                                                          
         ZAP   40(8,R2),=P'0'                                                   
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         LA    R3,GRNDTOT                                                       
         LA    R4,5                                                             
         SP    0(8,R3),0(8,R2)                                                  
         LA    R3,8(R3)                                                         
         LA    R2,8(R2)                                                         
         BCT   R4,*-14                                                          
         LA    R2,GRNDTOT                                                       
         ZAP   40(8,R2),=P'0'                                                   
         MVC   P+1(17),=C'UNDISBURSED TOTAL'                                    
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              PRINT DISBURSED LINE                                   *         
***********************************************************************         
*                                                                               
DISBLI   NTR1                                                                   
         MVI   CKNXT,C'N'          SET CHECK SWITCH TO IGNORE 81                
         GOTO1 SQUASHER,DMCB,STLINE,53        DESCRIPTION                       
         GOTO1 CHOPPER,DMCB,(53,STLINE),(36,P+22),(C'P',2)                      
         ZAP   STNET,TRNSAMNT                                                   
         LA    R2,STCD                                                          
         BAS   RE,FORMAT                                                        
         MVC   PS1,P                                                            
         MVC   PS2,PSECOND                                                      
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         CLI   SW64,C'Y'                                                        
         BE    DISBL2                                                           
         LR    RF,R4                                                            
         SH    RF,DATADISP                                                      
         USING ACKEYD,RF                                                        
         OC    ACDTUSED,ACDTUSED   SHOULD I HAVE A CHECK                        
         BZ    EXIT                IF NOT THEN OK TO PRINT                      
         CLC   ACDTUSED,TODAY2                                                  
         BE    EXIT                                                             
         MVI   CKNXT,C'Y'          NEXT S/B CHECK                               
         B     EXIT                                                             
*                                                                               
DISBL2   L     R3,AD64EL           FORMAT RHS OF LINE FOR 64 EL                 
         USING TRMAPD,R3                                                        
         OC    TRMAPDTE,TRMAPDTE   TEST ELEMENT CLEARED                         
         BZ    DISBL4                                                           
         MVC   PS1+81(6),TRMAPNO                                                
         MVC   PS1+98(11),TRMAPBNK+2                                            
         GOTO1 DATCON,DMCB,(2,TRMAPDTE),(8,PS1+89)                              
DISBL4   L     R3,ADXVEL           (INTERNAL) VOID ELEMENT                      
         TM    XVXIND1-XVXELD(R3),XVXIM0BV                                      
         BZ    *+12                                                             
         MVI   PS1+80,C'+'                                                      
         B     EXIT                                                             
         TM    XVXIND1-XVXELD(R3),XVXIB37V                                      
         BZ    EXIT                                                             
         MVI   PS1+80,C'*'                                                      
         MVI   PS1+87,C'*'                                                      
         ZAP   OFFLN+19(8),STCD                                                 
         ZAP   OFFLN+27(8),STNET                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CLIENT TOTALS - DISBURSED VERSION                      *         
***********************************************************************         
*                                                                               
CLTL     NTR1            CLIENT TOTAL                                           
         CLI   ACT,C'Y'                                                         
         BNE   EXIT                                                             
         MVI   ACT,C'N'                                                         
         BAS   RE,REPORT                                                        
         CLC   ACCUM(64),CLITOT              SUB-ACC TOT EQ CLI                 
         BNE   CLT1                                                             
         MVI   CLISW,1                                                          
         B     CLT2                                                             
*                                                                               
CLT1     MVC   P+22(17),=C'TOTALS FOR CLIENT'                                   
         CLI   CHACTL,X'FE'        TYPE 37                                      
         BNE   *+10                                                             
         MVC   P+33(12),=C'BANK ACCOUNT'                                        
         CLI   EXPLEDG,C'Y'                                                     
         BNE   *+10                                                             
         MVC   P+33(13),=CL13'CONTRA LEDGER'                                    
         LA    R2,CLITOT                                                        
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
*                                                                               
CLT2     BAS   RE,TOTBOX           UNDERLINE                                    
         BAS   RE,REPORT                                                        
         MVC   BUFAC(64),CLITOT                                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFKY                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PUB TOTALS                                             *         
***********************************************************************         
*                                                                               
PUBTL    NTR1                                                                   
         MVC   P+22(22),=C'TOTALS FOR PUBLICATION'                              
PUBTL2   LA    R2,STATOT                                                        
         MVI   REPNAME,C'Y'                                                     
         CLC   STATOT(64),CLITOT                                                
         BNE   PUBTL3                                                           
         MVC   P,SPACES                                                         
         CLI   FORCEHED,C'Y'                                                    
         BE    EXIT                                                             
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
         B     PUBTL4                                                           
*                                                                               
PUBTL3   MVI   SPACING,2                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,TOTBOX           UNDERLINE                                    
         BAS   RE,REPORT                                                        
*                                                                               
PUBTL4   LA    R3,STATOT           CLEAR STATION TOTALS                         
         LA    R5,ACCTOT-1                                                      
         BAS   RE,CLRACM                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              STATION TOTALS                                         *         
***********************************************************************         
*                                                                               
STATL    NTR1                                                                   
         MVC   P+22(18),=C'TOTALS FOR STATION'                                  
         B     PUBTL2                                                           
         EJECT                                                                  
***********************************************************************         
*              PRINT APPROVED/HELD TOTALS                             *         
***********************************************************************         
*                                                                               
AHTOT    NTR1                                                                   
         CLI   PROGPROF+3,C'Y'     DISPLAY APPR/HELD TOTALS?                    
         BNE   EXIT                                                             
         CP    TMPHELD,=P'0'                                                    
         BE    AHTOT10                                                          
         CLI   AHSW,1                                                           
         BNE   *+10                                                             
         MVC   P+1(30),=CL30'*TOTAL AMOUNT HELD*'                               
         CLI   AHSW,2                                                           
         BNE   *+10                                                             
         MVC   P+8(30),=CL30'TOTAL AMOUNT HELD'                                 
         CLI   AHSW,3                                                           
         BNE   *+10                                                             
         MVC   P+1(30),=CL30'TOTAL AMOUNT HELD'                                 
         EDIT  (P8,TMPHELD),(12,P+88),2,MINUS=YES                               
         BAS   RE,REPORT                                                        
*                                                                               
AHTOT10  DS    0H                                                               
         CP    TMPAPPR,=P'0'                                                    
         BE    EXIT                                                             
         CLI   AHSW,1                                                           
         BNE   *+10                                                             
         MVC   P+1(30),=CL30'*TOTAL AMOUNT APPROVED*'                           
         CLI   AHSW,2                                                           
         BNE   *+10                                                             
         MVC   P+8(30),=CL30'TOTAL AMOUNT APPROVED'                             
         CLI   AHSW,3                                                           
         BNE   *+10                                                             
         MVC   P+1(30),=CL30'TOTAL AMOUNT APPROVED'                             
         EDIT  (P8,TMPAPPR),(12,P+88),2,MINUS=YES                               
         BAS   RE,REPORT                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              DRAW A MIDLINE                                         *         
***********************************************************************         
*                                                                               
TOTBOX   NTR1                      UNDERLINE ROUTINE                            
         ST    RE,SAVEREG                                                       
         L     R2,ADBOX            ADDR OF BOX ROUTINE                          
         USING BOXD,R2                                                          
         ZIC   RE,LINE           DON'T WANT A MIDLINE IF I'M SO                 
         ZIC   RF,MAXLINES        CLOSE TO THE BOTTOM                           
         AH    RE,=H'4'                                                         
         CR    RE,RF                                                            
         BNL   EXIT                                                             
         ZIC   RE,LINE                                                          
         LA    RF,MYROW(RE)                                                     
         CLI   CLISW,1             NO CLIENT TOTAL                              
         BNE   TOTBOX2                                                          
         MVI   CLISW,0                                                          
         BCTR  RF,0                                                             
*                                                                               
TOTBOX2  MVI   0(RF),C'M'                                                       
         MVC   BOXROWS,MYROW                                                    
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         L     RE,SAVEREG                                                       
         B     EXIT                                                             
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
*              FIND X'23' ELEMENT                                     *         
***********************************************************************         
*                                                                               
LK23     NTR1                                                                   
         MVI   ELCODE,X'23'                                                     
         LR    R3,R4               ADDRESS OF TRANS                             
         LA    R2,STLINE                                                        
         BAS   RE,NEXTEL           GET 23                                       
         BNE   LK23A                                                            
         MVC   STLINE(6),2(R3)                                                  
         MVI   STLINE+6,C'-'                                                    
         MVC   STLINE+7(6),8(R3)                                                
         MVI   STLINE+13,C'-'                                                   
         LA    R2,STLINE+14                                                     
*                                                                               
LK23A    MVC   0(6,R2),TRNSREF     INVOICE NUMBER                               
         B     EXIT                RETURN                                       
         EJECT                                                                  
***********************************************************************         
*              FIND X'46' ELEMENT                                     *         
***********************************************************************         
*                                                                               
LK46     NTR1                                                                   
         LR    R3,R4               ADDRESS OF TRANS                             
         MVI   ELCODE,X'46'        GET CASH DISCOUNT IN 46 ELEMENT              
         ZAP   STCD,=P'0'                                                       
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         MVC   P+1(20),8(R3)          CLIENT PRODUCT                            
         MVC   STLINE,SPACES                                                    
         MVC   STLINE(20),28(R3)   PRODUCT NAME                                 
         MVC   STLINE+21(14),48(R3) INVOICE NUMBER                              
         ZAP   STCD,2(6,R3)        CASH DISCOUNT                                
         CLC   66(2,R3),=C'00'     ZEROES IN DAY                                
         BNE   LK46A                                                            
         GOTO1 DATCON,DMCB,(0,62(R3)),(6,STLINE+36)                             
         B     LK46B                                                            
LK46A    GOTO1 DATCON,DMCB,(0,62(R3)),(8,STLINE+36)                             
LK46B    CLC   68(6,R3),=6X'00'                                                 
         BE    EXIT                                                             
         CLC   68(6,R3),SPACES                                                  
         BE    EXIT                                                             
         GOTO1 (RF),(R1),(0,68(R3)),(8,STLINE+45)                               
         MVI   STLINE+44,C'-'                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              FIND X'50' ELEMENT                                     *         
***********************************************************************         
*                                                                               
LK50     NTR1                                                                   
         LR    R3,R4               GET CASH DISCOUNT                            
         MVI   ELCODE,X'50'        FROM 50                                      
         ZAP   STCD,=P'0'                                                       
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         CLI   2(R3),C'D'          ELEMENT MUST CONTAIN A D                     
         BNE   EXIT                                                             
         ZAP   STCD,3(6,R3)                                                     
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CHECK CLIENT TOTAL                                     *         
***********************************************************************         
*                                                                               
CLTLU    NTR1                                                                   
         LA    R2,8                CHECK IF ACTIVE                              
         LA    R3,CLITOT                                                        
         CP    0(8,R3),=P'0'                                                    
         BNE   CLTLU10                                                          
         LA    R3,8(R3)                                                         
         BCT   R2,*-14                                                          
         B     EXIT                                                             
*                                                                               
CLTLU10  L     R3,ADACC                                                         
         CLI   2(R3),C'V'                                                       
         BE    CLTLU20                                                          
         CLI   CHACTL+6,X'40'                                                   
         BNE   CLREP               MUST BE REP                                  
*                                                                               
CLTLU20  CLI   ACCTFRST,C'N'       FIRST TIME FOR ACCOUNT                       
         BE    CLTLN               NO, DO CLIENT TOTAL                          
         MVC   P+1(32),PAYEE                                                    
         MVI   ACCTFRST,C'N'                                                    
         B     CLTLN                                                            
*                                                                               
CLREP    CLI   ACCTFRST,C'N'       FIRST FOR ACCOUNT                            
         BE    CLREP2                                                           
         MVI   ACCTFRST,C'N'                                                    
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
         MVC   P+1(34),PAYEE         REP NAME                                   
         BAS   RE,REPORT                                                        
*                                                                               
CLREP2   CLI   SUBAFRST,C'N'       FIRST FOR STA/PUB                            
         BE    CLTLN               NO                                           
         MVI   SUBAFRST,C'N'                                                    
         BAS   RE,REPORT                                                        
         GOTO1 SQUASHER,DMCB,STAPUB,50                                          
         GOTO1 CHOPPER,DMCB,(50,STAPUB),(30,P+2),(C'P',2)                       
*                                                                               
CLTLN    DS    0H                                                               
         CLI   PROGPROF+2,C'Y'     SKIP CLIENT TOTAL                            
         BNE   CLTLN1                                                           
         MVC   P+34(L'P-34),SPACES                                              
         B     CLTLBUF                                                          
*                                                                               
CLTLN1   MVC   P+34(6),BUFKY          CLIENT CODE                               
         LA    R2,CLITOT                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         AP    PRTCOUNT,=P'1'                                                   
*                                                                               
CLTLBUF  MVC   BUFAC(64),CLITOT     CLIENT TOTALS                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFKY                                 
         B     EXIT                                                             
*                                                                               
PUBTLU   NTR1                                                                   
         MVI   SUBAFRST,C'Y'                                                    
         CLI   PROGPROF+2,C'Y'                                                  
         BE    PUBLR2                                                           
         CLC   STATOT(64),CLITOT                                                
         BE    PUBCLR                                                           
         MVC   P+1(15),=C'*TOTAL FOR PUB*'                                      
*                                                                               
PUBLR2   LA    R2,STATOT                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
*                                                                               
PUBCLR   LA    R3,STATOT                                                        
         LA    R5,ACCTOT-1                                                      
         BAS   RE,CLRACM                                                        
         B     EXIT                                                             
         EJECT                                                                  
*              P1 A(SOURCE)                                                     
*              P2 A(RESULT)                                                     
*              P3 NUMBER OF LEVELS TO ADD                                       
ADDBK    NTR1                                                                   
         LM    R2,R4,0(R1)                                                      
ADDNT    LA    R5,8           8 COLUMNS                                         
ADDLP    AP    0(8,R3),0(8,R2)                                                  
         LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R5,ADDLP       NEXT COLUMN                                       
         L     R2,0(R1)       SET R2 TO BEGINNING                               
         BCT   R4,ADDNT       NUMBER OF LEVELS                                  
         B     EXIT                                                             
*                                                                               
* CLEAR ACCUMULATORS                                                            
*                                                                               
CLRACM   LA    R4,8                CLEAR ACCUMULATORS                           
         ZAP   0(8,R3),=P'0'                                                    
         BXLE  R3,R4,*-6           R5 TO LAST +5                                
         BR    RE                                                               
*                                                                               
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
REPORT   NTR1                                                                   
         MVI   HEADSW,C'Y'                                                      
         CLI   FORCEHED,C'Y'                                                    
         BE    REPORT2                                                          
         CLC   LINE,MYMAX                                                       
         BNL   REPORT2                                                          
         MVI   HEADSW,C'N'                                                      
         CLI   QOPT1,C'U'                                                       
         BE    *+8                                                              
         MVI   MYBOXSW,3                                                        
         B     REPRTX                                                           
*                                                                               
REPORT2  DS    0H                                                               
         CLI   QOPT4,C'A'                                                       
         BNE   *+10                                                             
         MVC   HEAD5+84(19),=C'APPROVED ITEMS ONLY'                             
         CLI   QOPT1,C'U'                                                       
         BNE   REPORTD                                                          
*                                                                               
         MVI   MYBOXSW,1                                                        
         MVC   HEAD7+56(18),=CL18'ADVERTISING PERIOD'                           
         CLI   EXPLEDG,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD7+56(18),=CL18'   AGING PERIOD   '                           
         MVC   HEAD2+84(13),=C'BEFORE CHECKS'                                   
         CLI   QOPT2,C'Y'                                                       
         BE    *+10                                                             
         MVC   HEAD2+84(13),=CL13'AFTER CHECKS'                                 
*                                                                               
         CLI   QOPT6,C'Y'                                                       
         BNE   *+10                                                             
         MVC   HEAD2+99(6),=C'URGENT'                                           
         MVC   HEAD8+45(45),DATELN                                              
         MVC   HEAD9+54(26),DATELN2                                             
*                                                                               
         MVI   RCSUBPRG,0                                                       
         CLI   PROGPROF,C'Y'                                                    
         BE    *+12                                                             
         MVI   RCSUBPRG,3                                                       
         B     REPU10                                                           
*                                                                               
         CLI   QOPT6,C'Y'                                                       
         BE    REPU10                                                           
         MVI   RCSUBPRG,4                                                       
*                                                                               
REPU10   CLI   MODE,REQLAST                                                     
         BNE   REPU10C                                                          
         MVC   HEAD5+1(50),=CL50'SUMMARY FOR REQUEST'                           
         MVI   MYBOXSW,2                                                        
         CLI   MEDSW,C'Y'                                                       
         BE    REPU10A                                                          
         MVC   HEAD8+1(20),=C'CLIENT CODE AND NAME'                             
         MVC   HEAD9+1(20),=20C'-'                                              
         CLI   EXPLEDG,C'Y'                                                     
         BNE   REPRTX                                                           
         MVC   HEAD8+1(20),=CL20'CONTRA A/C'                                    
         MVC   HEAD9+1(20),=CL20'----------'                                    
         B     REPRTX                                                           
*                                                                               
REPU10A  DS    0H                                                               
         CLI   ONELEVEL,C'Y'                ONE LEVEL LEDGER?                   
         BE    REPRTX                       YES - FORGET THE TITLE              
         MVC   HEAD8+1(5),=C'MEDIA'                                             
         MVC   HEAD9+1(5),=C'-----'                                             
         CLI   EXPLEDG,C'Y'                                                     
         BNE   REPRTX                                                           
         MVC   HEAD8+1(8),=CL20'CATEGORY'                                       
         MVC   HEAD9+1(8),=C'--------'                                          
         B     REPRTX                                                           
*                                                                               
REPU10C  MVI   MYBOXSW,2                                                        
         MVC   HEAD5+1(11),=C'SUMMARY FOR'                                      
         MVC   HEAD8+1(20),=C'CLIENT CODE AND NAME'                             
         MVC   HEAD9+1(20),=20C'-'                                              
         L     R3,MEDCNT                                                        
         MH    R3,=H'101'                                                       
         L     RE,MEDTAB                                                        
         LA    R3,0(RE,R3)                                                      
         MVC   HEAD5+13(36),1(R3)                                               
         CLI   MODE,LEVALAST       LEVALAST?                                    
         BNE   REPU11              NO                                           
         MVI   MYBOXSW,2           YES, GET SUMMARY BOXES                       
         B     REPRTX              AND PRINT                                    
*                                                                               
REPU11   MVC   HEAD5+13(36),SPACES                                              
         MVC   HEAD5+13(36),1(R3)                                               
         CLI   QOPT3,C'Y'          SUMMARIES ONLY                               
         BNE   REPU12                                                           
         MVC   P,SPACES                                                         
         B     EXIT                                                             
*                                                                               
REPU12   MVI   MYBOXSW,1                                                        
         MVC   HEAD8+1(22),=CL22'PAYEE ACCOUNT AND NAME'                        
         MVC   HEAD9+1(22),=22C'-'                                              
         MVC   HEAD8+34(6),=C'CLIENT'                                           
         MVC   HEAD9+34(6),=C'------'                                           
         CLI   EXPLEDG,C'Y'                                                     
         BNE   *+16                                                             
         MVC   HEAD8+34(6),=C'CONTRA'                                           
         MVC   HEAD9+34(6),=C' A/C  '                                           
         CLI   ONELEVEL,C'N'                ONE LEVEL LEDGER?                   
         BE    *+14                         NO- CONTINUE                        
         MVC   HEAD5+1(48),SPACES           YES - TITLE DOES NOT APPLY          
         B     REPRTX                       NO- CONTINUE                        
         MVC   HEAD5+1(11),=CL11'MEDIA'                                         
         CLI   EXPLEDG,C'Y'                                                     
         BNE   REPRTX                                                           
         MVC   HEAD5+1(11),=CL20'CATEGORY'                                      
         B     REPRTX                                                           
*                                                                               
REPORTD  MVI   RCSUBPRG,2                                                       
         MVI   MYBOXSW,4                                                        
         CLI   MODE,REQLAST                                                     
         BNE   REPORTD1                                                         
         MVC   HEAD5+1(50),=CL50'SUMMARY FOR REQUEST'                           
         CLI   MEDSW,C'Y'                                                       
         BE    REPORTDA                                                         
         MVC   HEAD9+01(20),=CL20'CLIENT CODE AND NAME'                         
         MVC   HEAD10+1(20),=20C'-'                                             
         CLI   EXPLEDG,C'Y'                                                     
         BNE   REPRTX                                                           
         MVC   HEAD9+01(20),=CL20'CONTRA A/C'                                   
         MVC   HEAD10+1(20),=CL20'----------'                                   
         B     REPRTX                                                           
*                                                                               
REPORTDA DS    0H                                                               
         CLI   ONELEVEL,C'Y'                ONE LEVEL LEDGER?                   
         BE    REPRTX                       YES - FORGET THE TITLE              
         MVC   HEAD9+01(5),=C'MEDIA'                                            
         MVC   HEAD10+1(5),=C'-----'                                            
         CLI   EXPLEDG,C'Y'                                                     
         BNE   REPRTX                                                           
         MVC   HEAD9+01(20),=CL20'CATEGORY'                                     
         MVC   HEAD10+1(30),SPACES                                              
         MVC   HEAD10+1(8),=C'--------'                                         
         B     REPRTX                                                           
*                                                                               
REPORTD1 MVC   HEAD5+1(11),=C'SUMMARY FOR'                                      
         MVC   HEAD9+1(20),=C'CLIENT CODE AND NAME'                             
         MVC   HEAD10+1(20),=20C'-'                                             
         L     R3,MEDCNT                                                        
         MH    R3,=H'101'                                                       
         L     RE,MEDTAB                                                        
         LA    R3,0(RE,R3)                                                      
         MVC   HEAD5+13(36),1(R3)                                               
         CLI   MODE,LEVALAST                                                    
         BE    REPRTX                                                           
         MVC   HEAD5+13(36),SPACES                                              
         MVC   HEAD5+13(36),1(R3)                                               
         CLI   QOPT3,C'Y'          SUMMARIES ONLY                               
         BNE   REPORTD2                                                         
         MVC   P,SPACES                                                         
         B     EXIT                                                             
*                                                                               
REPORTD2 MVI   MYBOXSW,3                                                        
         MVC   HEAD9,SPACES                                                     
         MVC   HEAD10,SPACES                                                    
         MVC   HEAD5+1(7),=CL7'PAYEE'                                           
         MVC   HEAD5+8(50),PAYEE                                                
         MVI   RCSUBPRG,1                                                       
         MVC   HEAD8+1(20),=CL20'CLIENT'                                        
         MVC   HEAD9+1(20),=CL20'------'                                        
         CLI   EXPLEDG,C'Y'                                                     
         BNE   REPRTX                                                           
         MVC   HEAD8+1(20),=CL20'CONTRA A/C'                                    
         MVC   HEAD9+1(20),=CL20'----------'                                    
*                                                                               
REPRTX   OC    P,SPACES                                                         
REPRTY   GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* GET NAME FROM X'20' ELEMENT                                                   
*                                                                               
NAMOUT   NTR1                                                                   
         MVC   WORK,SPACES                                                      
         MVI   ELCODE,X'20'                                                     
         LR    R3,R2                                                            
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
         USING ACNAMED,R3                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EXMVC R1,WORK,ACNMNAME                                                 
         B     EXIT                                                             
*                                                                               
FORMAT   NTR1                                                                   
         CLI   QOPT1,C'U'                                                       
         BNE   FORMATD                                                          
         LA    R3,P+40                                                          
         LA    R4,5                                                             
FORMATU  CP    0(8,R2),=P'0'       UNDISBURSED                                  
         BE    FORMATU2                                                         
         EDIT  (P8,0(R2)),(12,0(R3)),2,MINUS=YES                                
FORMATU2 LA    R3,12(R3)                                                        
         LA    R2,8(R2)                                                         
         BCT   R4,FORMATU                                                       
         EDIT  (P8,0(R2)),(12,0(R3)),2,MINUS=YES,ZERO=BLANK                     
         B     EXIT                                                             
*                                                                               
FORMATD  CP    0(8,R2),=P'0'            DISBURSED                               
         BE    FORMATD1                                                         
         EDIT  (P8,0(R2)),(10,P+58),2,MINUS=YES                                 
*                                                                               
FORMATD1 CLI   MODE,REQLAST                                                     
         BNE   FORMATD2                                                         
         EDIT  (P8,8(R2)),(12,P+68),2,MINUS=YES                                 
         B     FORMATD3                                                         
*                                                                               
FORMATD2 EDIT  (P8,8(R2)),(12,P+68),2,MINUS=YES                                 
*                                                                               
FORMATD3 CLI   MODE,LEVALAST                                                    
         BE    FORMATD4                                                         
         CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
*                                                                               
FORMATD4 CP    16(8,R2),=P'0'                                                   
         BE    FORMATD5                                                         
         EDIT  (P8,16(R2)),(10,P+84),2,MINUS=YES                                
*                                                                               
FORMATD5 CP    24(8,R2),=P'0'                                                   
         BE    EXIT                                                             
         EDIT  (P8,24(R2)),(12,P+95),2,MINUS=YES                                
         B     EXIT                                                             
         EJECT                                                                  
BINADD   NTR1                                                                   
         CLI   OFFLN,C'A'                                                       
         BNL   *+8                                                              
         MVI   OFFLN,X'FE'         OTHERS                                       
         L     R7,OFFMED                                                        
         L     R3,0(R7)                                                         
         LTR   R3,R3                                                            
         BZ    BINFRST             1ST TIME                                     
         L     R4,4(R7)            ADDRESS OF LAST                              
         CLC   OFFLN(3),0(R4)      IF SAME AS LAST                              
         BE    BINFND              DO NOT SEARCH                                
*                                                                               
BINFRST  L     R5,8(R7)            MAX                                          
         LA    R4,12(R7)           DATA                                         
         GOTO1 BINSRCH,DMCB,(1,OFFLN),(R4),(R3),67,(0,3),(R5)                   
         MVC   0(4,R7),DMCB+8      COUNT                                        
         CLI   DMCB,1                                                           
         BE    BINX                                                             
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
         L     R4,DMCB             ADDRESS OF RECORD                            
BINFND   LA    R5,OFFLN+3                                                       
         LA    R4,3(R4)                                                         
         LA    R3,8                                                             
         AP    0(8,R4),0(8,R5)                                                  
         LA    R5,8(R5)                                                         
         LA    R4,8(R4)                                                         
         BCT   R3,*-14                                                          
         B     EXIT                                                             
*                                                                               
BINX     L     R4,DMCB             WHEN NEW RECORD                              
         ST    R4,4(R7)            SAVE ADDRESS OF LAST                         
         B     EXIT                                                             
         EJECT                                                                  
GETOFF   NTR1                                                                   
         BAS   RE,REPORT                                                        
         L     R6,OFFLIST                                                       
         CLC   0(2,R7),0(R6)       MATCH OFFICE CODE TO                         
         BE    *+20                OFFICE LIST                                  
         CLI   0(R6),X'FF'                                                      
         BE    *+12                                                             
         LA    R6,38(R6)                                                        
         B     *-22                                                             
*                                                                               
         MVC   P+1(2),0(R7)                                                     
         CLI   P+1,X'FE'                                                        
         BNE   *+14                                                             
         MVC   P+1(36),2(R6)       OTHERS                                       
         B     *+10                                                             
         MVC   P+4(36),2(R6)       OFFNAME                                      
         LA    R3,P+37                                                          
         CLI   0(R3),X'40'                                                      
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         CLI   ONELEVEL,C'Y'       IF THIS IS A ONE LEVEL LEDGER                
         BE    GETOFF99            DONT UNDERLINE OR SKIP A LINE.               
*                                  SINCE THERE IS NO MEDIA, THIS WILL           
*                                  BE AN OFFICE SUMMARY, NOT AN OFFICE          
*                                  MEDIA SUMMARY.                               
         MVI   PSECOND+1,C'-'                                                   
         LA    R2,P+2                                                           
         SR    R3,R2                                                            
         LTR   R3,R3                                                            
         BNP   *+18                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   PSECOND+2(0),PSECOND+1                                           
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
GETOFF99 DS    0H                                                               
         XIT1  REGS=(R6)                                                        
         SPACE 1                                                                
GETME    NTR1                                                                   
         L     R6,MEDTAB                                                        
         CLC   2(1,R7),0(R6)                                                    
         BE    *+12                                                             
         LA    R6,101(R6)                                                       
         B     *-14                                                             
         SPACE 1                                                                
         MVC   P+1(36),1(R6)                                                    
         B     EXIT                                                             
         EJECT                                                                  
*              DETERMINE IF SUB-ACCOUNT CHANGE                                  
*              IF SO RETURN CODE - CHACD                                        
*              CHACD  BIT 0 ON -                                                
*                         1 ON -                                                
*                         2 ON -                                                
*                         3 ON -                                                
*                         4 ON -                                                
*                         5 ON - STA-CHANGE  -SPOT                              
*                         6 ON - PUB-CHANGE  - PRINT                            
*                         7 ON - CLIENT CHANGE                                  
*                                                                               
CHA      NTR1                                                                   
         L     R2,ADSUBAC                                                       
         USING TRSUBHD,R2                                                       
         MVI   CHACD,0             NO CHANGE                                    
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         L     R3,ADACC                                                         
         CLI   2(R3),C'V'          PRODUCTION                                   
         BE    CHAPD                                                            
         CLI   2(R3),C'X'                                                       
         BE    CHAPD                                                            
         CLI   2(R3),C'Y'                                                       
         BE    CHAPD                                                            
*                                                                               
         CLI   MODE,ACCLAST        FORCE BREAKS FOR ACCTLAST                    
         BE    CHA5                                                             
         CLC   CHACTL(3),TRSBACNT+12  SPOT / PRINT CLIENT                       
         BE    CHA2                                                             
         CLI   CHACTL,X'FE'        IF TYPE 37,                                  
         BNE   *+14                                                             
         CLC   TRSBACNT+12(3),SPACES    COMPARE TO SPACES                       
         BE    CHA2                                                             
         OI    CHACD,1                                                          
*                                                                               
CHA2     CLI   CHACTL+6,X'40'      TO CATCH BAD INPUT                           
         BE    EXIT                                                             
         CLC   CHACTL+3(11),TRSBACNT  CHANGE OF PUB/STATION                     
         BE    EXIT                                                             
         OI    CHACD,1                                                          
*                                                                               
CHA3     CLC   CHACTL+3(3),SPACES  WAS LAST A PUB                               
         BNE   CHA4                                                             
         OI    CHACD,4             STATION CHANGE                               
         B     EXIT                                                             
*                                                                               
CHA4     CLI   CHACTL,X'FE'        TYPE 37                                      
         BE    EXIT                DO CLIENT CHANGE                             
         OI    CHACD,2             PUB CHANGE                                   
         B     EXIT                                                             
*                                                                               
CHA5     OI    CHACD,1                                                          
         CLI   CHACTL+6,X'40'                                                   
         BE    EXIT                                                             
         CLC   CHACTL+3(11),SPACES                                              
         BE    EXIT                                                             
         B     CHA3                                                             
*                                                                               
CHAPD    CLI   MODE,ACCLAST        CLIENT ON ACCLAST                            
         BNE   *+12                                                             
         OI    CHACD,1                                                          
         B     EXIT                                                             
*                                                                               
         CLI   SVTYP,X'71'                                                      
         BNE   CHA61                                                            
         ZIC   R5,STCLTLNG         VARIABLE LENGTH CLIENT                       
         EX    R5,CLCHA                                                         
         BE    EXIT                                                             
         OI    CHACD,1                                                          
         B     EXIT                                                             
*                                                                               
CLCHA    CLC   CHACTL(0),TRSBACNT+3                                             
*                                                                               
CHA61    CLI   SVTYP,X'61'                                                      
         BNE   CHALL                                                            
         CLC   CHACTL(5),TRNSNARR                                               
         BE    EXIT                                                             
         OI    CHACD,1                                                          
         B     EXIT                                                             
*                                                                               
CHALL    CLC   CHACTL(6),TRSBACNT+3                                             
         BE    EXIT                                                             
         OI    CHACD,1                                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              BUILD CONTROL FOR CURRENT TRANSACTION                            
*                                                                               
BLDCNTL  NTR1                                                                   
         MVC   BUFKY,SPACES                                                     
         MVC   CHACTL,SPACES                                                    
         MVC   STAPUB,SPACES                                                    
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         MVC   SVTYP,TRNSTYPE       SAVE TYPE                                   
         L     R2,ADSUBAC                                                       
         USING TRSUBHD,R2                                                       
         L     R3,ADACC                                                         
         CLI   2(R3),C'V'                                                       
         BE    BLDCP                                                            
         CLI   2(R3),C'X'                                                       
         BE    BLDCP                                                            
         CLI   2(R3),C'Y'                                                       
         BE    BLDCP                                                            
         CLI   2(R3),C'W'                                                       
         BE    BLDCP                                                            
         MVC   CHACTL(3),TRSBACNT+12  SPOT / PRINT CLIENT                       
         CLC   CHACTL(3),SPACES    BANK ACCOUNT                                 
         BNE   *+14                                                             
         MVI   CHACTL,X'FE'                                                     
         MVC   BUFKY+6(36),SBACCNM      MOVE BK ACCT NAME INTO KEY              
         MVC   CHACTL+3(11),TRSBACNT                                            
         MVC   BUFKY(3),CHACTL     CLIENT CODE TO BUFFALO                       
         LR    R3,R4                                                            
         MVI   ELCODE,X'46'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   BLDCN1                                                           
         LR    R7,R3                                                            
         USING TRPAYD,R7                                                        
         MVC   BUFKY+6(20),TRPYCLI             CLIENT NAME                      
BLDCN1   MVC   STAPUB(5),TRSBACNT+3            STATION                          
         OC    BUFKY,SPACES                                                     
         CLC   TRSBACNT(3),SPACES              IS IT A STATION                  
         BE    BLDCN2                          YES                              
         MVC   STAPUB(36),SBACCNM              MUST BE A PUB                    
         LA    R1,STAPUB                                                        
         OI    STAPUB,X'40'                                                     
         CLI   STAPUB,X'40'                                                     
         BE    *+8                                                              
         LA    R1,37(R1)                                                        
         MVC   0(11,R1),TRSBACNT               MOVE IN PUB NUMBER               
         B     EXIT                                                             
*                                                                               
BLDCN2   DS    0H                                                               
         MVC   STAPUB+5(1),TRSBACNT+8 DERIVE FREQUENCY (AM/FM)                  
         CLI   TRSBACNT+8,C'F'                                                  
         BNE   *+14                                                             
         MVC   STAPUB+5(3),=C'-FM'                                              
         B     EXIT                                                             
         CLI   TRSBACNT+8,C'A'                                                  
         BNE   *+10                                                             
         MVC   STAPUB+5(3),=C'-AM'                                              
         B     EXIT                                                             
*                                                                               
BLDCP    CLI   TRNSTYPE,X'71'                                                   
         BNE   BLDCP61                                                          
         ZIC   R5,STCLTLNG                                                      
         EX    R5,*+8                                                           
         B     BLDNME              NAME IS SUB ACC                              
         MVC   CHACTL(0),TRSBACNT+3                                             
*                                                                               
BLDCP61  CLI   TRNSTYPE,X'61'      MEDLINE PRINT                                
         BNE   BLDCPAL                                                          
         MVC   CHACTL(5),TRNSNARR  CLIENT CODE                                  
         MVC   BUFKY(5),CHACTL                                                  
         B     EXIT                                                             
*                                                                               
BLDCPAL  MVC   CHACTL(6),TRSBACNT+3    ALL OTHERS                               
BLDNME   MVC   BUFKY(6),CHACTL                                                  
         MVC   BUFKY+6(36),SBACCNM                                              
         OC    BUFKY,SPACES                                                     
         B     EXIT                                                             
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE 1                                                                
SQUASHER DC    V(SQUASHER)                                                      
DATVAL   DC    V(DATVAL)                                                        
ACLIST   DC    V(ACLIST)                                                        
OFFLIST  DC    A(COFFLIST)                                                      
OFFMED   DC    A(COFFMED)                                                       
MEDTAB   DC    A(CMEDTAB)                                                       
ABUFF    DC    A(BUFFALOC)                                                      
         LTORG                                                                  
         EJECT                                                                  
         BUFF  LINES=200,ROWS=2,COLUMNS=8,FLAVOR=PACKED,KEYLIST=(6,A), X        
               COMMENT=36                                                       
AC5802   CSECT                                                                  
         ENTRY COFFLIST                                                         
COFFLIST DS    0D                                                               
         DS    256CL38                                                          
         SPACE 1                                                                
         ENTRY COFFMED                                                          
COFFMED  DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    F'0'                ADDRESS OF LAST                              
         DC    F'3000'             MAX IN TABLE                                 
         DS    3000CL67            OFFICE/MEDIA/8PL8                            
         SPACE 1                                                                
         ENTRY CMEDTAB                                                          
CMEDTAB  DS    0D                                                               
         DS    500CL101                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
*              HEADLINE ROUTINES (HOOK)                                         
         ENTRY HOOK                                                             
HOOK     NMOD1 0,*HOOK*                                                         
         L     RC,SAVERC           RESTORE REG C                                
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   MYCOL,SPACES                                                     
         MVC   MYROW,SPACES                                                     
         MVI   MYCOL,C'L'                                                       
         CLI   MYBOXSW,2           UNDISB SUMMARY                               
         BH    HOOK2                                                            
         BE    HOOK1                                                            
         CLI   MYBOXSW,1           UNDISB DETAIL                                
         BNE   HOOKX                                                            
         MVI   MYCOL+33,C'C'                                                    
HOOK1    CLI   HEADSW,C'Y'                                                      
         BE    HOOK1A                                                           
         MVI   MYCOL+52,C'C'                                                    
         MVI   MYCOL+63,C'C'                                                    
         MVI   MYCOL+75,C'C'                                                    
HOOK1A   MVI   MYCOL+40,C'C'                                                    
         MVI   MYCOL+88,C'C'                                                    
         MVI   MYCOL+100,C'C'                                                   
         MVI   MYROW+5,C'T'                                                     
         MVI   MYROW+9,C'M'                                                     
         B     HOOKLAST                                                         
         SPACE 1                                                                
HOOK2    MVI   MYCOL+57,C'C'                                                    
         CLI   MYBOXSW,4           DISBURS SUMMARY                              
         BE    HOOK3                                                            
         CLI   MYBOXSW,3           DISBURS DETAIL                               
         BNE   HOOKX                                                            
         MVI   MYCOL+21,C'C'                                                    
         MVI   MYCOL+67,C'C'                                                    
         MVI   MYCOL+80,C'C'                                                    
         MVI   MYCOL+87,C'C'                                                    
         MVI   MYCOL+97,C'C'                                                    
         MVI   MYROW+6,C'T'                                                     
         MVI   MYROW+9,C'M'                                                     
         B     HOOKLAST                                                         
         SPACE 1                                                                
HOOK3    MVI   MYCOL+81,C'C'                                                    
         CLI   HEADSW,C'Y'                                                      
         BE    HOOK3A                                                           
         MVI   MYCOL+67,C'C'                                                    
         MVI   MYCOL+94,C'C'                                                    
HOOK3A   MVI   MYROW+7,C'T'                                                     
         MVI   MYROW+11,C'M'                                                    
         SPACE 1                                                                
HOOKLAST MVI   MYCOL+112,C'R'                                                   
         MVI   MYROW+55,C'B'                                                    
         MVC   BOXCOLS,MYCOL                                                    
         MVC   BOXROWS,MYROW                                                    
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
HOOKX    XMOD1 1                                                                
         SPACE 2                                                                
SAVERC   DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
AC58D    DSECT                                                                  
         SPACE 1                                                                
RELO     DS    F                                                                
AD64EL   DS    A                                                                
AD60EL   DS    A                                                                
ADXVEL   DS    A                   (INTERNAL) VOID ELEMENT                      
TODAY    DS    CL6                                                              
TODAY2   DS    CL2                                                              
TODAYP   DS    CL3                                                              
ACTVST   DS    PL2                                                              
ACTVED   DS    PL2                                                              
RQOFFICE DS    CL2                                                              
BYTE2    DS    CL2                                                              
PRTCOUNT DS    PL3            NUMBER OF PRINTED LINES FOR A VENDOR              
*                                                                               
DATELN   DS    CL45           DATES FOR HEADLINE MMM/YY OR MMMDD/YY             
DATELN2  DS    CL26                                                             
DATPK    DS    CL12           4  3BYTE DATES YMD FOR COMPARE                    
INCR     DS    F              NUMBER OF DAYS TO INCREMENT                       
LORK     DS    4F                                                               
LISTSW   DS    CL1                                                              
         SPACE 1                                                                
SAVEKEY  DS    CL3                 COMPANY/UNIT/LEDGER                          
STCLTLNG DS    CL1                 ACCOUNT LEVEL 1 LENGTH-1                     
         SPACE 1                                                                
MEDCNT   DS    F                   NUMBER OF ITEMS IN MEDTAB                    
         SPACE 1                                                                
ACCNO    DS    CL13                ACCOUNT NUMBER                               
ACCNAM   DS    CL36                ACCOUNT NAME                                 
         SPACE 1                                                                
SBACCNO  DS    CL16                SUB-ACCOUNT NUMBER                           
SBACCNM  DS    CL36                SUB-ACCOUNT NAME                             
         SPACE 1                                                                
STLINE   DS    CL53               WORK LINE                                     
S        DS    CL132                                                            
PS1      DS    CL132                                                            
PS2      DS    CL132                                                            
ACT      DS    CL1                                                              
ACCTFRST DS    CL1                 TO CONTROL PRINTING OF ACCOUNT NAME          
SUBAFRST DS    CL1                 TO CONTROL PRINTING OF SUB-ACC NAME          
REPNAME  DS    CL1                 TO CONTROL PRINTING OF PUB/STA               
CKNXT    DS    CL1                 Y = NEXT ITEM IS A CHECK                     
SW64     DS    CL1                                                              
         SPACE 1                                                                
PAYEE    DS    CL50                SQUASHED ACCOUNT NUMBER AND NAME             
CHACD    DS    CL1                 SUB-ACCOUNT CHANGE CODE                      
CHACTL   DS    CL15                CHANGE CONTROL                               
*                                                                               
SVTYP    DS    CL1                 SAVE LAST INPUT TYPE NOT 81                  
STAPUB   DS    CL50                FOR REP STA/PUB                              
*                                                                               
BUCK     DS    0H                                                               
STCD     DS    PL8                 TRANSACTION CASH DISCOUNT                    
STNET    DS    PL8                 TRANSACTION NET                              
ACCUM    DS    8PL8                SUB-ACC TOTALS                               
CLITOT   DS    8PL8                CLIENT TOTALS                                
STATOT   DS    8PL8                REP TOTALS                                   
ACCTOT   DS    8PL8                PAYEE TOTALS                                 
MEDTOT   DS    8PL8                MEDIA TOTAL                                  
GRNDTOT  DS    8PL8                GRAND TOTAL                                  
CRCK     DS    8PL8                CREDIT CHECKS                                
BUCKEND  EQU   *-1                                                              
*                                                                               
BUFKY    DS    CL42                CLIENT CODE MAX 6- CLIENT NAME 36            
BUFAC    DS    8PL8                ACCUMULATORS                                 
*                                                                               
PZR      DS    8PL8                                                             
OFFLN    DS    CL67                OFFICE/MEDIA/8PL8                            
MYMAX    DS    CL1                                                              
MEDSW    DS    CL1                                                              
OFCNT    DS    F                                                                
*                                                                               
MYBOXSW  DS    CL1                                                              
HEADSW   DS    CL1                                                              
MYCOL    DS    CL132                                                            
MYROW    DS    CL100                                                            
ADBOX    DS    F                                                                
CLISW    DS    CL1                                                              
SAVEREG  DS    F                                                                
*                                                                               
EXPLEDG  DS    CL1                                                              
ONELEVEL DS    CL1            ONE LEVEL LEDGER SWITCH                           
*                                                                               
AHSW     DS    CL1                 APPROVED/HELD TOTAL SWITCH                   
TMPHELD  DS    PL8                                                              
TMPAPPR  DS    PL8                                                              
ELCODE   DS    CL1                                                              
MOS      DS    PL2                                                              
WRK2     DS    CL10                                                             
REMOTOP  DS    C                                                                
MYKEY    DS    CL49                                                             
         DS    CL1000                                                           
         SPACE 2                                                                
XVXELD   DSECT                     (INTERNAL) VOID INFORMATION ELEMENT          
XVXEL    DS    XL1                                                              
XVXELQ   EQU   X'FF'                                                            
XVXLN    DS    XL1                                                              
XVXIND1  DS    XL1                 INDICATOR - 1                                
XVXIM0BV EQU   X'80'               CREDIT IS/WAS MARKER BANK/VOID               
XVXIB37V EQU   X'40'               CREDIT IS BT37 VOID                          
XVXIMPCR EQU   X'20'               PAYMENT DETAILS FROM CREDIT MPYEL            
XVXIDROK EQU   X'10'               DEBIT IS FOUND AND OK                        
XVXDATE  DS    XL(L'TRSDATE)       LATEST DEBIT ADDED DATE                      
XVXLNQ   EQU   *-XVXELD                                                         
         EJECT                                                                  
***********************************************************************         
*              OFFICE TABLE DSECT                                     *         
***********************************************************************         
*                                                                               
OFFLD    DSECT                                                                  
OFFL     DS    CL2                 OFFICE                                       
OFFLNAME DS    CL36                OFFICE NAME                                  
OFFLLNQ  EQU   *-OFFLD                                                          
         EJECT                                                                  
***********************************************************************         
*              MEDIA TABLE DSECT                                      *         
***********************************************************************         
*                                                                               
MEDD     DSECT                                                                  
MEDCODE  DS    CL1                 MEDIA CODE                                   
MEDNAME  DS    CL36                MEDIA NAME                                   
MEDACCUM DS    8PL8                ACCUMS                                       
MEDLNQ   EQU   *-MEDD                                                           
         EJECT                                                                  
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072ACREP5802 05/01/02'                                      
         END                                                                    
