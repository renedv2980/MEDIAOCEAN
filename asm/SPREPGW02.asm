*          DATA SET SPREPGW02  AT LEVEL 002 AS OF 05/01/02                      
*PHASE SPGW02A,+0,NOAUTO                                                        
         TITLE 'TRANSFER WORKER GOAL RECORDS TO SPOT FILE'                      
         PRINT NOGEN                                                            
SPGW02   CSECT                                                                  
         NMOD1 0,SPGW02,R3,RR=R5                                                
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         ST    R5,RELO                                                          
*---------------------------------------------------------------------          
*        RUNFIRST                                                               
*---------------------------------------------------------------------          
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   M00                                                              
         L     RE,=V(ESTTAB)                                                    
         A     RE,RELO                                                          
         ST    RE,VESTTAB                                                       
*-->     L     RE,=V(CNTTAB)                                                    
*        A     RE,RELO                                                          
*-->     ST    RE,VCNTTAB                                                       
         L     RF,=F'1800'                                                      
         XCEF                                                                   
         L     RE,=V(MKTTAB)                                                    
         A     RE,RELO                                                          
         ST    RE,VMKTTAB                                                       
         XC    MKTCNTR,MKTCNTR                                                  
         L     RF,=F'6000'                                                      
         XCEF                                                                   
         L     RE,=V(ERRDESC)                                                   
         A     RE,RELO                                                          
         ST    RE,VERRDESC                                                      
         MVC   DUB(4),ABRTLNC                                                   
         MVC   DUB+4(4),ABRTLNC                                                 
         LA    R4,DUB              ALLOCATE BRTRAB                              
         LA    R5,WHERE                                                         
         GETMAIN VC,LA=(R4),A=(R5)                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   DUB(4),=F'160000'       ALLOCATE CNTTAB                          
         MVC   DUB+4(4),=F'160000'     ALLOCATE CNTTAB                          
         LA    R4,DUB                                                           
         LA    R5,WHERE2                                                        
         GETMAIN VC,LA=(R4),A=(R5)                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (CDRQFIL,(OUTPUT))  OPEN FILE FOR CD REQUESTS                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
*        REQUEST FIRST                                                          
*---------------------------------------------------------------------          
*                                                                               
M00      CLI   MODE,REQFRST                                                     
         BNE   M000                                                             
         L     RE,VESTTAB                                                       
         L     RF,=F'1800'                                                      
         XCEF                                                                   
         XC    ETCNTR,ETCNTR                                                    
         L     RE,VMKTTAB                                                       
         XC    MKTCNTR,MKTCNTR                                                  
         XC    CTCNTR,CTCNTR                                                    
         L     RF,=F'6000'                                                      
         XCEF                                                                   
         XC    WRKRINDX,WRKRINDX                                                
         MVI   PHASESW,1                                                        
         B     EXIT                                                             
*---------------------------------------------------------------------          
*        REQUEST LAST                                                           
*---------------------------------------------------------------------          
M000     CLI   MODE,REQLAST                                                     
         BNE   M00A                                                             
         GOTO1 WORKER,DMCB,=C'CLOSE',AWRKR4K,WRKRINDX                           
         B     EXIT                                                             
*---------------------------------------------------------------------          
*        RUN LAST                                                               
*---------------------------------------------------------------------          
M00A     CLI   MODE,RUNLAST                                                     
         BNE   M01                                                              
         L     R1,WHERE2           RELEASE CNTTAB                               
         L     R0,AMOUNT2                                                       
         FREEMAIN R,LV=(0),A=(1)                                                
         L     R1,WHERE            RELEASE BRTTAB                               
         L     R0,AMOUNT                                                        
         FREEMAIN R,LV=(0),A=(1)                                                
*                                                                               
         MVC   WORK(80),SPACES                                                  
         MVC   WORK(2),=C'/*'      END REQUEST                                  
         L     R1,=A(CDRQFIL)                                                   
         LA    R0,WORK                                                          
         PUT   (1),(0)                                                          
         CLOSE (CDRQFIL)           CLOSE CD REQUEST FILE                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------          
*        CLIENT FIRST                                                           
*---------------------------------------------------------------------          
M01      DS    0C                                                               
         CLI   MODE,CLTFRST                                                     
         BNE   EXIT                                                             
         MVI   RCSUBPRG,1                                                       
         MVI   ACTIVITY,0                                                       
*                                                                               
         L     RE,=A(PRDEST)       CLEAR SUMMARY AREAS                          
         LA    RF,PRDESTEN-PRDESTST                                             
         XCEF                                                                   
         BAS   RE,CLRBRT                                                        
         L     R9,VMKTTAB                                                       
         L     R8,ADBUY                                                         
         USING MKTREC,R8                                                        
         XC    0(200,R8),0(R8)                                                  
         MVC   0(2,R8),=C'MT'                                                   
         MVC   1(1,R8),QMED                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',(R8),(R8)                    
         B     SEEDM3                                                           
SEEDM1   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'STATION',(R8),(R8)                    
SEEDM3   CLC   0(1,R8),=C'M'                                                    
         BNE   SEEDMX                                                           
         CLC   1(1,R8),QMED                                                     
         BNE   SEEDMX                                                           
         CLC   MKTKAGY,QAGY                                                     
         BNE   SEEDM1                                                           
         PACK  DUB,MKTKMKT                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'3000'         BYPASS RADIO MARKETS                         
*        BH    SEEDMX                                                           
         STCM  R1,3,0(R9)                                                       
         LA    R9,2(R9)                                                         
         L     RE,MKTCNTR                                                       
         LA    RE,1(RE)                                                         
         ST    RE,MKTCNTR                                                       
         CH    RE,=H'3000'                                                      
         BL    *+6                                                              
         DC    H'0'                                                             
         B     SEEDM1                                                           
SEEDMX   DS    0H                                                               
*                                                                               
         LA    R2,WRKRINDX         *TRANSFER GOAL RECORDS*                      
         USING UKRECD,R2                                                        
         PACK  DUB,QMKT            BUILD USER INDEX AREA                        
         CVB   RE,DUB                                                           
         STH   RE,SVUSRID                                                       
OPENREC  GOTO1 WORKER,DMCB,=C'INDEX',AWRKR4K,WRKRINDX                           
         TM    8(R1),X'80'                                                      
         BO    EXIT                                                             
         CLC   UKUSRID,SVUSRID                                                  
         BNE   OPENREC                                                          
         CLC   UKSYSPRG,=C'SGX'                                                 
         BNE   OPENREC                                                          
         CLC   UKCLASS,QMED                                                     
         BNE   OPENREC                                                          
GETRECS  GOTO1 WORKER,DMCB,=C'READ',AWRKR4K,WRKRINDX,WRKREC                     
         TM    DMCB+8,X'80'        END OF FILE                                  
         BO    END                                                              
         TM    DMCB+8,X'50'        ERROR READING WORKER                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R5,WRKREC+10                                                     
         USING GOALREC,R5                                                       
         SR    RF,RF                                                            
         ICM   RF,3,GLENGTH                                                     
         AR    RF,R5                                                            
         XC    0(5,RF),0(RF)                                                    
         CLI   GKEYTYPE,2          IS IT A GOAL RECORD                          
         BE    *+6                                                              
         DC    H'0'                NO - CAUSE A DUMP                            
         CLI   GKEYEST,0                                                        
         BE    GETRECS                                                          
         CLC   GKEYAM(6),SVGKAM    A/M/C/P/M OK                                 
         BE    GETREC0              YES - TRY PRODUCT                           
         CLI   ACTIVITY,1                                                       
         BNE   *+8                                                              
         BAS   RE,GETPRV           GET THE CURRENT FILE                         
         L     RE,=A(PRDEST)       CLEAR SUMMARY AREAS                          
         LA    RF,PRDESTEN-PRDESTST                                             
         XCEF                                                                   
GETREC0  CLC   GKEYAM(3),SVGKAM    A/M/C OK                                     
         BE    GETREC2              YES - TRY PRODUCT                           
         MVC   SVGKAM,GKEYAM       SAVE AGENCY/CLIENT                           
         MVC   GKEYAM,BAGYMD                                                    
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),GKEYAM                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    GETREC1                                                          
         XC    SVGKAM,SVGKAM                                                    
         MVC   P(36),=C'INVALID CLIENT CODE - RUN TERMINATED'                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
GETREC1  MVC   AREC,ADCLT                                                       
         GOTO1 GETCLT                                                           
         EJECT                                                                  
GETREC2  L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         MVC   GKEYAM,BAGYMD                                                    
         LA    RF,CLIST            SET START OF PRODUCTS                        
         LA    RE,CCLTIFC          END OF PRODUCTS                              
         SR    RE,RF                                                            
         SRL   RE,2                SET FOR BCT                                  
GETREC4  CLI   0(RF),0             PRODUCT NOT IN LIST                          
         BE    GETREC6                                                          
         CLC   0(3,RF),WRKREC+7    IS IT THIS PRODUCT                           
         BE    GETREC6A                                                         
         LA    RF,4(RF)                                                         
         BCT   RE,GETREC4                                                       
         DROP  R6                                                               
GETREC6  XC    WORK,WORK           ERROR - PRODUCT NOT IN LIST                  
         LA    RF,WORK                                                          
         USING COUNTD,RF                                                        
         MVC   CNTCLT,WRKREC+4                                                  
         MVC   CNTPRD,WRKREC+7                                                  
         MVI   CNTFLG,PRDNF                                                     
         BAS   R9,INSERR                                                        
         B     GETRECS                                                          
         DROP  RF                                                               
GETREC6A MVC   GKEYPRD,3(RF)       SET PRODUCT CODE IN KEY                      
         MVC   SVGKPM,GKEYPRD      SAVE PRD/MARKET                              
         MVC   CURRPRD,WRKREC+7    SAVE CURRENT PRODUCT                         
         MVC   CURREST,GKEYEST                                                  
         MVC   CURRCLT,GKEYCLT                                                  
         MVC   CURRMKT,GKEYMKT                                                  
         CLI   PHASESW,1                                                        
         BNE   PUTRECS                                                          
         SPACE 2                                                                
* FIND ESTIMATE FOR THIS PRODUCT                                                
         XC    WORK,WORK                                                        
         LA    RF,WORK                                                          
         USING ESTTABD,RF                                                       
         MVC   ETPRD,GKEYPRD                                                    
         MVC   ETEST,GKEYEST                                                    
         L     R9,ETCNTR                                                        
         L    R8,VESTTAB                                                        
         GOTO1 BINSRCH,DMCB,(0,WORK),(R8),(R9),6,2,300                          
         CLI   0(R1),1             FOUND                                        
         BNE   GETREC6B             YES - COUNT RECORDS                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),GKEYAM                                                  
         MVC   KEY+4(3),WRKREC+7                                                
         MVC   KEY+7(1),GKEYEST                                                 
         MVC   AREC,ADEST                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     ESTIMATE NOT FOUND                           
         BE    GETRC6A1                                                         
         XC    WORK,WORK                                                        
         LA    RF,WORK                                                          
         USING COUNTD,RF                                                        
         MVC   CNTCLT,WRKREC+4                                                  
         MVC   CNTPRD,WRKREC+7                                                  
         MVC   CNTEST,GKEYEST                                                   
         MVI   CNTFLG,ESTNF                                                     
         BAS   R9,INSERR                                                        
         B     GETRECS                                                          
         DROP  RF                                                               
GETRC6A1 GOTO1 GETEST                                                           
         L     R6,ADEST                                                         
         USING ESTHDR,R6                                                        
         LA    R9,WORK                                                          
         USING ESTTABD,R9                                                       
         MVC   ETPRD,GKEYPRD                                                    
         MVC   ETEST,GKEYEST                                                    
         GOTO1 DATCON,DMCB,(0,ESTART),(2,ETSTART)                               
         GOTO1 DATCON,DMCB,(0,EEND),(2,ETEND)                                   
         L     R9,ETCNTR                                                        
         L     R8,VESTTAB                                                       
         GOTO1 BINSRCH,DMCB,(1,WORK),(R8),(R9),6,2,300                          
         MVC   ETCNTR,8(R1)                                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                ESTIMATE TABLE IS FULL                       
         DROP  R9                                                               
         SPACE 2                                                                
GETREC6B L     RF,0(R1)            CHECK WITHIN ESTIMATE DATES                  
         USING ESTTABD,RF                                                       
*                                                                               
* SCAN THRU THE ELEMENTS FOR DATES OUTSIDE ESTIMATE                             
*                                                                               
         SPACE 2                                                                
         LA    R9,GDELEM           POINT TO FIRST ELEMENT                       
         USING GLEMENT,R9                                                       
CHKELM   CLI   0(R9),0             END                                          
         BE    GETREC8                                                          
         CLI   0(R9),X'21'         GOAL ELEMENT                                 
         BNE   CHKELM2                                                          
         CLC   GLWEEK,ETSTART      DATE LESS THAN START                         
         BL    GETREC6C             YES - ERROR                                 
         CLC   GLWEEK,ETEND        DATE GREATER THAN END                        
         BH    GETREC6C                                                         
CHKELM2  ZIC   R1,1(R9)            GET NEXT ELEMENT                             
         LTR   R1,R1               INSURE THAT LENGTH IS NOT ZERO               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R9,R1               BUMP ELEMENT                                 
         B     CHKELM                                                           
         DROP R9                                                                
         DROP  RF                                                               
GETREC6C XC    WORK,WORK                                                        
         LA    RF,WORK                                                          
         USING COUNTD,RF                                                        
         MVC   CNTCLT,WRKREC+4                                                  
         MVC   CNTPRD,WRKREC+7                                                  
         MVI   CNTFLG,OUTDATE                                                   
         MVC   CNTEST,GKEYEST                                                   
         BAS   R9,INSERR                                                        
         B     GETRECS                                                          
         SPACE 2                                                                
GETREC8  L     R9,MKTCNTR                                                       
         L     R8,VMKTTAB                                                       
         GOTO1 BINSRCH,DMCB,(X'00',GKEYMKT),(R8),(R9),2,2,3000                  
         CLI   0(R1),1             MARKET OK                                    
         BNE   GETREC10             YES - COUNT GOOD RECORD                     
         XC    WORK,WORK            NO - COUNT BAD RECORD                       
         LA    RF,WORK                                                          
         MVC   CNTMKT,GKEYMKT                                                   
         MVC   CNTMPRD,GKEYPRD                                                  
         MVI   CNTEST,X'01'                                                     
         MVI   CNTFLG,MKTERR                                                    
         BAS   R9,INSERR                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(13),WRKREC+10                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETRECS                                                          
         MVC   AREC,ADBUY                                                       
         GOTO1 GET                                                              
         LA    RE,=CL8'DMDEL'                                                   
         LA    RF,SPTFILE                                                       
         STM   RE,RF,DMCB                                                       
         LA    RE,KEY                                                           
         L     RF,AREC                                                          
         STM   RE,RF,DMCB+8                                                     
         LA    RF,DMWORK                                                        
         ST    RF,DMCB+16                                                       
         LA    R1,DMCB                                                          
         CLI   RCWRITE,C'N'                                                     
         BE    GETRECS                                                          
         L     RF,DATAMGR                                                       
         BASR  RE,RF                                                            
         TM    DMCB+8,X'50'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     GETRECS                                                          
         SPACE 2                                                                
GETREC10 LA    RF,WORK             COUNT GOOD RECORDS                           
         XC    WORK,WORK                                                        
         MVC   CNTCLT,WRKREC+4                                                  
         MVC   CNTPRD,WRKREC+7                                                  
         MVC   CNTEST,GKEYEST                                                   
         MVI   CNTFLG,GOODREC                                                   
         BAS   R9,INSERR                                                        
         B     GETRECS                                                          
         DROP  RF                                                               
         EJECT                                                                  
* PUT RECORDS TO GOAL FILE                                                      
PUTRECS  L     R9,MKTCNTR                                                       
         L     R6,VMKTTAB                                                       
         LA    R5,WRKREC+10                                                     
         GOTO1 BINSRCH,DMCB,(X'00',GKEYMKT),(R6),(R9),2,2,3000                  
         CLI   0(R1),1             MARKET OK                                    
         BE    GETRECS              NO - BYPASS THIS RECORD                     
*                                   PER MEL 3/24/97                             
*                                                                               
PUTREC1  LA    R5,WRKREC+10                                                     
         MVC   GKEYAM,BAGYMD       SET AGENCY AND MEDIA                         
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         LA    RF,CLIST            SET START OF PRODUCTS                        
         LA    RE,CCLTIFC          END OF PRODUCTS                              
         SR    RE,RF                                                            
         SRL   RE,2                SET FOR BCT                                  
PUTREC2  CLI   0(RF),0             PRODUCT NOT IN LIST                          
         BNE   *+6                                                              
         DC    H'0'                FOUND PRODUCT ON FIRST PASS                  
*                                   WHY ISNT IT THERE ANYMORE                   
         CLC   0(3,RF),WRKREC+7    IS IT THIS PRODUCT                           
         BE    PUTREC4              YES - PROCEED                               
         LA    RF,4(RF)                                                         
         BCT   RE,PUTREC2                                                       
         DC    H'0'                SEE DC H'0' ABOVE                            
         SPACE 2                                                                
PUTREC4  XC    KEY,KEY                                                          
         STM   RE,RF,DMCB                                                       
         L     RE,=A(WKTI)                                                      
         LA    RF,WKTIEN-WKTI                                                   
         XCEF                                                                   
         LM    RE,RF,DMCB                                                       
         SPACE 2                                                                
         MVC   GKEYPRD,3(RF)       SET TO GOAL NUMERIC CODE                     
         MVC   SVGFPRD,3(RF)                                                    
         BAS   R9,SVPRES           SAVE PROD. AND EST.                          
         MVC   KEY(13),GKEY                                                     
         L     R9,ADD                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PUTREC6                                                          
         MVC   AREC,ADBUY          SET BUY RECORD ADDRESS                       
         L     R9,PUT                                                           
         GOTO1 GET                                                              
         MVI   INTYPE,C'O'                                                      
         BAS   RE,EXTGOAL          EXTRACT EXISTING GOAL DATA                   
         SPACE 2                                                                
PUTREC6  LR    RF,R9                                                            
         LA    RE,WRKREC+10                                                     
         ST    RE,AREC                                                          
         STM   RE,RF,SAVEF                                                      
         MVI   INTYPE,C'I'                                                      
         BAS   RE,EXTGOAL          EXTRACT NEW GOAL DATA                        
         LM    RE,RF,SAVEF                                                      
         B     TESTX               DUMP FOR TESTING                             
         MVC   P(6),=C'ADDREC'                                                  
         C     RF,ADD                                                           
         BE    *+10                                                             
         MVC   P(6),=C'PUTREC'                                                  
         GOTO1 HEXOUT,DMCB,WRKREC+10,P+8,15,0,0                                 
         GOTO1 REPORT                                                           
TESTX    DS    0C                                                               
         STM   RE,RF,SAVEF                                                      
         BAS   RE,SUMINP           SUM INTO PRODUCT TOTALS                      
         BAS   RE,ADDCDREQ         AND ADD CD REQUEST                           
         LM    RE,RF,SAVEF                                                      
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    GETRECS                                                          
         GOTO1 (RF)                                                             
         B     GETRECS                                                          
         SPACE 2                                                                
SVPRES   L     RE,=A(PRDEST)                                                    
SVPRES1  CLI   0(RE),0             END - SET NEW ENTRY                          
         BE    SVPRES2                                                          
         CLC   0(3,RE),WRKREC+7    JUST EXIT IF ALREADY THERE                   
         BNE   *+12                                                             
         CLC   4(1,RE),GKEYEST                                                  
         BER   R9                                                               
         LA    RE,5(RE)            NOT YET SO TRY NEXT                          
         B     SVPRES1                                                          
*                                                                               
SVPRES2  MVC   0(4,RE),0(RF)       PRD ALPHA / NUMERIC                          
         MVC   4(1,RE),GKEYEST                                                  
         BR    R9                                                               
         SPACE 2                                                                
INSERR   LR    R0,R9               SAVE RETURN ADDRESS                          
         CLI   PHASESW,1           FIRST PASS                                   
         BNER  R9                  NO JUST RETURN                               
         L     R9,CTCNTR           BUILD REPORT TABLE                           
         L     R8,WHERE2                                                        
         CLI   WORK+4,4            BYPASS ERROR SETTING-GOOD RECORD             
         BE    *+8                                                              
         MVI   ERRORSW,0                                                        
         GOTO1 BINSRCH,DMCB,(X'01',WORK),(R8),(R9),10,8,16000                   
         MVC   CTCNTR,8(R1)                                                     
         OC    0(4,R1),0(R1)                                                    
         BNE   *+6                                                              
         DC    H'0'                REPORT TABLE OVERFLOW                        
         L     RF,0(R1)                                                         
         USING COUNTD,RF                                                        
         SR    RE,RE                                                            
         ICM   RE,3,CNTCNT                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,CNTCNT                                                      
         DROP  RF                                                               
         LR    R9,R0                                                            
         BR    R9                                                               
         EJECT                                                                  
END      CLI   PHASESW,1           ERROR                                        
         BNE   END2                                                             
         GOTO1 WORKER,DMCB,=C'CLOSE',AWRKR4K,WRKRINDX                           
         CLI   ERRORSW,1                                                        
         BE    DORPT                                                            
         MVI   PHASESW,2                                                        
         B     OPENREC                                                          
         SPACE 2                                                                
END2     CLI   RCWRITE,C'Y'                                                     
         BNE   DORPT                                                            
         GOTO1 WORKER,DMCB,=C'DELETE',AWRKR4K,WRKRINDX                          
         SPACE 2                                                                
DORPT    BAS   RE,PRECAP                                                        
         L     R8,WHERE2                                                        
         USING COUNTD,R8                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R7,P                                                             
         USING RPTA,R7                                                          
DORPT2   MVC   RPTAPRD,CNTPRD                                                   
         MVC   RPTACLT,CNTCLT                                                   
         EDIT  CNTEST,(3,RPTAEST)                                               
         EDIT  CNTCNT,(07,RPTARCNT)                                             
         L     RF,VERRDESC                                                      
DORPT4   CLI   0(RF),X'FF'         FIND ERROR MESSAGE                           
         BE    DORPT5                                                           
         CLC   CNTFLG(1),0(RF)                                                  
         BE    DORPT5                                                           
         LA    RF,ERRENT(RF)                                                    
         B     DORPT4                                                           
DORPT5   MVC   RPTAMSG,2(RF)                                                    
         SPACE 2                                                                
DORPT6   CLI   1(RF),1             NORMAL PRODUCT REPORT                        
         BE    DORPT10              YES - PRINT IT                              
         CLI   1(RF),2             MARKET REPORT                                
         BE    *+6                  YES - REFORMAT                              
         DC    H'0'                TABLE ERROR                                  
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         LA    RE,CLIST                                                         
         LA    R0,220                                                           
DORPT8   CLC   CNTMPRD,3(RE)                                                    
         BE    DORPT8A                                                          
         LA    RE,4(RE)                                                         
         BCT   R0,DORPT8                                                        
         DC    H'0'                                                             
DORPT8A  MVC   RPTAPRD,0(RE)                                                    
         EDIT  CNTMKT,(4,RPTAMKT)                                               
DORPT10  GOTO1 REPORT                                                           
         LA    R8,LCNTTAB(R8)                                                   
*        CLI   0(R8),0                                                          
*        BNE   DORPT2                                                           
         OC    0(5,R8),0(R8)                                                    
         BNZ   DORPT2                                                           
         CLI   RCWRITE,C'Y'                                                     
         BNE   EXIT                                                             
         CLI   ERRORSW,1                                                        
         BNE   OPENREC                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   OPENREC                                                          
         GOTO1 WORKER,DMCB,=C'DELETE',AWRKR4K,WRKRINDX                          
         B     OPENREC                                                          
         DROP  R6                                                               
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
* CLEAR THE BRAND RECAP TABLE                                                   
CLRBRT   NTR1                                                                   
         XC    BRTCNT,BRTCNT                                                    
*--->    L     RF,=A(BRTABC)                                                    
         L     RF,WHERE                                                         
         USING BRTABD,RF                                                        
         XC    BRTST(BRTCURR-BRTST),BRTST                                       
         MVC   BRTCURR,=PL8'0'                                                  
         MVC   BRTIN(16),BRTCURR                                                
         LR    RE,RF                                                            
         LA    RF,BRTEN-BRTST(RF)                                               
         LA    R9,MAXPRD                                                        
         BCTR  R9,0                                                             
         MVC   0(BRTEN-BRTST,RF),0(RE)                                          
         LA    RF,BRTEN-BRTST(RF)                                               
         BCT   R9,*-10                                                          
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
EXTGOAL  NTR1                      XTACT INFO FROM GOAL RECORD                  
         L     R5,AREC                                                          
         USING GOALREC,R5                                                       
         SR    RE,RE                                                            
         ICM   RE,3,GLENGTH                                                     
         AR    RE,R5                                                            
         XC    0(5,RE),0(RE)                                                    
         LA    R7,GDELEM                                                        
         USING GLEMENT,R7                                                       
EXTGOAL1 CLI   GLCODE,0            END                                          
         BE    EXTGOALX                                                         
         CLI   GLCODE,X'21'        REGULAR GOAL ELEMENT                         
         BE    EXTGOAL3                                                         
EXTGOAL2 SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
         AR    R7,RE                                                            
         B     EXTGOAL1                                                         
         SPACE 2                                                                
EXTGOAL3 L     RE,=A(WKTI)                                                      
         USING WKTABD,RE                                                        
EXTGOAL4 CLC   WKTWK,GLWEEK        ADD OR CREATE WEEKLY ITEMS                   
         BE    EXTGOAL5                                                         
         CLI   WKTWK,0                                                          
         BE    EXTGOAL5                                                         
         LA    RE,WKTEN-WKTST(RE)  NEXT                                         
         B     EXTGOAL4                                                         
         SPACE 2                                                                
EXTGOAL5 MVC   WKTWK,GLWEEK                                                     
         SR    R8,R8                                                            
         SR    R9,R9                                                            
         ICM   R8,15,GLGRP                                                      
         ICM   R9,15,GLBUDGET                                                   
         SPACE 2                                                                
         CLI   INTYPE,C'P'                                                      
         BE    EXTGOAL6                                                         
         CLI   INTYPE,C'I'                                                      
         BE    EXTGOAL8                                                         
         ICM   RF,15,WKTDOLO       INPUT FROM EXISTING FILE                     
         AR    RF,R9                                                            
         STCM  RF,15,WKTDOLO                                                    
         ICM   RF,15,WKTDEMO                                                    
         AR    RF,R8                                                            
         STCM  RF,15,WKTDEMO                                                    
         B     EXTGOAL2                                                         
         SPACE 2                                                                
EXTGOAL6 ICM   RF,15,WKTDOLC       INPUT FROM EXISTING FILE                     
         AR    RF,R9                                                            
         STCM  RF,15,WKTDOLC                                                    
         ICM   RF,15,WKTDEMP                                                    
         AR    RF,R8                                                            
         STCM  RF,15,WKTDEMP                                                    
         B     EXTGOAL2                                                         
         SPACE 2                                                                
EXTGOAL8 ICM   RF,15,WKTDOLI       INPUT FROM TRANSFER                          
         AR    RF,R9                                                            
         STCM  RF,15,WKTDOLI                                                    
         ICM   RF,15,WKTDEMI                                                    
         AR    RF,R8                                                            
         STCM  RF,15,WKTDEMI                                                    
         B     EXTGOAL2                                                         
         SPACE 2                                                                
EXTGOALX B     EXIT                                                             
         EJECT                                                                  
SUMINP   NTR1                                                                   
         XC    WORK,WORK                                                        
         MVI   ACTIVITY,1                                                       
         LA    RF,WORK                                                          
         USING BRTABD,RF                                                        
         MVC   BRTMKT,CURRMKT                                                   
         MVC   BRTPRD,CURRPRD                                                   
         MVC   BRTCLT,CURRCLT                                                   
         MVC   BRTEST,CURREST                                                   
         MVC   BRTCURR,=PL8'0'                                                  
         MVC   BRTIN(16),BRTCURR                                                
*--->    L     R9,=A(BRTABC)                                                    
         L     R9,WHERE                                                         
         GOTO1 BINSRCH,DMCB,(1,WORK),(R9),BRTCNT,BRTILN,8,BRTMAX                
         MVC   BRTCNT,8(R1)                                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            SET TO TABLE FOR THIS PRD                    
         L     RE,=A(WKTI)         ADD IN WEEKLY DATA                           
SUMINP1  OC    WKTST,WKTST         END                                          
         BZ    SUMINPX                                                          
         ICM   R8,15,WKTDOLC       ADD UP CURRENT DOLLARS                       
         CVD   R8,DUB                                                           
         AP    BRTCURR,DUB                                                      
         ICM   R8,15,WKTDOLI       ADD UP INPUT DOLLARS                         
         CVD   R8,DUB                                                           
         AP    BRTIN,DUB                                                        
         ICM   R9,15,WKTDOLO                                                    
         SR    R8,R9               GET THE CHANGE                               
         CVD   R8,DUB                                                           
         AP    BRTCHG,DUB                                                       
         LA    RE,WKTEN-WKTST(RE)                                               
         B     SUMINP1                                                          
*                                                                               
SUMINPX  B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
GETPRV   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R5,KEY              SET TO READ FOR CURRENT CLIENT               
         MVI   KEY,X'02'                                                        
         MVC   KEY+1(6),SVGKAM                                                  
         MVC   KEY+4(1),SVGFPRD                                                 
         GOTO1 HIGH                                                             
         B     GETPRV2                                                          
GETPRV1  GOTO1 SEQ                                                              
GETPRV2  CLC   KEY+1(6),KEYSAVE+1  END OF CLIENT - EXIT                         
         BNE   GETPRVX                                                          
         L     RE,=A(PRDEST)                                                    
GETPRV3  CLI   0(RE),0             END - GET NEXT RECORD                        
         BE    GETPRV1                                                          
         CLC   3(1,RE),GKEYPRD     CHECK ACTIVE PRODUCT                         
         BNE   GETPRV4                                                          
         CLC   4(1,RE),GKEYEST     AND ESTIMATE                                 
         BNE   GETPRV4                                                          
         MVC   CURRPRD,0(RE)                                                    
         MVC   CURREST,GKEYEST                                                  
         MVC   AREC,ADBUY                                                       
         GOTO1 GET                 READ A GOAL RECORD                           
         MVI   INTYPE,C'P'                                                      
         L     RE,=A(WKTI)                                                      
         LA    RF,WKTIEN-WKTI                                                   
         XCEF                                                                   
         BAS   RE,EXTGOAL          EXTRACT THE GOAL DOLLARS                     
         BAS   RE,SUMINP           AND POST IN PRIOR BUCKETS                    
         B     GETPRV1                                                          
         SPACE 2                                                                
GETPRV4  LA    RE,5(RE)            TRY NEXT PRODUCT / ESTIMATE                  
         B     GETPRV3                                                          
*                                                                               
GETPRVX  B     EXIT                                                             
         EJECT                                                                  
PRECAP   NTR1                                                                   
         BAS   RE,GETPRV                                                        
         L     R8,WHERE              =A(BRTABC)                                 
         USING BRTABD,R8                                                        
         LA    R7,P                                                             
         USING FILINE,R7                                                        
         MVC   TOTPREV,=PL8'0'                                                  
         MVC   TOTIN(24),TOTPREV                                                
         L     R9,BRTCNT           NUMBER OF ENTRIES                            
         LTR   R9,R9               EXIT IF NO ENTRIES                           
         BZ    PRECAPX                                                          
         SPACE 2                                                                
PRECAPA  MVC   FIPRD,BRTPRD        PRINT NUMERIC PRODUCT IF AVAILABLE           
         LA    R1,CCPTAB                                                        
PRECAPB  CLI   0(R1),X'FF'                                                      
         BE    PRECAPC                                                          
         CLC   BRTPRD,2(R1)                                                     
         BE    *+12                                                             
         LA    R1,LNCCPTAB(R1)                                                  
         B     PRECAPB                                                          
         MVC   FIPRD,SPACES                                                     
         MVC   FIPRD(2),0(R1)                                                   
         SPACE 2                                                                
PRECAPC  DS    0H                                                               
         EDIT  (B2,BRTMKT),(4,FIMKT)                                            
         EDIT  (B1,BRTEST),(3,FIEST)                                            
         EDIT  (P8,BRTCURR),(14,FICURR),2,MINUS=YES                             
         EDIT  (P8,BRTIN),(14,FIIN),2,MINUS=YES                                 
         MVC   AREC,ADCLT                                                       
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVGKAM                                                  
         MVC   KEY+2(2),BRTCLT                                                  
         GOTO1 HIGH                                                             
         GOTO1 GET                                                              
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         MVC   FICNAME+4(14),CNAME                                              
*        EDIT  (B2,CCLTINTR),(3,FICNAME)   OLD INTERFACE                        
         MVC   FICNAME(3),CCLTIFC                                               
         MVC   DUB,BRTCURR         CALCULATE PREVIOUS                           
         SP    DUB,BRTCHG                                                       
         AP    TOTCURR,BRTCURR     ADD UP TOTALS                                
         AP    TOTIN,BRTIN                                                      
         AP    TOTPREV,DUB                                                      
         AP    TOTCHG,BRTCHG                                                    
         EDIT  (P8,DUB),(14,FIPREV),2                                           
         EDIT  (P8,BRTCHG),(14,FICHG),2,MINUS=YES                               
         GOTO1 REPORT                                                           
         LA    R8,BRTEN-BRTST(R8)                                               
         BCT   R9,PRECAPA                                                       
         MVC   FICNAME(14),=C'****TOTAL**** '                                   
         EDIT  (P8,TOTCURR),(14,FICURR),2                                       
         EDIT  (P8,TOTIN),(14,FIIN),2                                           
         EDIT  (P8,TOTPREV),(14,FIPREV),2                                       
         EDIT  (P8,TOTCHG),(14,FICHG),2,MINUS=YES                               
         GOTO1 REPORT                                                           
PRECAPX  B     EXIT                                                             
*---------------------------------------------------------------------          
*        ADD CD REQUEST FOR SECOND STEP                                         
*---------------------------------------------------------------------          
ADDCDREQ NTR1                                                                   
*                                                                               
         LA    R6,P                                                             
REQ      USING QAREA,R6                                                         
*                                                                               
         MVC   P(80),SPACES                                                     
         MVC   REQ.QCODE,=C'CD'                                                 
         MVC   REQ.QAGY,QAGY                                                    
         MVC   REQ.QMED,QMED                                                    
         MVC   REQ.QCLT,WRKREC+4                                                
         MVC   REQ.QPRD,CURRPRD                                                 
         EDIT  (B1,CURREST),(3,REQ.QEST),FILL=0                                 
         EDIT  (B2,CURRMKT),(4,REQ.QMKT),FILL=0                                 
         MVI   REQ.QOPT1,C'W'      TELL CD REQ IS FROM GW                       
*                                                                               
         L     R5,=A(WKTI)         GET WEEKS WITH CHANGES                       
         USING WKTABD,R5                                                        
ADDCD20  CLI   WKTWK,0                                                          
         BE    ADDCDX                                                           
         OC    WKTDOLI,WKTDOLI     INPUT FROM TRANSFER                          
         BZ    ADDCD40                                                          
         CLC   WKTDOLO,WKTDOLI     NO CHANGE?                                   
         BE    ADDCD40                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,WKTWK),(0,WORK)      GET BRD MONTH                
         GOTO1 GETBROAD,DMCB,WORK,WORK+6,=V(GETDAY),=V(ADDAY)                   
         MVC   REQ.QEND(4),WORK+12                                              
*                                                                               
         CLC   LASTREQ,P           SAME AS ANY OF LAST 3                        
         BE    ADDCD40                                                          
         CLC   LASTREQ2,P                                                       
         BE    ADDCD40                                                          
         CLC   LASTREQ3,P                                                       
         BE    ADDCD40                                                          
ADDCD30  L     R1,=A(CDRQFIL)                                                   
         LA    R0,P                                                             
         PUT   (1),(0)                                                          
         MVC   LASTREQ3,LASTREQ2   SAVE LAST 3 REQ (1 QUARTER)                  
         MVC   LASTREQ2,LASTREQ                                                 
         MVC   LASTREQ,P                                                        
*                                                                               
ADDCD40  LA    R5,WKTEN-WKTST(R5)  NEXT WEEK                                    
         B     ADDCD20                                                          
*                                                                               
ADDCDX   XIT1                                                                   
         DROP  REQ,R5                                                           
         EJECT                                                                  
*---------------------------------------------------------------------          
*        DCB LTORG AND TABLES                                                   
*---------------------------------------------------------------------          
*                                                                               
CDRQFIL  DCB   DDNAME=CDRQFIL,DSORG=PS,RECFM=FB,BLKSIZE=3200,          X        
               LRECL=80,MACRF=PM                                                
*                                                                               
         LTORG                                                                  
*                                                                               
CCPTBO   DC    C'01',C'CC '        OLD PRODUCT TABLE                            
         DC    C'02',C'FR '                                                     
         DC    C'03',C'TB '                                                     
         DC    C'04',C'SP '                                                     
         DC    C'05',C'PB '                                                     
         DC    C'06',C'MY '                                                     
         DC    C'07',C'FN '                                                     
         DC    C'09',C'RB '                                                     
         DC    C'12',C'DC '                                                     
         DC    C'13',C'CY '                                                     
         DC    X'FF'                                                            
LNCCPTBO EQU   5                                                                
CCPTAB   DC    C'01',C'CL '                                                     
         DC    C'02',C'FR '                                                     
         DC    C'03',C'TB '                                                     
         DC    C'04',C'SP '                                                     
         DC    C'06',C'MY '                                                     
         DC    C'08',C'CC '                                                     
         DC    C'11',C'MM '                                                     
         DC    C'12',C'DC '                                                     
         DC    C'13',C'CY '                                                     
         DC    C'15',C'PA '                                                     
         DC    C'70',C'NT '                                                     
         DC    X'FF'                                                            
LNCCPTAB EQU   5                                                                
*                                                                               
SVGKAM   DS    CL3                 SAVE ORIGINAL AGENCY MEDIA                   
SVGKPM   DS    CL3                 SAVE ORIGINAL PRODUCT/MARKET                 
SVGFPRD  DS    C                                                                
INTYPE   DS    C                                                                
CURRMKT  DS    CL2                                                              
CURRPRD  DS    CL3                                                              
CURRCLT  DS    CL2                                                              
CURREST  DS    CL1                                                              
ACTIVITY DC    X'00'                                                            
PHASESW  DC    X'01'                                                            
ERRORSW  DC    X'00'                                                            
RELO     DC    F'0'                                                             
ETCNTR   DC    F'0'                NO OF ENTRIES IN EST TABLE                   
CTCNTR   DC    F'0'                NO OF ENTRIES IN COUNT TABLE                 
MKTCNTR  DC    F'0'                                                             
VESTTAB  DC    F'0'                                                             
VMKTTAB  DC    F'0'                                                             
VERRDESC DC    F'0'                                                             
SAVEF    DS    D                                                                
LASTREQ  DS    CL80                                                             
LASTREQ2 DS    CL80                                                             
LASTREQ3 DS    CL80                                                             
*                                                                               
TOTPREV  DS    CL8                                                              
TOTIN    DS    CL8                                                              
TOTCURR  DS    CL8                                                              
TOTCHG   DS    CL8                                                              
*                                                                               
*--->    BRTLNC   EQU   BRTENC-BRTABC                                           
BRTLNC   EQU   ((BRTEN-BRTST)*MAXPRD*200)                                       
BRTILN   DC    A(BRTEN-BRTST)                                                   
BRTMAX   DC    A(BRTLNC/(BRTEN-BRTST))                                          
BRTCNT   DC    A(0)                                                             
ABRTLNC  DC    A(BRTLNC)                                                        
AWRKR4K  DC    A(WRKR4K)                                                        
WHERE    DS    F                                                                
AMOUNT   DS    F                                                                
WHERE2   DS    F                                                                
AMOUNT2  DS    F                                                                
SVUSRID  DS    H                                                                
WRKRINDX DS    CL16                                                             
WRKREC   DS    1000C                                                            
WRKR4K   DS    6000C                                                            
* BRTABC   DS    ((BRTEN-BRTST)*MAXPRD*200)C                                    
* BRTENC   DS    0C                                                             
WKTI     DS    ((WKTEN-WKTST)*MAXWEEK)C                                         
WKTIEN   DS    0C                                                               
PRDEST   DS    0CL5                                                             
PRDESTST DS    CL(5*400)                                                        
PRDESTEN DS    0C                                                               
         EJECT                                                                  
RPTA     DSECT                                                                  
         DS    CL1                                                              
RPTACLT  DS    CL3                                                              
         DS    CL1                                                              
RPTAPRD  DS    CL3                                                              
         DS    CL1                                                              
RPTAEST  DS    CL3                                                              
         DS    CL2                                                              
RPTAMKT  DS    CL4                                                              
         DS    CL2                                                              
RPTARCNT DS    CL7                                                              
         DS    CL2                                                              
RPTAMSG  DS    CL27                                                             
COUNTD   DSECT                                                                  
CNTCLT   DS    CL3                                                              
CNTPRD   DS    CL3                                                              
         ORG   CNTPRD                                                           
CNTMKT   DS    CL2                                                              
CNTMPRD  DS    C                                                                
CNTEST   DS    C                                                                
CNTFLG   DS    C                                                                
CNTCNT   DS    CL2                                                              
LCNTTAB  EQU   10                                                               
         SPACE 2                                                                
ESTTABD  DSECT                                                                  
ETPRD    DS    C                                                                
ETEST    DS    C                                                                
ETSTART  DS    CL2                                                              
ETEND    DS    CL2                                                              
LESTTAB  EQU   6                                                                
         SPACE 2                                                                
PRDNF    EQU   1                                                                
ESTNF    EQU   2                                                                
OUTDATE  EQU   3                                                                
MKTERR   EQU   40                                                               
GOODREC  EQU   4                                                                
         EJECT                                                                  
** THESE DSECTS ARE FOR THE BALANCING REPORT                                    
MAXPRD   EQU   220                 MAXIMUM NUMBER OF PRODUCTS                   
BRTABD   DSECT                     AREA FOR BRAND DOLLARS                       
BRTST    DS    0C                                                               
BRTMKT   DS    CL2                 MARKET                                       
BRTPRD   DS    CL3                 PRODUCT                                      
BRTCLT   DS    CL2                 CLIENT                                       
BRTEST   DS    CL2                 ESTIMATE                                     
BRTCURR  DS    PL8                 CURRENT DOLLARS                              
BRTIN    DS    PL8                 INPUT FILE DOLLARS                           
BRTCHG   DS    PL8                 ACTUAL CHANGE                                
BRTEN    DS    C                                                                
         SPACE 2                                                                
MAXWEEK  EQU   53                                                               
WKTABD   DSECT                     TABLE FOR WEEKLY GOALS                       
WKTST    DS    0C                                                               
WKTWK    DS    CL2                 WEEK DATE (MONDAY)                           
WKTDOLC  DS    CL4                                                              
WKTDEMP  DS    CL4                                                              
WKTDOLI  DS    CL4                                                              
WKTDEMI  DS    CL4                                                              
WKTDOLO  DS    CL4                                                              
WKTDEMO  DS    CL4                                                              
WKTEN    DS    0C                                                               
         SPACE 2                                                                
FILINE   DSECT                     BALANCING REPORT LINE                        
FIMKT    DS    CL4                                                              
         DS    CL2                                                              
FIPRD    DS    CL3                                                              
         DS    CL2                                                              
FICNAME  DS    CL20                                                             
         DS    CL2                                                              
FIEST    DS    CL3                                                              
         DS    CL2                                                              
FIPREV   DS    CL14                                                             
         DS    CL2                                                              
FIIN     DS    CL14                                                             
         DS    CL2                                                              
FICURR   DS    CL14                                                             
         DS    CL2                                                              
FICHG    DS    CL14                                                             
         EJECT                                                                  
ERRDESC  CSECT                                                                  
         DC    AL1(PRDNF),AL1(1)                                                
         DC    CL27'PRODUCT NOT FOUND'                                          
         DC    AL1(ESTNF),AL1(1)                                                
         DC    CL27'ESTIMATE NOT FOUND'                                         
         DC    AL1(OUTDATE),AL1(1)                                              
         DC    CL27'DATA OUTSIDE ESTIMATE DATES'                                
         DC    AL1(MKTERR),AL1(2)                                               
         DC    CL27'MARKET NOT FOUND'                                           
         DC    AL1(GOODREC),AL1(1)                                              
         DC    CL27'RECORDS ADDED OR UPDATED'                                   
         DC    AL1(255),AL1(1)                                                  
         DC    CL27'UNDEFINED ERROR CONDITION'                                  
ERRENT   EQU   29                                                               
ESTTAB   CSECT                                                                  
         DS    1800C                                                            
*CNTTAB   CSECT                                                                 
*         DS    160000C                                                         
MKTTAB   CSECT                                                                  
         DS    6000C                                                            
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE DMWRKRK                                                        
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPGW02 05/01/02'                                      
         END                                                                    
