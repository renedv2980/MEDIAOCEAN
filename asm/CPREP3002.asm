*          DATA SET CPREP3002  AT LEVEL 069 AS OF 05/01/02                      
*PHASE CP3002A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'CPREP3002-ONE SPOT PER MARKET REPORT'                           
         PRINT NOGEN                                                            
CP3002   CSECT                                                                  
         NMOD1 0,CP3002,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING CPWORKD,RA,RC                                                    
         L     R2,=V(CP30WK)                                                    
         USING CP30WK,R2                                                        
         AR    R2,R5                                                            
         ST    R5,RELO                                                          
         STM   RA,RC,CP30RA                                                     
         ST    R2,CP30R2                                                        
         LA    RE,MYHEAD                                                        
         ST    RE,HEADHOOK                                                      
         CLI   MODE,RUNFRST                                                     
         BNE   *+8                                                              
         MVI   ACTSW,1                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   BK20                                                             
         CLI   ACTSW,2             SORT ALREADY OPEN                            
         BE    BK09                                                             
         L     R6,=V(SORTER)                                                    
         A     R6,RELO                                                          
         ST    R6,VSORTER                                                       
         GOTO1 VSORTER,DMCB,SORT,RECCARD,0                                      
BK09     MVI   SVTGT,0                                                          
         MVI   ACTSW,0                                                          
         LA    RE,HOMES            DEFAULT                                      
         CLC   QTARGET,=C'   '                                                  
         BE    BK09A                                                            
         PACK  DUB,QTARGET                                                      
         CVB   RE,DUB                                                           
BK09A    STC   RE,SRTGT                                                         
         STC   RE,PLTGT                                                         
         LA    RE,RTGGRP                                                        
BK10     CLC   SRTGT,0(RE)         FIND TARGET GROUP                            
         BE    BK10A                                                            
         LA    RE,RTGGRPLN(RE)                                                  
         CLI   0(RE),X'FF'         BYPASS INVALID DEMOS                         
         BE    EXIT                                                             
         B     BK10                                                             
BK10A    MVC   SVTGT,1(RE)         SET TARGET GROUP CODE                        
         ST    RE,SVTGTA           SAVE TARGET ADDRESS                          
         MVI   ACTSW,0                                                          
         B     EXIT                                                             
         SPACE 2                                                                
BK20     CLI   MODE,PROCDATA                                                    
         BNE   BK40                                                             
         OC    MKTRANK,MKTRANK                                                  
         BZ    EXIT                                                             
         L     R3,ADDATA                                                        
         USING CPKEYD,R3                                                        
         CLI   SVTGT,0                                                          
         BE    EXIT                                                             
         MVI   ACTSW,1                                                          
         L     RE,SVTGTA                                                        
         XC    SRREC,SRREC                                                      
         MVC   SRDEMO,CPKDEMO                                                   
         LA    RE,RTGGRP                                                        
BK20A    CLC   CPKTARGT,0(RE)      FIND TARGET GROUP                            
         BE    BK20B                                                            
         LA    RE,RTGGRPLN(RE)                                                  
         CLI   0(RE),X'FF'         BYPASS DEMOS WHICH ARE NOT GROUPED           
         BE    EXIT                                                             
         B     BK20A                                                            
BK20B    MVI   6(RE),1                                                          
         MVC   CPDGRP,1(RE)                                                     
         MVC   CPDGRP(1),SVTGT                                                  
         CLC   QTARGET,=C'   '                                                  
         BNE   *+12                                                             
         MVI   CPDGRP,C'3'                                                      
         MVI   SRTGT,HOMES                                                      
         CLI   QOPT2,C' '          OVERRIDE GROUP                               
         BE    *+14                                                             
         MVC   CPDGRP(1),QOPT2                                                  
         MVI   CPDGRP+1,C' '                                                    
         MVC   CPDPT,CPKDAYPT                                                   
         MVI   SRSLN,0             ZERO SPOT LENGTH                             
         MVC   SRMKT,CPKMKT                                                     
         MVC   SRWGHT+2(2),MKTWT                                                
         CLI   QOPT1,C'N'          SUPPRESS WEIGHTS                             
         BNE   *+10                                                             
         MVC   SRWGHT,=F'1'                                                     
         LA    R4,PERTABLE                                                      
         USING CPERD,R4                                                         
         L     R8,NPERIODS                                                      
BK20C    L     RE,SRDOL                                                         
         A     RE,CPCASH                                                        
         ST    RE,SRDOL                                                         
         L     RE,SRSPOT                                                        
         A     RE,CPSPOTS                                                       
         ST    RE,SRSPOT                                                        
         L     RE,SRPNT                                                         
         A     RE,CPOINTS                                                       
         ST    RE,SRPNT                                                         
         L     RE,SRIMP                                                         
         A     RE,CPIMPS                                                        
         ST    RE,SRIMP                                                         
         L     RE,SRDOL            EQUIVALENCE DOLLARS                          
         MH    RE,=H'1000'                                                      
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,EQVFACT                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,SREQUIV                                                       
         A     R4,WIDTHPER                                                      
         BCT   R8,BK20C                                                         
         SPACE 2                                                                
* FIND DAYPARTS FOR DEMO GROUP                                                  
         LA    R5,CPDGRP                                                        
BK20D    CLI   0(R5),C' '                                                       
         BE    EXIT                                                             
         LA    RE,DEMGTAB                                                       
BK20D1   CLI   0(RE),X'FF'                                                      
         BE    EXIT                                                             
         CLC   0(1,RE),0(R5)       SAME GROUP                                   
         BE    BK20D2                                                           
         MVC   HALF,5(RE)                                                       
         AH    RE,HALF                                                          
         B     BK20D1                                                           
BK20D2   MVC   SRTGT,4(RE)                                                      
         SPACE 2                                                                
* EXPLODE DAYPARTS                                                              
         LA    R7,31(RE)                                                        
BK20E1   CLI   0(R7),0                                                          
         BNE   BK20E2                                                           
         B     EXIT                                                             
BK20E2   DS    0H                                                               
         CLC   CPDPT,0(R7)                                                      
         BE    PUTSORT                                                          
         LA    R7,2(R7)                                                         
         B     BK20E1                                                           
         SPACE 2                                                                
* GENERATE SORT RECORDS                                                         
PUTSORT  XC    SRREN,SRREN         CREATE MARKET RECORDS                        
         MVC   SRRST,MKTRANK                                                    
         MVC   SRDPT,1(R7)                                                      
         BAS   R9,PSOK             PUT DETAIL RECORD                            
         MVC   HALF,MKTRANK                                                     
         LH    RE,HALF                                                          
         BCTR  RE,0                                                             
         SRDA  RE,32                                                            
         D     RE,=F'10'                                                        
         LA    RF,1(RF)                                                         
         MH    RF,=H'10'                                                        
         ST    RF,FULL                                                          
         MVC   SRREN,FULL+2                                                     
         S     RF,=F'9'                                                         
         STH   RF,FULL                                                          
         CLC   FULL(2),=H'51'                                                   
         BL    PSSETENX                                                         
         CLC   FULL(2),=H'100'                                                  
         BH    PSSETEN1                                                         
         MVC   FULL(2),=H'51'                                                   
         MVC   SRREN,=H'100'                                                    
         B     PSSETENX                                                         
PSSETEN1 DS    0H                                                               
         MVC   FULL(2),=H'101'                                                  
         MVC   SRREN,=H'200'                                                    
PSSETENX CLC   SRREN,HIGROUP       SET HIGHEST ACTIVE GROUP                     
         BL    *+10                                                             
         MVC   HIGROUP,SRREN                                                    
         XC    FULL(2),=X'FFFF'    COMPLIMENT START GROUP                       
         MVC   SRRST,FULL                                                       
         BAS   R9,PSOK             PUT SUBGROUP RECORD                          
         LA    R7,2(R7)                                                         
         B     BK20E1                                                           
         SPACE 2                                                                
PSOK     DS    0H                                                               
PSOK1    GOTO1 VSORTER,DMCB,=C'PUT',SRREC                                       
         BR    R9                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
BK40     CLI   MODE,REQLAST                                                     
         BNE   BK50                                                             
         XC    HLD2REC,HLD2REC                                                  
         XC    HOLDREC,HOLDREC                                                  
         XC    DPAREA(112),DPAREA                                               
         MVI   EOFSW,0                                                          
         CLI   ACTSW,0                                                          
         BNE   PR1                                                              
         MVI   ACTSW,2             BYPASS SORT OPEN                             
         B     EXIT                                                             
         SPACE 2                                                                
BK50     CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         CLI   ACTSW,2             SORT OPENED BUT NOT CLOSED                   
         BNE   EXIT                                                             
         GOTO1 VSORTER,DMCB,=C'END'                                             
         B     EXIT                                                             
         EJECT                                                                  
PR1      MVC   SRREC,HLD2REC                                                    
         CLI   EOFSW,1                                                          
         BE    SD1                                                              
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     RE,4(R1)                                                         
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   PR3                                                              
         MVI   EOFSW,1                                                          
         XC    HLD2REC,HLD2REC                                                  
         GOTO1 VSORTER,DMCB,=C'END'                                             
         B     PRX                                                              
         SPACE 2                                                                
PR3      MVC   HLD2REC,0(RE)       ACCUMULATE DUPS                              
         OC    SRREC,SRREC                                                      
         BZ    PR1                                                              
         CLC   HLD2KEY,SRREC                                                    
         BNE   PRX                                                              
         LA    RF,HLD2DATA                                                      
         LA    RE,SRSPOT                                                        
         LA    R9,4                                                             
         LA    R1,0                                                             
PR4      L     R8,0(R1,RE)                                                      
         A     R8,0(R1,RF)                                                      
         ST    R8,0(R1,RF)                                                      
         LA    R1,4(R1)                                                         
         BCT   R9,PR4                                                           
         LA    R1,4(R1)                                                         
         L     R8,0(R1,RE)                                                      
         A     R8,0(R1,RF)                                                      
         ST    R8,0(R1,RF)                                                      
         B     PR1                                                              
PRX      DS    0H                                                               
         EJECT                                                                  
* SUM RECORDS INTO TABLE IF DETAIL ITEM                                         
SD1      OC    HLDREN,HLDREN       DETAIL ITEM                                  
         BNZ   SM1                  NO - ONE SPM ROUTINES                       
         CLI   RCSUBPRG,1                                                       
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         MVC   SLTTGT,SRTGT                                                     
         MVC   SLTDPT,SRDPT                                                     
         OC    HOLDREC,HOLDREC                                                  
         BZ    SD2                                                              
         CLC   SRMKT,HLDMKT        MARKET EQUAL                                 
         BNE   CPMDR                NO - DO REPORT                              
SD2      BAS   R9,FSLOT            FIND DAYPART SLOT                            
         L     RE,SLOT                                                          
         BCTR  RE,0                                                             
         MH    RE,=H'28'                                                        
         LA    R4,DPAREA                                                        
         USING DPD,R4                                                           
         MVC   DPDWGHT,SRWGHT                                                   
         AR    R4,RE                                                            
         MVC   DPDWGHT,SRWGHT                                                   
         L     RE,DPDSPT                                                        
         A     RE,SRSPOT                                                        
         ST    RE,DPDSPT                                                        
         L     RE,DPDDOL                                                        
         A     RE,SRDOL                                                         
         ST    RE,DPDDOL                                                        
         L     RE,DPDDOLE                                                       
         A     RE,SREQUIV                                                       
         ST    RE,DPDDOLE                                                       
         L     RE,DPDPNT                                                        
         A     RE,SRPNT                                                         
         ST    RE,DPDPNT                                                        
         L     RE,DPDIMP                                                        
         A     RE,SRIMP                                                         
         ST    RE,DPDIMP                                                        
         MVC   HOLDREC,SRREC                                                    
         B     PR1                                                              
         EJECT                                                                  
         DROP  R4                                                               
* SUM RECORDS INTO OSP BUCKETS FOR GROUP ITEM                                   
SM1      CLC   SRMKT,HLDMKT                                                     
         BNE   SM2                                                              
         CLC   SRREN(4),HLDREN                                                  
         BNE   SM2                                                              
         CLC   HLDDPT,SRDPT                                                     
         BNE   SM2                                                              
SM1A     CLI   RCSUBPRG,1                                                       
         BNE   SM1B                                                             
         MVI   FORCEHED,C'Y'                                                    
         XC    DPGAREA(112),DPGAREA                                             
SM1B     DS    0H                                                               
         MVI   RCSUBPRG,2                                                       
         MVC   OSPWGHT,SRWGHT      SUMMARIZE A MARKET                           
         L     RF,SREQUIV                                                       
         A     RF,OSPDOLE                                                       
         ST    RF,OSPDOLE                                                       
         L     RF,OSPSPT                                                        
         A     RF,SRSPOT                                                        
         ST    RF,OSPSPT                                                        
         L     RF,OSPDOL                                                        
         A     RF,SRDOL                                                         
         ST    RF,OSPDOL                                                        
         L     RF,OSPPNT                                                        
         A     RF,SRPNT                                                         
         ST    RF,OSPPNT                                                        
         L     RF,OSPIMP                                                        
         A     RF,SRIMP                                                         
         ST    RF,OSPIMP                                                        
         MVI   MGRPSW,0                                                         
         MVC   SLTTGT,SRTGT                                                     
         MVC   SLTDPT,SRDPT                                                     
         MVC   HOLDREC,SRREC                                                    
         B     PR1                                                              
         SPACE 2                                                                
SM2      L     RE,OSPDOL           AVERAGE SPOT FOR MARKET                      
         BAS   R9,OSPDIV                                                        
         ST    RE,OSPDOL                                                        
         L     RE,OSPDOLE                                                       
         BAS   R9,OSPDIV                                                        
         ST    RE,OSPDOLE                                                       
         L     RE,OSPPNT                                                        
         BAS   R9,OSPDIV                                                        
         ST    RE,OSPPNT                                                        
         L     RE,OSPIMP                                                        
         BAS   R9,OSPDIV                                                        
         ST    RE,OSPIMP                                                        
         L     RE,OSPPNT                                                        
         MH    RE,OSPWGHT+2                                                     
         ST    RE,OSPPNT                                                        
         BAS   R9,FSLOT                                                         
         L     RE,SLOT                                                          
         BCTR  RE,0                                                             
         MH    RE,=H'28'                                                        
         LA    R4,DPAREA                                                        
         USING DPD,R4                                                           
         AR    R4,RE                                                            
SM3      L     RE,DPDNMKT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,DPDNMKT                                                       
         L     RE,DPDWGHT                                                       
         A     RE,OSPWGHT                                                       
         ST    RE,DPDWGHT                                                       
         L     RE,DPDSPT                                                        
         A     RE,OSPSPT                                                        
         ST    RE,DPDSPT                                                        
         L     RE,DPDDOL                                                        
         A     RE,OSPDOL                                                        
         ST    RE,DPDDOL                                                        
         L     RE,DPDDOLE                                                       
         A     RE,OSPDOLE                                                       
         ST    RE,DPDDOLE                                                       
         L     RE,DPDPNT                                                        
         A     RE,OSPPNT                                                        
         ST    RE,DPDPNT                                                        
         L     RE,DPDIMP                                                        
         A     RE,OSPIMP                                                        
         ST    RE,DPDIMP                                                        
         CLI   MGRPSW,1                                                         
         BE    SM4                                                              
         LA    R4,112(R4)          SUMMARIZE MARKET GROUP                       
         MVI   MGRPSW,1                                                         
         B     SM3                                                              
SM4      DS    0H                                                               
         XC    OSPMAR,OSPMAR                                                    
         CLC   SRREN(4),HLDREN                                                  
         BE    SM1A                                                             
         B     CPMR                                                             
         EJECT                                                                  
OSPDIV   OC    OSPSPT,OSPSPT                                                    
         BNZ   OSPDIV1                                                          
         SR    RE,RE                                                            
         BR    R9                                                               
OSPDIV1  DS    0H                                                               
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,OSPSPT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         LR    RE,RF                                                            
         BR    R9                                                               
         SPACE 2                                                                
* FIND SLOT FOR PREV DAYPART                                                    
FSLOT    LA    RE,DEMGTAB                                                       
FSLOT1   CLC   SLTTGT,4(RE)                                                     
         BE    FSLOT2                                                           
         MVC   HALF,5(RE)                                                       
         AH    RE,HALF                                                          
         CLI   0(RE),X'FF'                                                      
         BNE   FSLOT1                                                           
         DC    H'0'                INVALID TARGET GROUP                         
         SPACE 2                                                                
FSLOT2   LA    RE,27(RE)           SET TO DAYPART SEQUENCE                      
         LA    RF,1                                                             
         LA    R8,4                                                             
FSLOT3   CLC   SLTDPT,0(RE)                                                     
         BE    FSLOT4                                                           
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R8,FSLOT3                                                        
         DC    H'0'                INVALID DAYPART                              
         SPACE 2                                                                
FSLOT4   ST    RF,SLOT                                                          
         BR    R9                                                               
         EJECT                                                                  
* MARKET DETAIL REPORT                                                          
CPMDR    L     RE,ADMKTTAB         GET MARKET NAME                              
         MVC   P(07),=C'UNKNOWN'                                                
         CLC   SRTGT(2),HLDTRGT                                                 
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
CPMDR1   CLC   HLDMKT,0(RE)                                                     
         BE    CPMDR2                                                           
         CLI   0(RE),X'FF'                                                      
         BE    CPMDR3                                                           
         LA    RE,34(RE)                                                        
         B     CPMDR1                                                           
CPMDR2   MVC   P+4(20),6(RE)                                                    
         MVI   P+3,C' '                                                         
         EDIT  HLDRST,(3,P)                                                     
         SPACE 2                                                                
CPMDR3   LA    R4,DPAREA                                                        
         USING DPD,R4                                                           
         LA    R5,4                                                             
         LA    R6,P+23                                                          
         LA    R6,P+24                                                          
CPMDR4   OC    DPDSPT,DPDSPT                                                    
         BZ    CPMDR11                                                          
         L     RE,DPDIMP                                                        
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,DPDSPT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,DPDIMP           AVE IMP PER SPOT                             
         L     RE,DPDDOLE                                                       
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,DPDSPT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,DPDDOLE          AVE DOLLARS PER SPOT                         
         EDIT  (RF),(4,0(R6))                                                   
         L     RE,DPDPNT                                                        
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,DPDSPT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,DPDPNT           AVE POINTS PER SPOT                          
         EDIT  (RF),(4,5(R6)),1                                                 
         L     RE,DPDDOLE                                                       
         SRDA  RE,32                                                            
         OC    DPDPNT,DPDPNT                                                    
         BZ    CPMDR7                                                           
         MVC   10(5,R6),=C' HIGH'                                               
         MH    RF,=H'1000'                                                      
         SLA   RF,1                                                             
         D     RE,DPDPNT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         CH    RF,=H'9999'                                                      
         BH    CPMDR5                                                           
         EDIT  (RF),(5,10(R6)),2                                                
         B     CPMDR7                                                           
CPMDR5   SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         CH    RF,=H'9999'                                                      
         BH    CPMDR6                                                           
         EDIT  (RF),(5,10(R6)),1                                                
         B     CPMDR7                                                           
CPMDR6   SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(5,10(R6))                                                  
         SPACE 2                                                                
CPMDR7   L     RE,DPDDOLE                                                       
         SRDA  RE,32                                                            
CPMDR8   OC    DPDIMP,DPDIMP                                                    
         BZ    CPMDR11                                                          
         MH    RF,=H'100'                                                       
         SLA   RF,1                                                             
         D     RE,DPDIMP                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         CH    RF,=H'999'                                                       
         BH    CPMDR9                                                           
         EDIT  (RF),(4,16(R6)),2                                                
         B     CPMDR11                                                          
CPMDR9   SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         CH    RF,=H'999'                                                       
         BH    CPMDR10                                                          
         EDIT  (RF),(4,16(R6)),1                                                
         B     CPMDR11                                                          
CPMDR10  SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         MVC   16(4,R6),=C'HIGH'                                                
         C     RF,=F'9999'                                                      
         BH    CPMDR11                                                          
         EDIT  (RF),(4,16(R6))                                                  
CPMDR11  LA    R4,DPDLEN(R4)                                                    
         LA    R6,21(R6)                                                        
         BCT   R5,CPMDR4                                                        
         GOTO1 REPORT                                                           
         B     RPTDONE                                                          
         DROP  R4                                                               
         EJECT                                                                  
* MARKET SUMMARY REPORT                                                         
CPMR     MVC   HALF,HLDREN                                                      
         LH    RF,HALF                                                          
         CH    RF,=H'10'                                                        
         BNE   *+8                                                              
         MVI   MGRPSW,0                                                         
         EDIT  (RF),(3,P+4),,ALIGN=LEFT                                         
         MVC   HALF,HLDRST                                                      
         XC    HALF,=X'FFFF'                                                    
         LH    RF,HALF                                                          
         EDIT  (RF),(3,P)                                                       
         MVI   P+3,C'-'                                                         
         SPACE 2                                                                
         LA    R4,DPAREA                                                        
         USING DPD,R4                                                           
         LA    R5,4                                                             
         LA    R6,P+8                                                           
CPMR1    OC    DPDNMKT,DPDNMKT                                                  
         BZ    CPM10                                                            
         EDIT  DPDNMKT,(3,0(R6))                                                
         L     RE,DPDPNT                                                        
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,DPDWGHT                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,DPDPNT                                                        
         EDIT  DPDDOLE,(4,4(R6))                                                
         L     RF,DPDPNT                                                        
         CH    RF,=H'99'                                                        
         BH    CPM1                                                             
         EDIT  DPDPNT,(3,9(R6)),1                                               
         B     CPM2                                                             
CPM1     SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(3,9(R6))                                                   
CPM2     DS    0H                                                               
         L     RE,DPDDOLE                                                       
         SRDA  RE,32                                                            
         OC    DPDPNT,DPDPNT                                                    
         BZ    CPM5                                                             
         MVC   13(5,R6),=C' HIGH'                                               
         MH    RF,=H'1000'                                                      
         SLA   RF,1                                                             
         D     RE,DPDPNT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         C     RF,=F'9999'                                                      
         BH    CPM2A                                                            
         EDIT  (RF),(5,13(R6)),2                                                
         B     CPM4                                                             
CPM2A    SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         C     RF,=F'9999'                                                      
         BH    CPM2B                                                            
         EDIT  (RF),(5,13(R6)),1                                                
         B     CPM4                                                             
CPM2B    SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         C     RF,=F'99999'                                                     
         BH    CPM4                                                             
         EDIT  (RF),(5,13(R6))                                                  
CPM4     L     RE,DPDDOLE                                                       
         SRDA  RE,32                                                            
CPM5     OC    DPDIMP,DPDIMP                                                    
         BZ    CPM10                                                            
         MH    RF,=H'100'                                                       
         SLA   RF,1                                                             
         D     RE,DPDIMP                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         CH    RF,=H'999'                                                       
         BH    CPM6                                                             
         EDIT  (RF),(4,19(R6)),2                                                
         B     CPM10                                                            
CPM6     SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         CH    RF,=H'999'                                                       
         BH    CPM7                                                             
         EDIT  (RF),(4,19(R6)),1                                                
         B     CPM10                                                            
CPM7     MVC   19(4,R6),=C'HIGH'                                                
         SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         CH    RF,=H'999'                                                       
         BH    CPM10                                                            
         EDIT  (RF),(4,19(R6))                                                  
CPM10    LA    R4,DPDLEN(R4)                                                    
         LA    R6,26(R6)                                                        
         BCT   R5,CPMR1                                                         
         GOTO1 REPORT                                                           
         CLI   MGRPSW,1                                                         
         BNE   RPTDONE                                                          
         MVC   DPAREA(112),DPGAREA                                              
         MVC   HLDRST,=X'FFFE'                                                  
         MVI   MGRPSW,0                                                         
         B     CPMR                                                             
         DROP  R4                                                               
         EJECT                                                                  
RPTDONE  XC    DPAREA(112),DPAREA                                               
         MVC   HOLDREC,SRREC                                                    
         CLI   SRTGT,0             EOF                                          
         BE    EXIT                                                             
         B     SD1                                                              
         LTORG                                                                  
         EJECT                                                                  
*HEADLINES ROUTINES                                                             
         DS    0D                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=CP30RB                                                      
         DROP  RF                                                               
         L     R2,CP30R2                                                        
         LM    RA,RC,CP30RA                                                     
         LA    R3,RTGGRP                                                        
         GOTO1 =V(MYHEADC),DMCB,(RA),(R3),RR=RELO                               
         XIT1                                                                   
         LTORG                                                                  
CP30RA   DS    F                                                                
CP30RB   DS    F                                                                
CP30RC   DS    F                                                                
CP30R2   DS    F                                                                
         EJECT                                                                  
       ++INCLUDE CPDPTRTGEQ                                                     
         EJECT                                                                  
MYHEADC  CSECT                                                                  
         NMOD1 0,MYHEADC                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING CPWORKD,RA,RC                                                    
         USING CP30WK,R2                                                        
         USING RTGGRP,R3                                                        
         LA    RE,DEMGTAB                                                       
MYHEAD1  CLI   0(RE),X'FF'                                                      
         BE    MYHEADX                                                          
         CLC   HLDTRGT,4(RE)                                                    
         BE    MYHEAD2                                                          
         MVC   HALF,5(RE)                                                       
         AH    RE,HALF                                                          
         B     MYHEAD1                                                          
MYHEAD2  L     RF,ADDEMBUF         GET TARGET NAME                              
         ZIC   R9,PLTGT                                                         
         BCTR  R9,0                                                             
         MH    R9,=H'7'                                                         
         AR    R9,RF                                                            
         MVC   H5+40(8),=C'UNIVERSE'                                            
         MVC   H6+40(11),=C'REPORT DEMO'                                        
         MVC   H5+52(7),0(R9)                                                   
         MVC   H5+60(12),=C'TARGETS ONLY'                                       
         CLC   QTARGET,=C'   '                                                  
         BNE   *+10                                                             
         MVC   H5+52(20),=CL20'ALL'                                             
         L     RF,ADDEMBUF         GET DEMO NAME                                
         SR    R9,R9                                                            
         IC    R9,HLDDEMO                                                       
         BCTR  R9,0                                                             
         MH    R9,=H'7'                                                         
         AR    R9,RF                                                            
         MVC   H6+52(7),0(R9)                                                   
         ST    RE,SAVERE                                                        
         MVC   H7+43(12),=C'(WEIGHTED)  '                                       
         CLI   QOPT1,C'Y'                                                       
         BNE   *+10                                                             
         MVC   H7+43(12),=C'(UNWEIGHTED)'                                       
         L     RE,SAVERE                                                        
         LA    RE,27(RE)                                                        
         LA    R4,4                                                             
         LA    RF,DPTNAM                                                        
         LA    R5,H8+26                                                         
         CLI   RCSUBPRG,1                                                       
         BE    *+8                                                              
         LA    R5,H8+9                                                          
MYHEAD3  ZIC   R9,0(RE)                                                         
         CLI   0(RE),0                                                          
         BE    MYHEADX                                                          
         BCTR  R9,0                                                             
         MH    R9,=H'17'                                                        
         AR    R9,RF                                                            
         CLI   RCSUBPRG,2                                                       
         BE    MYHEAD4                                                          
         MVC   0(15,R5),2(R9)                                                   
         LA    R5,21(R5)                                                        
         B     MYHEAD5                                                          
MYHEAD4  MVC   0(15,R5),2(R9)                                                   
         LA    R5,26(R5)                                                        
MYHEAD5  LA    RE,1(RE)                                                         
         BCT   R4,MYHEAD3                                                       
MYHEADX  XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
CP30WK   CSECT                                                                  
SRREC    DS    0CL36               SORT RECORD                                  
SRKEY    DS    0CL12                                                            
SRTGT    DS    CL1                 TARGET                                       
SRDEMO   DS    CL1                 DEMO                                         
SRREN    DS    CL2                 END RANK   (0 IF SINGLE MARKET)              
SRRST    DS    CL2                 START RANK (COMPLIMENT IF GROUP)             
SRDPT    DS    CL1                 DAYPART                                      
SRSLN    DS    CL1                 SPOT LENGTH                                  
SRMKT    DS    CL2                 MARKET NUMBER                                
         DS    CL2                                                              
SRDATA   DS    0CL24                                                            
SRSPOT   DS    CL4                 SPOTS                                        
SRDOL    DS    CL4                 DOLLARS                                      
SRPNT    DS    CL4                 POINTS                                       
SRIMP    DS    CL4                 IMPRESSIONS                                  
SRWGHT   DS    CL4                 MARKET WEIGHT                                
SREQUIV  DS    CL4                 MARKET EQUIVALENCE                           
         SPACE 2                                                                
HOLDREC  DS    0CL36                                                            
HLDKEY   DS    0CL12                                                            
HLDTRGT  DS    CL1                                                              
HLDDEMO  DS    CL1                                                              
HLDREN   DS    CL2                                                              
HLDRST   DS    CL2                                                              
HLDDPT   DS    CL1                                                              
HLDSLN   DS    CL1                                                              
HLDMKT   DS    CL2                                                              
         DS    CL2                                                              
HLDDATA  DS    0CL24                                                            
HLDSPOT  DS    CL4                                                              
HLDDOL   DS    CL4                                                              
HLDPNT   DS    CL4                                                              
HLDIMP   DS    CL4                                                              
HLDWGHT  DS    CL4                                                              
HLDEQUIV DS    CL4                                                              
HLD2REC  DS    0CL36                                                            
HLD2KEY  DS    0CL12                                                            
         DS    CL12                                                             
HLD2DATA DS    CL24                                                             
CPDGRP   DS    CL5                 DEMO GROUPS FOR CURRENT DEMO                 
CPDPT    DS    CL1                 DAYPART FOR CURRENT DEMO                     
DPAREA   DS    28F                 DAYPART TOTAL AREAS                          
DPGAREA  DS    28F                 MARKET GROUP TOTAL AREA                      
OSPMAR   DS    0CL24               ONE SPOT PER MARKET AREA                     
OSPWGHT  DS    F                                                                
OSPSPT   DS    F                                                                
OSPDOL   DS    F                                                                
OSPDOLE  DS    F                                                                
OSPPNT   DS    F                                                                
OSPIMP   DS    F                                                                
SLOT     DS    F                                                                
RELO     DC    A(0)                                                             
VSORTER  DS    F                                                                
SAVERE   DS    F                                                                
EOFSW    DC    X'00'                                                            
MGRPSW   DS    CL1                 MARKET GROUP SWITCH                          
ACTSW    DS    CL1                                                              
HIGROUP  DS    H                                                                
SLTTGT   DS    CL1                                                              
SLTDPT   DS    CL1                                                              
SVTGTA   DS    F                                                                
PLTGT    DS    C                                                                
SVTGT    DS    C                                                                
         EJECT                                                                  
SORT     DC    C'SORT FIELDS=(1,10,BI,A),WORK=1 '                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=36 '                                      
DPD      DSECT                                                                  
DPDNMKT  DS    F                                                                
DPDWGHT  DS    F                                                                
DPDSPT   DS    F                                                                
DPDDOL   DS    F                                                                
DPDDOLE  DS    F                                                                
DPDPNT   DS    F                                                                
DPDIMP   DS    F                                                                
DPDLEN   EQU   28                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CPREPWORKD                                                     
       ++INCLUDE CPREPMODES                                                     
       ++INCLUDE CPGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069CPREP3002 05/01/02'                                      
         END                                                                    
