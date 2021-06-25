*          DATA SET CPREP2602  AT LEVEL 088 AS OF 05/01/02                      
*PHASE CP2602A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'CPREP2602-CPP MARKET FOCUS REPORT'                              
         PRINT NOGEN                                                            
CP2602   CSECT                                                                  
         NMOD1 0,CP2602,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING CPWORKD,RA,RC                                                    
         L     R2,=V(CP26WK)                                                    
         USING CP26WK,R2                                                        
         AR    R2,R5                                                            
         ST    R5,RELO                                                          
         STM   RA,RC,CP26RA                                                     
         ST    R2,CP26R2                                                        
         LA    RE,MYHEAD                                                        
         ST    RE,HEADHOOK                                                      
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   *+8                                                              
         MVI   ACTSW,0                                                          
         MVI   OPTRDSEQ,C'Y'                                                    
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   BK20                                                             
         LR    RE,R2                                                            
         LA    RF,WRKLEN                                                        
         XCEF                                                                   
         XC    HIGROUP,HIGROUP                                                  
         L     R6,=V(SORTER)                                                    
         A     R6,RELO                                                          
         ST    R6,VSORTER                                                       
         CLI   ACTSW,2             SORT ALREADY OPEN                            
         BE    BK10A                                                            
         GOTO1 VSORTER,DMCB,SORT,RECCARD,0                                      
BK10A    MVI   ACTSW,0                                                          
         B     EXIT                                                             
         SPACE 2                                                                
BK20     CLI   MODE,PROCDATA                                                    
         BNE   BK30                                                             
         L     R3,ADDATA                                                        
         USING CPKEYD,R3                                                        
         XC    SRREC,SRREC                                                      
         MVC   SRDEMO,CPKDEMO                                                   
         MVC   SRCLT,CPKCLT                                                     
         MVC   SRTGT,CPKTARGT                                                   
         LA    RE,RTGGRP                                                        
         CLI   QSELECT,C' '                                                     
         BNE   BK20A                                                            
         CLC   SRDEMO,SRTGT                                                     
         BNE   EXIT                                                             
         MVI   SRDEMO,64                                                        
         CLI   QOPT2,C' '                                                       
         BNE   BK20A                                                            
         MVI   QOPT2,C'1'                                                       
BK20A    CLC   CPKDEMO,0(RE)       FIND TARGET GROUP                            
         BE    BK20B                                                            
         LA    RE,RTGGRPLN(RE)                                                  
         CLI   0(RE),X'FF'         BYPASS DEMOS WHICH ARE NOT GROUPED           
         BE    EXIT                                                             
         B     BK20A                                                            
BK20B    MVI   6(RE),1                                                          
         MVC   CPDGRP,1(RE)                                                     
         CLI   QOPT2,C' '                                                       
         BE    *+14                                                             
         MVC   CPDGRP,QOPT2                                                     
         MVI   CPDGRP+1,C' '                                                    
         MVC   CPDPT,CPKDAYPT                                                   
         XC    MSEQ,MSEQ                                                        
         MVC   MSEQ(2),MKTRANK                                                  
         CLI   QMKTSEQ,C'C'                                                     
         BNE   *+10                                                             
         MVC   MSEQ(2),MARKET                                                   
         CLI   QMKTSEQ,C'A'                                                     
         BNE   *+10                                                             
         MVC   MSEQ,MKTNAME                                                     
         MVI   SRSLN,0                                                          
         MVC   SRMKT,CPKMKT                                                     
         MVC   SRMKT2,CPKMKT                                                    
         MVC   SRWGHT+2(2),MKTWT                                                
         LA    R4,PERTABLE                                                      
         USING CPERD,R4                                                         
         L     RF,NPERIODS                                                      
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
         L     RE,SRPNTW                                                        
         L     R8,CPOINTS                                                       
         SRDA  R8,32                                                            
         M     R8,SRWGHT                                                        
         AR    RE,R9                                                            
         ST    RE,SRPNTW                                                        
         L     R8,CPCASH           EQUIVALENCE DOLLARS                          
         SRDA  R8,32                                                            
         M     R8,=F'1000'                                                      
         SLDA  R8,1                                                             
         D     R8,EQVFACT                                                       
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         L     R8,SREQUIV                                                       
         AR    R8,R9                                                            
         ST    R8,SREQUIV                                                       
         A     R4,WIDTHPER                                                      
         BCT   RF,BK20C                                                         
         SPACE 2                                                                
* FIND DAYPARTS FOR DEMO GROUP                                                  
         LA    R5,CPDGRP                                                        
BK20D    CLI   0(R5),C' '                                                       
         BE    EXIT                                                             
         L     RE,=A(DEMGTAB)                                                   
         A     RE,RELO                                                          
BK20D1   CLI   0(RE),X'FF'                                                      
         BE    EXIT                                                             
         CLC   0(1,RE),0(R5)       SAME GROUP                                   
         BE    BK20D2                                                           
         MVC   HALF,5(RE)                                                       
         AH    RE,HALF                                                          
         B     BK20D1                                                           
BK20D2   MVC   SRGRP,4(RE)                                                      
         SPACE 2                                                                
* EXPLODE DAYPARTS                                                              
         LA    R7,31(RE)                                                        
BK20E1   CLI   0(R7),0                                                          
         BE    EXIT                                                             
         CLC   CPDPT,0(R7)                                                      
         BE    PUTSORT                                                          
         LA    R7,2(R7)                                                         
         B     BK20E1                                                           
         SPACE 2                                                                
* GENERATE SORT RECORDS                                                         
PUTSORT  XC    SRREN,SRREN         CREATE MARKET RECORDS                        
         XC    SRRST,SRRST                                                      
         MVC   SRMSEQ,MSEQ                                                      
         MVC   SRMKT,CPKMKT                                                     
         MVC   SRDPT,1(R7)                                                      
         BAS   R9,PSOK             PUT DETAIL RECORD                            
         XC    SRMKT,SRMKT                                                      
         XC    SRMSEQ,SRMSEQ                                                    
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
         CLC   SRRST,=X'FFFE'      PUT OVERALL GROUP                            
         BE    PUTSRTA                                                          
         MVC   SRRST,=X'FFFE'                                                   
PUTSORTA BAS   R9,PSOK                                                          
PUTSRTA  MVC   HALF,SRREN          SET UP DECADE GROUPS                         
         LH    RE,HALF                                                          
         CH    RE,=H'199'                                                       
         BH    PUTSORT1                                                         
         CH    RE,=H'49'                                                        
         BH    *+12                                                             
         AH    RE,=H'10'                                                        
         B     PUTSRTA1                                                         
         CH    RE,=H'99'                                                        
         BH    *+12                                                             
         LH    RE,=H'100'                                                       
         B     PUTSRTA1                                                         
         LH    RE,=H'200'                                                       
PUTSRTA1 DS    0H                                                               
         STH   RE,HALF                                                          
         MVC   SRREN,HALF                                                       
         B     PUTSORTA                                                         
PUTSORT1 DS    0H                                                               
         LA    R7,2(R7)                                                         
         B     BK20E1                                                           
         SPACE 2                                                                
PSOK     DS    0H'0'                                                            
PSOK1    ST    R9,DUB                   C                                       
         BAS   R9,PSPUT                                                         
         MVC   CURRCLT,SRCLT                                                    
         MVC   SRCLT,=X'FFFFFF'                                                 
         BAS   R9,PSPUT                                                         
         MVC   CURRTGT,SRTGT                                                    
         CLI   QSELECT,C' '        TARGETS ONLY                                 
         BE    PSOK2                YES - BYPASS OVERALL TOTALS                 
         MVI   SRTGT,X'FF'                                                      
         BAS   R9,PSPUT                                                         
PSOK2    MVC   SRTGT,CURRTGT                                                    
         MVC   SRCLT,CURRCLT                                                    
         L     R9,DUB                                                           
         BR    R9                                                               
         SPACE 2                                                                
PSPUT    GOTO1 VSORTER,DMCB,=C'PUT',SRREC                                       
         MVI   ACTSW,1                                                          
         BR    R9                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
BK30     CLI   MODE,MKTFRST                                                     
         BNE   BK40                                                             
         XC    MSEQ,MSEQ                                                        
         MVC   MSEQ(2),MKTRANK                                                  
         CLI   QMKTSEQ,C'C'                                                     
         BNE   *+10                                                             
         MVC   MSEQ(2),MARKET                                                   
         CLI   QMKTSEQ,C'A'                                                     
         BNE   *+10                                                             
         MVC   MSEQ,MKTNAME                                                     
         B     EXIT                                                             
         MVC   P(L'MKTNAME),MKTNAME                                             
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
BK40     CLI   MODE,REQLAST                                                     
         BNE   BK50                                                             
         XC    HLD2REC,HLD2REC                                                  
         XC    HOLDREC,HOLDREC                                                  
         XC    DPAREA(128),DPAREA                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   EOFSW,0                                                          
         CLI   ACTSW,0             ACTIVITY                                     
         BNE   PR1                                                              
         MVI   ACTSW,2                                                          
         B     EXIT                                                             
         EJECT                                                                  
BK50     CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         CLI   ACTSW,2             SORT OPENED BUT NOT CLOSED                   
         BNE   EXIT                                                             
         GOTO1 VSORTER,DMCB,=C'END',SRREC                                       
         B     EXIT                                                             
         EJECT                                                                  
PR1      MVC   SRREC,HLD2REC                                                    
         CLI   EOFSW,1                                                          
         BE    SD1                                                              
         GOTO1 VSORTER,DMCB,=C'GET',SRREC                                       
         L     RE,4(R1)                                                         
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   PR3                                                              
         GOTO1 VSORTER,DMCB,=C'END',SRREC                                       
         MVI   EOFSW,1             SET EOF AND RELEASE PREV. RECORD             
         XC    HLD2REC,HLD2REC                                                  
         MVI   ACTSW,0                                                          
         B     PRX                                                              
         SPACE 2                                                                
PR3      MVC   HLD2REC,0(RE)       ACCUMULATE DUPS                              
         OC    SRREC,SRREC                                                      
         BZ    PR1                                                              
         CLC   HLD2KEY,SRREC                                                    
         BNE   PRX                                                              
         CLC   SRMKT2,HLD2MKT2                                                  
         BE    PR3A                                                             
         L     RE,HLD2WGHT                                                      
         A     RE,SRWGHT                                                        
         ST    RE,HLD2WGHT                                                      
PR3A     LA    RF,HLD2DATA                                                      
         LA    RE,SRSPOT                                                        
         LA    R9,6                                                             
         LA    R1,0                                                             
PR4      L     R8,0(R1,RE)                                                      
         A     R8,0(R1,RF)                                                      
         ST    R8,0(R1,RF)                                                      
         LA    R1,4(R1)                                                         
         BCT   R9,PR4                                                           
         B     PR1                                                              
PRX      DS    0H                                                               
         SPACE 2                                                                
PRX1     CLC   SRREN,HIGROUP       BYPASS HIGH GROUPS                           
         BH    PR1                                                              
         EJECT                                                                  
* SUM RECORDS INTO TABLE IF DETAIL ITEM                                         
SD1      DS    0H                                                               
         CLI   RCSUBPRG,1                                                       
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         MVC   SLTTGT,SRGRP                                                     
         MVC   SLTDPT,SRDPT                                                     
         OC    HOLDREC,HOLDREC                                                  
         BZ    SD2                                                              
         CLC   SRMKT,HLDMKT        MARKET EQUAL                                 
         BNE   CPMDR                NO - DO REPORT                              
         CLC   SRCLT,HLDCLT                                                     
         BNE   CPMDR                                                            
         CLC   SRTGT,HLDTRGT                                                    
         BNE   CPMDR                                                            
SD2      BAS   R9,FSLOT            FIND DAYPART SLOT                            
         L     RE,SLOT                                                          
         BCTR  RE,0                                                             
         MH    RE,=H'32'                                                        
         LA    R4,DPAREA                                                        
         USING DPD,R4                                                           
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
         L     RE,DPDPNTW                                                       
         A     RE,SRPNTW                                                        
         ST    RE,DPDPNTW                                                       
         MVC   HOLDREC,SRREC                                                    
         B     PR1                                                              
         EJECT                                                                  
         DROP  R4                                                               
         SPACE 2                                                                
* FIND SLOT FOR PREV DAYPART                                                    
FSLOT    L     RE,=A(DEMGTAB)                                                   
         A     RE,RELO                                                          
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
CPMDR    DS    0H                                                               
         CLC   HLDTRGT,PREVTGT                                                  
         BE    CPMDR2A                                                          
         CLI   FORCEHED,C'Y'                                                    
         BNE   *+10                                                             
         XC    TGTNAM,TGTNAM                                                    
         MVI   P,0                                                              
         GOTO1 REPORT                                                           
         MVI   CCOUNT,0                                                         
         MVC   PREVTGT,HLDTRGT                                                  
         CLI   HLDTRGT,X'FF'        ALL TARGETS                                 
         BNE   CPMDR1                                                           
         MVC   TGTNAM,=C'ALL    '                                               
         MVC   P(7),TGTNAM                                                      
         MVI   CCOUNT,X'05'        FORCE TOTAL LINE                             
         B     CPMDR2A                                                          
CPMDR1   DS    0H                                                               
         L     RF,ADDEMBUF         GET TARGET NAME                              
         ZIC   R9,HLDTRGT                                                       
         BCTR  R9,0                                                             
         MH    R9,=H'7'                                                         
         AR    R9,RF                                                            
         MVC   P(7),0(R9)                                                       
         MVC   TGTNAM,0(R9)                                                     
         SPACE 2                                                                
CPMDR2A  L     RE,ADCLTBUF                                                      
         USING CLTTABD,RE                                                       
         CLI   HLDCLT,X'FF'                                                     
         BNE   CPMDR2B                                                          
         CLI   CCOUNT,1                                                         
         BE    RPTDONE                                                          
         MVC   P+12(14),=C' AGENCY TOTALS'                                      
         CLC   TGTNAM(3),=C'ALL'                                                
         BE    *+10                                                             
         MVC   P+12(7),TGTNAM                                                   
         B     CPMDR3                                                           
CPMDR2B  CLC   HLDCLT,CLTABCD                                                   
         BE    CPMDR2C                                                          
         L     RF,NCLTS                                                         
         MH    RF,WIDTHCLT                                                      
         A     RF,ADCLTBUF                                                      
         AH    RE,WIDTHCLT                                                      
         CR    RE,RF                                                            
         BNH   CPMDR2B                                                          
         DC    H'0'                CLIENT NOT IN TABLE                          
CPMDR2C  MVC   P+8(20),CLTTABNM                                                 
         ZIC   RF,CCOUNT                                                        
         LA    RF,1(RF)                                                         
         STC   RF,CCOUNT                                                        
         DROP  RE                                                               
         SPACE 2                                                                
         SPACE 2                                                                
CPMDR3   LA    R4,DPAREA                                                        
         USING DPD,R4                                                           
CPMDR3A  LA    R5,4                                                             
         LA    R6,P+30                                                          
CPMDR4   OC    DPDSPT,DPDSPT                                                    
         BZ    CPMDR11                                                          
         OC    DPDWGHT,DPDWGHT                                                  
         BNZ   *+10                                                             
         MVC   DPDWGHT,=F'1'                                                    
         L     RE,DPDPNTW          UNWGTH POINTS                                
         OC    DPDPNTW,DPDPNTW                                                  
         BZ    CPMDR4A                                                          
         SRDA  RE,32                                                            
         C     RF,=F'2000000000'                                                
         BH    CPMDR41                                                          
         SLDA  RE,1                                                             
         D     RE,DPDWGHT                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,DPDPNTW                                                       
CPMDR40  CLI   QOPT1,C'Y'                                                       
         BNE   CPMDR4A                                                          
         MVC   DPDPNTW,DPDPNT                                                   
         B     CPMDR4A                                                          
CPMDR41  DS    0H                                                               
         D     RE,DPDWGHT                                                       
         ST    RF,DPDPNTW                                                       
         B     CPMDR40                                                          
CPMDR4A  DS    0H                                                               
         L     RF,DPDPNTW                                                       
         SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         C     RF,=F'99999'                                                     
         BH    CPMDR4B                                                          
         EDIT  (RF),(5,0(R6))                                                   
         B     CPMDR4C                                                          
CPMDR4B  EDIT  (RF),(8,129(R6))                                                 
CPMDR4C  DS    0H                                                               
         L     RE,DPDPNT                                                        
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,DPDSPT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         SLA   RF,1                SCALE FOR NO DECIMALS                        
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(2,6(R6))                                                   
         L     RE,DPDDOLE                                                       
         SRDA  RE,32                                                            
         OC    DPDPNTW,DPDPNTW                                                  
         BZ    CPMDR8                                                           
         M     RE,=F'1000'                                                      
         SLDA  RE,1                                                             
         D     RE,DPDPNTW                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         CH    RF,=H'9999'                                                      
         BH    CPMDR5                                                           
         EDIT  (RF),(5,9(R6)),2                                                 
         B     CPMDR7                                                           
CPMDR5   SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         CH    RF,=H'9999'                                                      
         BH    CPMDR6                                                           
         EDIT  (RF),(5,9(R6)),1                                                 
         B     CPMDR7                                                           
CPMDR6   SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(5,9(R6))                                                   
         SPACE 2                                                                
CPMDR7   L     RE,DPDDOLE                                                       
         SRDA  RE,32                                                            
CPMDR8   OC    DPDIMP,DPDIMP                                                    
         BZ    CPMDR11                                                          
         M     RE,=F'100'                                                       
         SLDA  RE,1                                                             
         D     RE,DPDIMP                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         CH    RF,=H'999'                                                       
         BH    CPMDR9                                                           
         EDIT  (RF),(4,15(R6)),2                                                
         B     CPMDR11                                                          
CPMDR9   SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         CH    RF,=H'999'                                                       
         BH    CPMDR10                                                          
         EDIT  (RF),(4,15(R6)),1                                                
         B     CPMDR11                                                          
CPMDR10  SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         MVC   15(4,R6),=C'HIGH'                                                
         CH    RF,=H'999'                                                       
         BH    CPMDR11                                                          
         EDIT  (RF),(4,15(R6))                                                  
CPMDR11  LA    R4,DPDLEN(R4)                                                    
         LA    R6,20(R6)                                                        
         BCT   R5,CPMDR4                                                        
         GOTO1 REPORT                                                           
         B     RPTDONE                                                          
         DROP  R4                                                               
         EJECT                                                                  
         EJECT                                                                  
RPTDONE  XC    DPAREA(128),DPAREA                                               
         CLC   SRRST,HLDRST                                                     
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLC   SRMKT,HLDMKT                                                     
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   HOLDREC,SRREC                                                    
         CLI   SRTGT,0             EOF                                          
         BE    EXIT                                                             
         B     SD1                                                              
         LTORG                                                                  
         EJECT                                                                  
*HEADLINES ROUTINES                                                             
         DS    0D                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=CP26RB                                                      
         DROP  RF                                                               
         L     R2,CP26R2                                                        
         LM    RA,RC,CP26RA                                                     
         L     RE,=A(DEMGTAB)                                                   
         A     RE,RELO                                                          
MYHEAD1  CLI   0(RE),X'FF'                                                      
         BE    MYHEADX                                                          
         CLC   HLDGRP,4(RE)                                                     
         BE    MYHEAD2                                                          
         MVC   HALF,5(RE)                                                       
         AH    RE,HALF                                                          
         B     MYHEAD1                                                          
MYHEAD2  DS    0H                                                               
         MVC   H7+45(12),=C' (WEIGHTED) '                                       
         CLI   QOPT1,C'Y'                                                       
         BNE   *+10                                                             
         MVC   H7+45(12),=C'(UNWEIGHTED)'                                       
         MVC   P(7),TGTNAM                                                      
         MVC   H5+40(16),=C'TARGET UNIVERSE '                                   
         MVC   H5+57(11),=C'ALL TARGETS'                                        
         CLC   QTARGET,=C'   '                                                  
         BE    MYHEAD2A                                                         
         PACK  DUB,QTARGET                                                      
         CVB   R9,DUB                                                           
         L     RF,ADDEMBUF                                                      
         BCTR  R9,0                                                             
         MH    R9,=H'7'                                                         
         AR    R9,RF                                                            
         MVC   H5+57(7),0(R9)                                                   
         MVC   H5+64(12),=C'TARGETS ONLY'                                       
MYHEAD2A DS    0H                                                               
         MVC   H6+40(11),=C'REPORT DEMO'                                        
         CLI   QSELECT,C' '                                                     
         BNE   MYHEAD2B                                                         
         MVC   H6+57(7),=C'TARGETS'                                             
         B     MYHEAD2C                                                         
MYHEAD2B DS    0H                                                               
         L     RF,ADDEMBUF         GET DEMO NAME                                
         SR    R9,R9                                                            
         IC    R9,HLDDEMO                                                       
         BCTR  R9,0                                                             
         MH    R9,=H'7'                                                         
         AR    R9,RF                                                            
         MVC   H6+57(7),0(R9)                                                   
MYHEAD2C LA    RE,27(RE)                                                        
         LA    R4,4                                                             
         LA    RF,DPTNAM                                                        
         LA    R5,H9+33                                                         
         CLI   RCSUBPRG,1                                                       
         BE    *+8                                                              
         LA    R5,H8+9                                                          
MYHEAD3  ZIC   R9,0(RE)                                                         
         CLI   0(RE),0                                                          
         BE    MYHEAD5                                                          
         BCTR  R9,0                                                             
         MH    R9,=H'17'                                                        
         AR    R9,RF                                                            
         CLI   RCSUBPRG,2                                                       
         BE    MYHEAD4                                                          
         MVC   0(15,R5),2(R9)                                                   
         LA    R5,20(R5)                                                        
         B     MYHEAD5                                                          
MYHEAD4  MVC   0(15,R5),2(R9)                                                   
         LA    R5,24(R5)                                                        
MYHEAD5  LA    RE,1(RE)                                                         
         BCT   R4,MYHEAD3                                                       
         OC    HLDREN,HLDREN                                                    
         BZ    MYHEAD6                                                          
         MVC   H4(12),=C'MARKET GROUP'                                          
         MVC   HALF,HLDRST                                                      
         XC    HALF,=X'FFFF'                                                    
         EDIT  HALF,(3,H4+13)                                                   
         MVI   H4+16,C'-'                                                       
         EDIT  HLDREN,(3,H4+17),,ALIGN=LEFT                                     
         B     MYHEADX                                                          
         SPACE 2                                                                
MYHEAD6  L     RE,ADMKTTAB                                                      
MYHEAD61 CLC   HLDMKT,0(RE)                                                     
         BE    MYHEAD62                                                         
         CLI   0(RE),X'FF'                                                      
         BE    MYHEADX                                                          
         LA    RE,34(RE)                                                        
         B     MYHEAD61                                                         
         SPACE 2                                                                
MYHEAD62 MVC   H4(6),=C'MARKET'                                                 
         MVC   H4+7(24),6(RE)                                                   
         MVC   H5+7(5),=C'CODE='                                                
         MVC   HALF,0(RE)                                                       
         EDIT  HALF,(3,H5+12),ALIGN=LEFT                                        
         MVC   H5+18(5),=C'RANK='                                               
         MVC   HALF,2(RE)                                                       
         CLI   USERPROF,C'S'                                                    
         BNE   *+10                                                             
         MVC   HALF,4(RE)                                                       
         EDIT  HALF,(3,H5+23),,ALIGN=LEFT                                       
MYHEADX  XIT1                                                                   
         LTORG                                                                  
CP26RA   DS    F                                                                
CP26RB   DS    F                                                                
CP26RC   DS    F                                                                
CP26R2   DS    F                                                                
         EJECT                                                                  
       ++INCLUDE CPDPTRTGEQ                                                     
         EJECT                                                                  
CP26WK   CSECT                                                                  
SRREC    DS    0CL58               SORT RECORD                                  
SRKEY    DS    0CL28                                                            
SRGRP    DS    CL1                 DAYPART GROUP                                
SRDEMO   DS    CL1                 DEMO                                         
SRREN    DS    CL2                 END RANK   (0 IF SINGLE MARKET)              
SRRST    DS    CL2                 START RANK (COMPLIMENT IF GROUP)             
*                                   SEQUENCE NUMBER IF NO GROUP                 
SRMSEQ   DS    CL13                MARKET SEQUENCE                              
SRMKT    DS    CL2                 MARKET NUMBER                                
SRTGT    DS    CL1                 TARGET                                       
SRCLT    DS    CL3                 CLIENT CODE                                  
SRDPT    DS    CL1                 DAYPART                                      
SRSLN    DS    CL1                 SPOT LENGTH                                  
         DS    CL1                                                              
SRDATA   DS    0CL28                                                            
SRSPOT   DS    CL4                 SPOTS                                        
SRDOL    DS    CL4                 DOLLARS                                      
SRPNT    DS    CL4                 POINTS                                       
SRIMP    DS    CL4                 IMPRESSIONS                                  
SRPNTW   DS    CL4                                                              
SREQUIV  DS    CL4                 EQUIVALENCED DOLLARS                         
SRWGHT   DS    CL4                 MARKET WEIGHT                                
SRMKT2   DS    CL2                                                              
         SPACE 2                                                                
HOLDREC  DS    0CL58                                                            
HLDKEY   DS    0CL28                                                            
HLDGRP   DS    CL1                                                              
HLDDEMO  DS    CL1                                                              
HLDREN   DS    CL2                                                              
HLDRST   DS    CL2                                                              
HLDMSEQ  DS    CL13                                                             
HLDMKT   DS    CL2                                                              
HLDTRGT  DS    CL1                                                              
HLDCLT   DS    CL3                                                              
HLDDPT   DS    CL1                                                              
HLDSLN   DS    CL1                                                              
         DS    CL1                                                              
HLDDATA  DS    0CL28                                                            
HLDSPOT  DS    CL4                                                              
HLDDOL   DS    CL4                                                              
HLDPNT   DS    CL4                                                              
HLDIMP   DS    CL4                                                              
         DS    CL4                                                              
HLDEQUIV DS    CL4                                                              
HLDWGHT  DS    CL4                                                              
         DS    CL2                                                              
HLD2REC  DS    0CL58                                                            
HLD2KEY  DS    0CL28                                                            
         DS    CL28                                                             
HLD2DATA DS    0CL28                                                            
         DS    CL20                                                             
         DS    CL4                                                              
HLD2WGHT DS    CL4                                                              
HLD2MKT2 DS    CL2                                                              
CPDGRP   DS    CL5                 DEMO GROUPS FOR CURRENT DEMO                 
CPDPT    DS    CL1                 DAYPART FOR CURRENT DEMO                     
DPAREA   DS    32F                 DAYPART TOTAL AREAS                          
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
CCOUNT   DC    X'00'                                                            
SLTTGT   DS    CL1                                                              
SLTDPT   DS    CL1                                                              
MCOUNT   DC    F'0'                                                             
HIGROUP  DS    H                                                                
CURRCLT  DS    CL3                                                              
CURRTGT  DS    C                                                                
PREVTGT  DC    X'00'                                                            
TGTNAM   DS    CL7                                                              
MSEQ     DS    CL13                                                             
WRKLEN   EQU   *-SRREC                                                          
ACTSW    DS    CL1                 ACTIVITY SWITCH                              
         EJECT                                                                  
SORT     DC    C'SORT FIELDS=(1,28,BI,A,57,2,BI,A),WORK=1 '                     
RECCARD  DC    C'RECORD TYPE=F,LENGTH=58 '                                      
DPD      DSECT                                                                  
DPDNMKT  DS    F                                                                
DPDWGHT  DS    F                                                                
DPDSPT   DS    F                                                                
DPDDOL   DS    F                                                                
DPDDOLE  DS    F                                                                
DPDPNT   DS    F                                                                
DPDIMP   DS    F                                                                
DPDPNTW  DS    F                                                                
DPDLEN   EQU   32                                                               
CLTTABD  DSECT                                                                  
         DS    CL4                                                              
CLTABCD  DS    CL3                                                              
CLTTABNM DS    CL20                                                             
CLTABOF  DS    CL1                                                              
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CPREPWORKD                                                     
       ++INCLUDE CPREPMODES                                                     
       ++INCLUDE CPGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'088CPREP2602 05/01/02'                                      
         END                                                                    
