*          DATA SET CPREP7402  AT LEVEL 049 AS OF 05/01/02                      
*PHASE CP7402A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'CPREP2802-CPP MARKET SUMMARY'                                   
         PRINT NOGEN                                                            
CP2802   CSECT                                                                  
         NMOD1 0,CP2802,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING CPWORKD,RA,RC                                                    
         L     R2,=V(CP28WK)                                                    
         USING CP28WK,R2                                                        
         AR    R2,R5                                                            
         ST    R5,RELO                                                          
         STM   RA,RC,CP28RA                                                     
         ST    R2,CP28R2                                                        
         LA    RE,MYHEAD                                                        
         ST    RE,HEADHOOK                                                      
         CLI   MODE,RUNFRST                                                     
         BNE   *+8                                                              
         MVI   ACTSW,1                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   BK20                                                             
         CLC   QTARGET,=C'   '                                                  
         BE    BK08                                                             
         PACK  DUB,QTARGET                                                      
         CVB   RF,DUB                                                           
         STC   RF,REQTGT                                                        
BK08     DS    0H                                                               
         CLI   ACTSW,2             SORT ALREADY OPEN                            
         BE    BK09                                                             
         L     R6,=V(SORTER)                                                    
         A     R6,RELO                                                          
         ST    R6,VSORTER                                                       
         GOTO1 (R6),DMCB,SORT,RECCARD,0                                         
BK09     MVI   ACTSW,0                                                          
         B     EXIT                                                             
         SPACE 2                                                                
BK20     CLI   MODE,PROCDATA                                                    
         BNE   BK30                                                             
         L     R3,ADDATA                                                        
         USING CPKEYD,R3                                                        
         XC    SRREC,SRREC                                                      
         CLI   CPKDEMO,1           HH ONLY                                      
         BNE   EXIT                                                             
         MVC   SRDEMO,CPKDEMO                                                   
         MVI   SRDPT,0             DPT=0                                        
         MVC   SRSLN,CPKSPTLN                                                   
         MVC   SRMKT,CPKMKT                                                     
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
         L     R8,CPOINTS                                                       
         SRDA  R8,32                                                            
         M     R8,SRWGHT                                                        
         A     R9,SRPNTW                                                        
         ST    R9,SRPNTW                                                        
         L     R8,CPCASH                                                        
         SRDA  R8,32                                                            
         M     R8,=F'1000'                                                      
         SLDA  R8,1                                                             
         D     R8,EQVFACT                                                       
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         A     R9,SREQUIV                                                       
         ST    R9,SREQUIV                                                       
         A     R4,WIDTHPER                                                      
         MVI   ACTSW,1                                                          
         BCT   RF,BK20C                                                         
         SPACE 2                                                                
* FIND DAYPARTS FOR DEMO GROUP                                                  
         LA    R5,CPDGRP                                                        
         CLI   QOPT2,C' '                                                       
         BE    BK20D                                                            
         MVC   CPDGRP,=C'     '                                                 
         MVC   CPDGRP(1),QOPT2                                                  
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
         BE    EXIT                                                             
         CLC   CPDPT,0(R7)                                                      
         BE    PUTSORT                                                          
         LA    R7,2(R7)                                                         
         B     BK20E1                                                           
         SPACE 2                                                                
* GENERATE SORT RECORDS                                                         
PUTSORT  XC    SRREN,SRREN         CREATE MARKET RECORDS                        
         MVC   SRMSEQ,MSEQ                                                      
         MVC   SRRST,MKTRANK                                                    
         MVC   SRDPT,1(R7)                                                      
         BAS   R9,PSOK             PUT DETAIL RECORD                            
         MVC   SRMSEQ,=X'FFFF'                                                  
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
PUTSRTA1 DS    0H                                                               
         LA    R7,2(R7)                                                         
         B     BK20E1                                                           
PUTSORT1 DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
PSOK     CLC   QUESTOR(6),=C'ZTRACE'                                            
         BNE   PSOK1                                                            
         GOTO1 HEXOUT,DMCB,SRREC,P,36,0,0                                       
         GOTO1 REPORT                                                           
PSOK1    GOTO1 VSORTER,DMCB,=C'PUT',SRREC                                       
         BR    R9                                                               
         SPACE 2                                                                
BK30     CLI   MODE,MKTFRST                                                     
         BNE   BK40                                                             
         LH    RE,MSEQ                                                          
         LA    RE,1(RE)                                                         
         STH   RE,MSEQ                                                          
         B     EXIT                                                             
EXIT     XMOD1 1                                                                
         EJECT                                                                  
BK40     CLI   MODE,REQLAST                                                     
         BNE   BK50                                                             
         XC    HLD2REC,HLD2REC                                                  
         XC    HOLDREC,HOLDREC                                                  
         XC    DPAREA(128),DPAREA                                               
         XC    DPGAREA(128),DPGAREA                                             
         MVI   EOFSW,0                                                          
         CLI   ACTSW,1             ACTIVITY                                     
         BE    PR1                                                              
         MVI   ACTSW,2              NO - SET TO BYPASS OPEN                     
         B     EXIT                                                             
         SPACE 2                                                                
BK50     CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         CLI   ACTSW,2                                                          
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
         GOTO1 VSORTER,DMCB,=C'END'                                             
         MVI   EOFSW,1             SET EOF AND RELEASE PREV. RECORD             
         XC    HLD2REC,HLD2REC                                                  
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
PR3A     DS    0H                                                               
         LA    RF,HLD2DATA                                                      
         LA    RE,SRSPOT                                                        
         LA    R9,5                                                             
         LA    R1,0                                                             
PR4      L     R8,0(R1,RE)                                                      
         A     R8,0(R1,RF)                                                      
         ST    R8,0(R1,RF)                                                      
         LA    R1,4(R1)                                                         
         BCT   R9,PR4                                                           
         LA    R1,24                                                            
         L     R8,0(R1,RE)                                                      
         A     R8,0(R1,RF)                                                      
         ST    R8,0(R1,RF)                                                      
         B     PR1                                                              
PRX      CLC   SRREN,HIGROUP       BYPASS HIGH GROUPS                           
         BH    PR1                                                              
         CLC   QUESTOR(6),=C'ZTRACE'                                            
         BNE   SD1                                                              
         GOTO1 HEXOUT,DMCB,SRREC,P,36                                           
         GOTO1 REPORT                                                           
         EJECT                                                                  
* SUM RECORDS INTO TABLE IF DETAIL ITEM                                         
SD1      DS    0H                                                               
         CLI   RCSUBPRG,1                                                       
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         MVC   SLTTGT,SRTGT                                                     
         MVC   SLTDPT,SRDPT                                                     
         OC    HOLDREC,HOLDREC                                                  
         BZ    SD2                                                              
         CLC   SRRST,HLDRST        MARKET EQUAL                                 
         BNE   CPMDR                NO - DO REPORT                              
SD2      BAS   R9,FSLOT            FIND DAYPART SLOT                            
         L     RE,SLOT                                                          
         BCTR  RE,0                                                             
         MH    RE,=H'32'                                                        
         LA    R4,DPAREA                                                        
         USING DPD,R4                                                           
         AR    R4,RE                                                            
         MVI   MGRPSW,1                                                         
         CLI   SRMSEQ,X'FF'                                                     
         BE    *+8                                                              
         MVI   MGRPSW,0                                                         
SD3      DS    0H                                                               
         L     RE,DPDWGHT                                                       
         A     RE,SRWGHT                                                        
         ST    RE,DPDWGHT                                                       
         L     RE,DPDPNTW                                                       
         A     RE,SRPNTW                                                        
         ST    RE,DPDPNTW                                                       
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
         CLI   MGRPSW,1                                                         
         BNE   PR1                                                              
         LA    R4,128(R4)                                                       
         MVI   MGRPSW,0                                                         
         B     SD3                                                              
         B     PR1                                                              
         EJECT                                                                  
         DROP  R4                                                               
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
         MVI   MGRPSW,0                                                         
         MVC   P(07),=C'UNKNOWN'                                                
         CLI   HLDMSEQ,X'FF'                                                    
         BNE   CPMDR1                                                           
         MVI   MGRPSW,1                                                         
         CLC   HLDREN,=H'10'                                                    
         BNE   CPMDRA                                                           
         MVC   P(7),SPACES                                                      
         GOTO1 REPORT                                                           
CPMDRA   DS    0H                                                               
         MVC   P(7),SPACES                                                      
         XC    HLDRST,=X'FFFF'                                                  
         MVC   P+4(7),=C'MARKETS'                                               
         MVC   HALF,HLDRST                                                      
         EDIT  HALF,(3,P+12)                                                    
         MVC   HALF,HLDREN                                                      
         EDIT  HALF,(3,P+16),,ALIGN=LEFT                                        
         MVI   P+15,C'-'                                                        
         CLC   HLDREN,=H'10'                                                    
         BE    CPMDR3                                                           
         B     CPMDR3                                                           
CPMDR1   CLC   HLDMKT,0(RE)                                                     
         BE    CPMDR2                                                           
         CLI   0(RE),X'FF'                                                      
         BE    CPMDR3                                                           
         LA    RE,34(RE)                                                        
         B     CPMDR1                                                           
CPMDR2   MVC   P+4(24),6(RE)                                                    
         MVI   P+3,C' '                                                         
         EDIT  HLDRST,(3,P)                                                     
         SPACE 2                                                                
CPMDR3   LA    R4,DPAREA                                                        
         USING DPD,R4                                                           
         LA    R5,4                                                             
         LA    R6,P+30                                                          
CPMDR4   OC    DPDSPT,DPDSPT                                                    
         BZ    CPMDR11                                                          
         L     RE,DPDPNTW          UNWEIGHT POINTS                              
         OC    DPDPNTW,DPDPNTW                                                  
         BZ    CPMDR4A                                                          
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         D     RE,DPDWGHT                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,DPDPNTW                                                       
         CLI   QOPT1,C'Y'                                                       
         BNE   CPMDR4A                                                          
         MVC   DPDPNTW,DPDPNT                                                   
CPMDR4A  L     RE,DPDPNTW                                                       
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(5,0(R6))                                                   
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
         CLI   MGRPSW,1                                                         
         BNE   CPMDR12                                                          
         MVC   DPAREA(128),DPGAREA                                              
         MVC   HLDRST,=X'FFFE'                                                  
         MVI   MGRPSW,0                                                         
         CLC   HLDREN,=H'10'                                                    
         BE    CPMDR12                                                          
         B     CPMDRA                                                           
CPMDR12  DS    0H                                                               
         CLC   SRTGT(3),HLDTRGT                                                 
         BE    RPTDONE                                                          
         MVI   FORCEHED,C'Y'                                                    
         XC    DPGAREA(128),DPGAREA                                             
         B     RPTDONE                                                          
         DROP  R4                                                               
         EJECT                                                                  
         EJECT                                                                  
RPTDONE  XC    DPAREA(128),DPAREA                                               
         MVC   HOLDREC,SRREC                                                    
         CLI   SRTGT,0             EOF                                          
         BE    EXIT                                                             
         B     SD1                                                              
         LTORG                                                                  
         EJECT                                                                  
*HEADLINES ROUTINES                                                             
         DS    0D                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=CP28RB                                                      
         DROP  RF                                                               
         L     R2,CP28R2                                                        
         LM    RA,RC,CP28RA                                                     
         MVC   H7+45(12),=C'(WEIGHTED)  '                                       
         CLI   QOPT1,C'Y'                                                       
         BNE   *+10                                                             
         MVC   H7+45(12),=C'(UNWEIGHTED)'                                       
         LA    RE,DEMGTAB                                                       
MYHEAD1  CLI   0(RE),X'FF'                                                      
         BE    MYHEADX                                                          
         CLC   HLDTRGT,4(RE)                                                    
         BE    MYHEAD2                                                          
         MVC   HALF,5(RE)                                                       
         AH    RE,HALF                                                          
         B     MYHEAD1                                                          
MYHEAD2  MVC   H5+40(8),=C'UNIVERSE'                                            
         L     RF,ADDEMBUF         GET DEMO NAME                                
         SR    R9,R9                                                            
         IC    R9,HLDTGT1                                                       
         BCTR  R9,0                                                             
         MH    R9,=H'7'                                                         
         AR    R9,RF                                                            
         MVC   H5+56(7),0(R9)                                                   
         MVC   H5+64(12),=C'TARGETS ONLY'                                       
         CLC   QTARGET,=C'   '                                                  
         BNE   *+10                                                             
         MVC   H5+56(20),=C'ALL                 '                               
         MVC   H6+40(11),=C'REPORT DEMO'                                        
         L     RF,ADDEMBUF                                                      
         SR    R9,R9                                                            
         IC    R9,HLDDEMO                                                       
         BCTR  R9,0                                                             
         MH    R9,=H'7'                                                         
         AR    R9,RF                                                            
         MVC   H6+56(7),0(R9)                                                   
         LA    RE,27(RE)                                                        
         LA    R4,4                                                             
         LA    RF,DPTNAM                                                        
         LA    R5,H9+33                                                         
MYHEAD3  ZIC   R9,0(RE)                                                         
         CLI   0(RE),0                                                          
         BE    MYHEAD5                                                          
         BCTR  R9,0                                                             
         MH    R9,=H'17'                                                        
         AR    R9,RF                                                            
         MVC   0(15,R5),2(R9)                                                   
         LA    R5,20(R5)                                                        
MYHEAD5  LA    RE,1(RE)                                                         
         BCT   R4,MYHEAD3                                                       
MYHEADX  XIT1                                                                   
         LTORG                                                                  
CP28RA   DS    F                                                                
CP28RB   DS    F                                                                
CP28RC   DS    F                                                                
CP28R2   DS    F                                                                
         EJECT                                                                  
       ++INCLUDE CPDPTRTGEQ                                                     
         EJECT                                                                  
CP28WK   CSECT                                                                  
SRREC    DS    0CL46               SORT RECORD                                  
SRKEY    DS    0CL16                                                            
SRTGT    DS    CL1                 TARGET                                       
SRTGT1   DS    CL1                 ACTUAL TARGET                                
SRDEMO   DS    CL1                 DEMO                                         
SRMSEQ   DS    CL2                 MARKET SEQUENCE                              
SRREN    DS    CL2                 END RANK   (0 IF SINGLE MARKET)              
SRRST    DS    CL2                 START RANK (COMPLIMENT IF GROUP)             
SRMKT    DS    CL2                 MARKET NUMBER                                
SRDPT    DS    CL1                 DAYPART                                      
SRSLN    DS    CL1                 SPOT LENGTH                                  
         DS    CL3                                                              
SRDATA   DS    0CL30                                                            
SRSPOT   DS    CL4                 SPOTS                                        
SRDOL    DS    CL4                 DOLLARS                                      
SRPNT    DS    CL4                 POINTS                                       
SRIMP    DS    CL4                 IMPRESSIONS                                  
SRPNTW   DS    CL4                                                              
SRWGHT   DS    CL4                 MARKET WEIGHT                                
SREQUIV  DS    CL4                 MARKET EQUIVALENCE                           
SRMKT2   DS    CL2                                                              
         DS    0F                                                               
         SPACE 2                                                                
HOLDREC  DS    0CL46                                                            
HLDKEY   DS    0CL16                                                            
HLDTRGT  DS    CL1                                                              
HLDTGT1  DS    CL1                                                              
HLDDEMO  DS    CL1                                                              
HLDMSEQ  DS    CL2                                                              
HLDREN   DS    CL2                                                              
HLDRST   DS    CL2                                                              
HLDMKT   DS    CL2                                                              
HLDDPT   DS    CL1                                                              
HLDSLN   DS    CL1                                                              
         DS    CL3                                                              
HLDDATA  DS    0CL30                                                            
HLDSPOT  DS    CL4                                                              
HLDDOL   DS    CL4                                                              
HLDPNT   DS    CL4                                                              
HLDIMP   DS    CL4                                                              
HLDPNTW  DS    CL4                                                              
HLDWGHT  DS    CL4                                                              
HLDEQUIV DS    CL4                                                              
HLDMKT2  DS    CL2                                                              
         DS    0F                                                               
HLD2REC  DS    0CL46                                                            
HLD2KEY  DS    0CL16                                                            
         DS    CL16                                                             
HLD2DATA DS    0CL30                                                            
         DS    CL20                                                             
HLD2PNTW DS    CL4                                                              
HLD2WGHT DS    CL4                                                              
         DS    CL4                                                              
HLD2MKT2 DS    CL2                                                              
CPDGRP   DS    CL5                 DEMO GROUPS FOR CURRENT DEMO                 
CPDPT    DS    CL1                 DAYPART FOR CURRENT DEMO                     
DPAREA   DS    32F                 DAYPART TOTAL AREAS                          
DPGAREA  DS    32F                 MARKET GROUP TOTAL AREA                      
OSPMAR   DS    0CL24               ONE SPOT PER MARKET AREA                     
OSPWGHT  DS    F                                                                
OSPSPT   DS    F                                                                
OSPDOL   DS    F                                                                
OSPDOLE  DS    F                                                                
OSPPNT   DS    F                                                                
OSPIMP   DS    F                                                                
OSPPNTW  DS    F                                                                
SLOT     DS    F                                                                
RELO     DC    A(0)                                                             
VSORTER  DS    F                                                                
SAVERE   DS    F                                                                
MSEQ     DC    H'0'                                                             
EOFSW    DC    X'00'                                                            
REQTGT   DS    C                                                                
ACTSW    DS    C                                                                
HIGROUP  DS    H                                                                
MGRPSW   DS    CL1                 MARKET GROUP SWITCH                          
SLTTGT   DS    CL1                                                              
SLTDPT   DS    CL1                                                              
         EJECT                                                                  
SORT     DC    C'SORT FIELDS=(1,12,BI,A),WORK=1 '                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=46 '                                      
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
         EJECT                                                                  
       ++INCLUDE CPREPWORKD                                                     
       ++INCLUDE CPREPMODES                                                     
       ++INCLUDE CPGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049CPREP7402 05/01/02'                                      
         END                                                                    
