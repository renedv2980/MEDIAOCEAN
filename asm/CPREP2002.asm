*          DATA SET CPREP2002  AT LEVEL 097 AS OF 05/01/02                      
*PHASE CP2002A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'CPREP2002-COST PER POINT GUIDE'                                 
         PRINT NOGEN                                                            
CP2002   CSECT                                                                  
         NMOD1 0,CP2002,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING CPWORKD,RA,RC                                                    
         L     R2,=V(CP20WK)                                                    
         USING CP20WK,R2                                                        
         AR    R2,R5                                                            
         ST    R5,RELO                                                          
         ST    R2,CP20R2                                                        
         STM   RA,RC,CP20RA                                                     
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   BK10                                                             
         MVI   ACTSW,0                                                          
         LA    RE,MYHEAD                                                        
         ST    RE,HEADHOOK                                                      
         B     EXIT                                                             
         SPACE 2                                                                
BK10     CLI   MODE,REQFRST                                                     
         BNE   BK20                                                             
         L     R6,=V(DPLUTAB)                                                   
         A     R6,RELO                                                          
         ST    R6,DAYTABL                                                       
         XC    MENU,MENU                                                        
         CLC   QSELECT,=C'   '                                                  
         BE    FMENU2                                                           
         PACK  DUB,QSELECT                                                      
         CVB   RF,DUB                                                           
         STC   RF,MENU+1                                                        
         CH    RF,=H'129'                                                       
         BL    FMENU2                                                           
         SH    RF,=H'128'                                                       
         L     RE,ADDEMGRP                                                      
FMENU    ZIC   R8,0(RE)                                                         
         BCT   RF,*+8                                                           
         B     FMENU1                                                           
         AR    RE,R8                                                            
         B     FMENU                                                            
FMENU1   MVC   MENU,0(RE)                                                       
FMENU2   DS    0H                                                               
         SPACE 2                                                                
         CLI   ACTSW,2             SORT ALREADY OPEN                            
         BE    BK10A                                                            
         L     R6,=V(SORTER)                                                    
         A     R6,RELO                                                          
         ST    R6,VSORTER                                                       
         GOTO1 VSORTER,DMCB,SORT,RECCARD,0                                      
BK10A    MVI   ACTSW,0                                                          
         L     R6,=V(DPAREAC)                                                   
         A     R6,RELO                                                          
         ST    R6,DPAREA                                                        
         LA    R7,9                                                             
         LA    R6,QDATATYP                                                      
         LA    RE,4                                                             
BK10B    CLI   0(R6),C' '                                                       
         BE    BK10C                                                            
         LA    RE,1(RE)                                                         
         LA    R6,1(R6)                                                         
         BCT   R7,BK10B                                                         
BK10C    STC   RE,ALLOWLIN                                                      
         LA    RE,3(RE)            ADJUST ALLOWLIN                              
         STC   RE,ALLOWLIN                                                      
         B     EXIT                                                             
         SPACE 2                                                                
         EJECT                                                                  
BK30     CLI   MODE,PROCDATA                                                    
         BNE   BK40                                                             
         L     R3,ADDATA                                                        
         USING CPKEYD,R3                                                        
         CLI   QOPT3,C'Y'          TARGETS ONLY                                 
         BNE   BK301                NO                                          
         CLC   CPKDEMO,CPKTARGT    TARGET SAME AS DEMO                          
         BNE   EXIT                 NO - EXIT                                   
         SPACE 2                                                                
BK301    DS    0H                                                               
         LA    RF,MENU                                                          
         SPACE 2                                                                
BK303    LA    RF,1(RF)            CHECK FOR DEMO IN MENU                       
BK303A   CLC   CPKDEMO,0(RF)       FOUND                                        
         BE    BK304                                                            
         CLI   0(RF),X'FF'         NOT IN MENU                                  
         BE    EXIT                                                             
         CLI   0(RF),0                                                          
         BNE   BK303B                                                           
         CLC   QTARGET,=C'   '                                                  
         BNE   BK303B                                                           
         MVC   0(1,RF),CPKDEMO     BUILD MENU                                   
         B     BK304                                                            
BK303B   DS    0H                                                               
         LA    RF,1(RF)                                                         
         B     BK303A                                                           
         SPACE 2                                                                
BK304    XC    SRREC,SRREC         BUILD PROTOTYPE RECORD                       
         MVC   SRRST,MCOUNT+2                                                   
         MVC   SRTGT,CPKDEMO                                                    
         MVC   SRDEMO,CPKDEMO                                                   
         MVC   SRDPT,CPKDAYPT                                                   
         MVC   SRSLN,CPKSPTLN                                                   
         MVC   SRMKT,CPKMKT                                                     
         MVC   SRWGHT+2(2),MKTWT                                                
         MVC   SRMKT2,CPKMKT                                                    
         MVI   ACTSW,1                                                          
         LA    R4,PERTABLE                                                      
         USING CPERD,R4                                                         
         L     RF,NPERIODS                                                      
BK20C    L     RE,SRDOL            SUM PERIODS                                  
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
         A     R4,WIDTHPER                                                      
         BCT   RF,BK20C                                                         
         SPACE 2                                                                
         L     RE,SRDOL            EQUIVALENCE DOLLARS                          
         SRDA  RE,32                                                            
         M     RE,=F'1000'                                                      
         SLDA  RE,1                                                             
         D     RE,EQVFACT                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,SRDOLE                                                        
         SPACE 2                                                                
* GENERATE SORT RECORDS                                                         
PUTSORT  XC    SRREN,SRREN                                                      
         XC    SRRST,SRRST                                                      
         MVC   SRMSEQ,MSEQ                                                      
         MVC   SRMKT,CPKMKT                                                     
         MVC   SVDOLE,SRDOLE       SAVE EQUIVALENCED DOLLARS                    
         MVC   SRDOLE,SRDOL                                                     
         CLI   QOPT2,C'Y'                                                       
         BNE   SUPSLX                                                           
         MVI   SRSLN,X'FF'                                                      
         MVC   SRDOLE,SVDOLE                                                    
SUPSLX   DS    0H                                                               
         L     RE,SRPNT                                                         
         SRDA  RE,32                                                            
         M     RE,SRWGHT                                                        
         ST    RF,SRPNTW                                                        
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
         CLC   SRREN,HIGROUP       SET HIGHEST ACTIVE GROUP                     
         BL    *+10                                                             
         MVC   HIGROUP,SRREN                                                    
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
         BE    PUTSRTAA                                                         
         MVC   SRRST,=X'FFFE'                                                   
PUTSORTA BAS   R9,PSOK                                                          
PUTSRTAA MVC   HALF,SRREN          SET UP DECADE GROUPS                         
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
         B     EXIT                                                             
         SPACE 2                                                                
PSOK     CLC   QUESTOR(6),=C'ZTRACE'                                            
         BNE   PSOK1                                                            
         GOTO1 HEXOUT,DMCB,SRREC,P,48,0,0                                       
         GOTO1 REPORT                                                           
PSOK1    GOTO1 VSORTER,DMCB,=C'PUT',SRREC                                       
         CLI   SRSLN,30                                                         
         BE    *+8                                                              
         CLI   SRSLN,60                                                         
         BNER  R9                                                               
         MVC   SVSLN,SRSLN         COMBINE 30 AND 60                            
         MVI   SRSLN,63                                                         
         MVC   SRDOLE,SVDOLE                                                    
         GOTO1 VSORTER,DMCB,=C'PUT',SRREC                                       
         MVC   SRDOLE,SRDOL                                                     
         MVC   SRSLN,SVSLN                                                      
         BR    R9                                                               
         EJECT                                                                  
BK20     CLI   MODE,MKTFRST                                                     
         BNE   BK30                                                             
         XC    MSEQ,MSEQ                                                        
         MVC   MSEQ(2),MKTRANK                                                  
         CLI   QMKTSEQ,C'C'                                                     
         BNE   *+10                                                             
         MVC   MSEQ(2),MARKET                                                   
         CLI   QMKTSEQ,C'A'                                                     
         BNE   *+10                                                             
         MVC   MSEQ,MKTNAME                                                     
         B     EXIT                                                             
EXIT     XMOD1 1                                                                
         EJECT                                                                  
BK40     CLI   MODE,REQLAST                                                     
         BNE   BK50                                                             
         XC    HLD2REC,HLD2REC                                                  
         XC    HOLDREC,HOLDREC                                                  
         L     RE,DPAREA                                                        
         L     RF,=F'512'                                                       
         XCEF                                                                   
         MVI   CNT63,0                                                          
         MVI   EOFSW,0                                                          
         CLI   ACTSW,0             ACTIVITY                                     
         BNE   BK40A                                                            
         MVI   ACTSW,2             BYPASS SORT OPEN                             
         B     EXIT                                                             
         SPACE 2                                                                
BK50     CLI   MODE,RUNLAST        MAKE SURE SORT IS CLOSED                     
         BNE   EXIT                                                             
         CLI   ACTSW,2             OPEN NOT CLOSED                              
         BNE   EXIT                                                             
         GOTO1 VSORTER,DMCB,=C'END',SRREC                                       
         B     EXIT                                                             
         EJECT                                                                  
BK40A    LA    RE,0                                                             
         LA    RF,MENU+1                                                        
BK40B    CLI   0(RF),X'FF'                                                      
         BE    BK40C                                                            
         CLI   0(RF),0                                                          
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     BK40B                                                            
         SPACE 2                                                                
BK40C    C     RE,=F'7'                                                         
         BH    BK40E                                                            
         MVC   SVMENU,MENU+1                                                    
         XC    MENU,MENU                                                        
         LA    RE,SVMENU                                                        
         LA    RF,MENU+1                                                        
BK40D    MVC   1(1,RF),0(RE)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,2(RF)                                                         
         CLI   0(RE),0                                                          
         BNE   BK40D                                                            
         SPACE 2                                                                
BK40E    GOTO1 =V(SETHL),DMCB,(RA),MENU,RR=RELO                                 
         EJECT                                                                  
PR1      MVC   SRREC,HLD2REC                                                    
         CLI   EOFSW,1                                                          
         BE    SD1                                                              
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     RE,4(R1)                                                         
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   PR3                                                              
         XC    HLD2REC,HLD2REC                                                  
         MVI   EOFSW,1                                                          
         GOTO1 VSORTER,DMCB,=C'END',SRREC                                       
         B     PRX                                                              
         SPACE 2                                                                
PR3      MVC   HLD2REC,0(RE)       ACCUMULATE DUPS                              
         OC    SRREC,SRREC                                                      
         BZ    PR1                                                              
         CLC   HLD2KEY,SRREC                                                    
         BNE   PRX1                                                             
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
         SPACE 2                                                                
PRX      DS    0H                                                               
PRX1     CLC   SRREN,HIGROUP       BYPASS HIGH GROUPS                           
         BH    PR1                                                              
         CLC   QUESTOR(6),=C'ZTRACE'                                            
         BNE   SD1                                                              
         GOTO1 HEXOUT,DMCB,SRREC,P,48,0,0                                       
         GOTO1 REPORT                                                           
         EJECT                                                                  
* SUM RECORDS INTO TABLE IF DETAIL ITEM                                         
SD1      DS    0H                                                               
         OC    HOLDREC,HOLDREC                                                  
         BZ    SD2                                                              
         CLC   SRRST,HLDRST        MARKET CHANGE                                
         BNE   CPMR                 DO - REPORT                                 
         CLC   SRDPT(2),HLDDPT     DAYPART/SPTLN CHANGE                         
         BNE   CPMR                 DO - REPORT                                 
         CLC   SRMKT,HLDMKT                                                     
         BNE   CPMR                                                             
SD2      BAS   R9,FSLOT            FIND DEMO SLOT                               
         L     RE,SLOT                                                          
         MH    RE,=H'32'                                                        
         L     R4,DPAREA                                                        
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
         A     RE,SRDOLE                                                        
         ST    RE,DPDDOLE                                                       
         L     RE,DPDPNT                                                        
         A     RE,SRPNT                                                         
         ST    RE,DPDPNT                                                        
         L     RE,DPDPNTW                                                       
         A     RE,SRPNTW                                                        
         ST    RE,DPDPNTW                                                       
         L     RE,DPDIMP                                                        
         A     RE,SRIMP                                                         
         ST    RE,DPDIMP                                                        
         MVC   HOLDREC,SRREC                                                    
         B     PR1                                                              
         DROP  R4                                                               
         EJECT                                                                  
* FIND SLOT FOR CURRENT DEMO                                                    
FSLOT    LA    RE,MENU                                                          
         SR    R8,R8                                                            
         SPACE 2                                                                
FSLOT2   CLC   SRDEMO,1(RE)        SET TO DEMO SEQUENCE                         
         BE    FSLOT3                                                           
         CLI   1(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                DEMO NOT IN MENU                             
         LA    RE,1(RE)                                                         
         LA    R8,1(R8)                                                         
         B     FSLOT2                                                           
         SPACE 2                                                                
FSLOT3   ST    R8,SLOT             SAVE SLOT NUMBER                             
         BR    R9                                                               
         EJECT                                                                  
*PRINT REPORT                                                                   
CPMR     CLC   SRDPT(2),HLDDPT                                                  
         BE    CPMR1                                                            
         CLI   HLDSLN,30           COUNT 30 AND 60                              
         BE    *+8                                                              
         CLI   HLDSLN,60                                                        
         BNE   CPMA1                                                            
         ZIC   RE,CNT63                                                         
         LA    RE,1(RE)                                                         
         STC   RE,CNT63                                                         
CPMA1    CLI   HLDSLN,63                                                        
         BNE   CPMA2                                                            
         CLI   CNT63,1                                                          
         BE    RPTDONE             SUPPRESS 3+6 LINE                            
CPMA2    DS    0H                                                               
         MVI   P,C' '                                                           
         MVC   ALLOWLIN,SVALIN     SET ALLOWLIN                                 
         GOTO1 REPORT                                                           
         L     RE,DAYTABL                                                       
CPMRA1   CLI   0(RE),X'FF'                                                      
         BNE   CPMRA1A                                                          
         MVC   P(3),=C'ZZZ'                                                     
         B     CPMRA2A                                                          
CPMRA1A  CLC   0(1,RE),HLDDPT                                                   
         BE    CPMRA2                                                           
         LA    RE,9(RE)                                                         
         B     CPMRA1                                                           
CPMRA2   MVC   P(3),5(RE)                                                       
         MVI   P+3,C'-'                                                         
CPMRA2A  EDIT  HLDSLN,(3,P+4),,ALIGN=LEFT                                       
         CLI   HLDSLN,63                                                        
         BNE   *+10                                                             
         MVC   P+4(3),=C'3+6'                                                   
         CLI   HLDSLN,X'FF'                                                     
         BNE   *+10                                                             
         MVC   P+3(4),=C'    '                                                  
CPMR1    LA    R5,QDATATYP-1                                                    
         ST    R5,SVR5                                                          
         LA    RE,P                                                             
         ST    RE,PLADDR                                                        
CPMDR2   L     R4,DPAREA                                                        
         USING DPD,R4                                                           
         L     R5,SVR5             RESTORE R5                                   
         LA    R5,1(R5)                                                         
         ST    R5,SVR5             SAVE R5                                      
         LA    R6,QDATATYP         CHECK FOR END                                
         LA    R6,9(R6)                                                         
         CR    R5,R6                                                            
         BE    RPTDONE                                                          
         CLI   0(R5),C' '                                                       
         BE    RPTDONE                                                          
         MVC   HALF,0(R5)                                                       
         NI    HALF,X'0F'                                                       
         ZIC   RF,HALF                                                          
         BCTR  RF,0                                                             
         SLL   RF,2                X 4                                          
         MVI   SLOTIDX,1                                                        
         XC    SLOT1,SLOT1                                                      
         XC    SLOTN,SLOTN                                                      
         LA    RE,EDITS(RF)                                                     
         BR    RE                                                               
         EJECT                                                                  
EDITS    B     ECPP                1. COST PER POINT                            
         B     ECPM                2. COST PER THOUSAND                         
         B     ECOST               3. COST                                      
         B     ESPOTS              4. NUMBER OF SPOTS                           
         B     EGRP                5. POINTS                                    
         B     EIMPS               6. IMPRESSIONS                               
         B     ECPS                7. COST PER SPOT                             
         B     EPPS                8. POINTS PER SPOT                           
         B     EIPS                9. IMPRESSIONS PER SPOT                      
         EJECT                                                                  
ECPP     MVC   PCAP,=C'CPP  '      EDIT CPP                                     
         BAS   R9,ESET                                                          
ECPP1    L     RF,DPDDOLE                                                       
         OC    DPDDOLE,DPDDOLE                                                  
         BZ    ECPP2                                                            
         MH    RF,=H'1000'                                                      
         ST    RF,DUB                                                           
         CLI   QOPT1,C'Y'                                                       
         BE    ECPP3                                                            
         L     RF,DPDPNTW                                                       
         L     RE,DPDPNTW                                                       
         BAS   R9,UNWEIGHT                                                      
         ST    RF,DUB+4                                                         
         BAS   R9,DIV2                                                          
ECPP2    BAS   R9,NXTSLOT                                                       
         B     ECPP1                                                            
ECPP3    MVC   DUB+4(4),DPDPNT     UNWEIGHTED REPORT                            
         BAS   R9,DIV2                                                          
         B     ECPP2                                                            
         SPACE 2                                                                
ECPM     MVC   PCAP,=C'CPM  '      EDIT CPM                                     
         BAS   R9,ESET                                                          
ECPM1    L     RF,DPDDOLE                                                       
         MH    RF,=H'100'                                                       
         ST    RF,DUB                                                           
         MVC   DUB+4(4),DPDIMP                                                  
         BAS   R9,DIV2                                                          
         BAS   R9,NXTSLOT                                                       
         B     ECPM1                                                            
         SPACE 2                                                                
ECOST    MVC   PCAP,=C'$-000'                                                   
         BAS   R9,ESET                                                          
ECOST1   MVC   DUB(4),DPDDOL                                                    
         MVC   DUB+4(4),=F'10'                                                  
         BAS   R9,DIV2                                                          
         CLC   DPDDOL,DPDDOLE                                                   
         BE    ECOST2                                                           
         L     R9,PLADDR                                                        
         LA    R9,132(R9)                                                       
         MVI   LIN2SW,1                                                         
         MVC   8(5,R9),=C'E$000'                                                
         MVC   DUB(4),DPDDOLE                                                   
         MVC   DUB+4(4),=F'10'                                                  
         LA    R6,132(R6)                                                       
         BAS   R9,DIV2                                                          
         SH    R6,=H'132'                                                       
ECOST2   DS    0H                                                               
         BAS   R9,NXTSLOT                                                       
         B     ECOST1                                                           
         SPACE 2                                                                
ESPOTS   MVC   PCAP,=C'SPOTS'                                                   
         BAS   R9,ESET                                                          
ESPOTS1  EDIT  DPDSPT,(5,(R6))                                                  
         XC    SLOT1,SLOT1                                                      
         BAS   R9,NXTSLOT                                                       
         B     ESPOTS1                                                          
         SPACE 2                                                                
EGRP     MVC   PCAP,=C'GRP  '                                                   
         BAS   R9,ESET                                                          
EGRP1    L     RE,DPDPNTW                                                       
         LTR   RE,RE                                                            
         BZ    EGRP2                                                            
         BAS   R9,UNWEIGHT                                                      
         CLI   QOPT1,C'Y'                                                       
         BNE   *+8                                                              
         L     RF,DPDPNT                                                        
         ST    RF,DUB                                                           
         MVC   DUB+4(4),=F'1'                                                   
         BAS   R9,DIV1                                                          
EGRP2    BAS   R9,NXTSLOT                                                       
         B     EGRP1                                                            
         SPACE 2                                                                
EIMPS    MVC   PCAP,=C'IMPS '                                                   
         BAS   R9,ESET                                                          
EIMPS1   CLC   DPDIMP,=F'99999'                                                 
         BH    EIMPS2                                                           
         EDIT  DPDIMP,(5,(R6))                                                  
         L     RF,DPDIMP                                                        
         BAS   RE,SLOTIT                                                        
         B     EIMPS5                                                           
EIMPS2   STC   R5,WORK                                                          
         TM    WORK,1              SECOND LINE                                  
         BZ    EIMPS3               NO                                          
         SH    R6,=H'3'            SET ADDRESS FOR FIRST LINE                   
         CLI   0(R6),C' '          PREVOIUS HIGH                                
         BE    EIMPS2A                                                          
         SH    R6,=H'3'                                                         
         MVC   132(5,R6),0(R6)                                                  
         XC    0(5,R6),0(R6)                                                    
         AH    R6,=H'3'                                                         
EIMPS2A  DS    0H                                                               
         EDIT  DPDIMP,(8,(R6))                                                  
         L     RF,DPDIMP                                                        
         BAS   RE,SLOTIT                                                        
         AH    R6,=H'3'                                                         
         B     EIMPS4                                                           
EIMPS3   EDIT  DPDIMP,(8,129(R6))  EDIT FOR SECOND LINE                         
         L     RF,DPDIMP                                                        
         BAS   RE,SLOTIT                                                        
EIMPS4   MVI   LIN2SW,1                                                         
EIMPS5   DS    0H                                                               
         BAS   R9,NXTSLOT                                                       
         B     EIMPS1                                                           
         SPACE 2                                                                
ECPS     MVC   PCAP,=C'CPS  '                                                   
         BAS   R9,ESET                                                          
ECPS1    OC    DPDSPT,DPDSPT                                                    
         BZ    ECPS2                                                            
         L     RE,DPDDOLE                                                       
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,DPDSPT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(5,(R6))                                                    
         BAS   RE,SLOTIT                                                        
ECPS2    BAS   R9,NXTSLOT                                                       
         B     ECPS1                                                            
EPPS     MVC   PCAP,=C'PPS  '                                                   
         BAS   R9,ESET                                                          
EPPS1    OC    DPDSPT,DPDSPT                                                    
         BZ    EPPS2                                                            
         L     RE,DPDPNT                                                        
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,DPDSPT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(5,(R6)),1                                                  
         BAS   RE,SLOTIT                                                        
EPPS2    BAS   R9,NXTSLOT                                                       
         MVC   DUB+4(4),=F'10'                                                  
         B     EPPS1                                                            
         SPACE 2                                                                
EIPS     MVC   PCAP,=C'IPS  '                                                   
         BAS   R9,ESET                                                          
EIPS1    OC    DPDSPT,DPDSPT                                                    
         BZ    EIPS2                                                            
         L     RE,DPDIMP                                                        
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,DPDSPT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(5,(R6))                                                    
         BAS   RE,SLOTIT                                                        
EIPS2    BAS   R9,NXTSLOT                                                       
         B     EIPS1                                                            
         EJECT                                                                  
* SET UP VARIOUS EDITS                                                          
ESET     L     R6,PLADDR                                                        
         MVI   LIN2SW,0                                                         
         LA    R6,8(R6)                                                         
         MVC   0(5,R6),PCAP        MOVE IN TITLE                                
         LA    R6,6(R6)                                                         
         L     R4,DPAREA                                                        
         USING DPD,R4                                                           
         LA    R5,16                                                            
         BR    R9                                                               
UNWEIGHT L     RE,DPDPNTW                                                       
         CLC   DPDWGHT,=F'0'                                                    
         BNE   *+10                                                             
         MVC   DPDWGHT,=F'1'                                                    
         SRDA  RE,32                                                            
         C     RF,=F'2000000000'                                                
         BH    UNW2                                                             
         SLDA  RE,1                                                             
         D     RE,DPDWGHT                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
UNW2     D     RE,DPDWGHT          DONT ROUND                                   
         BR    R9                                                               
         SPACE 2                                                                
DIV2     L     RE,DUB              EDIT TO 2 DECIMALS                           
         LTR   RE,RE                                                            
         BZR   R9                                                               
         C     RE,=F'2000000000'                                                
         BL    *+12                                                             
         BAS   R7,DIV10                                                         
         B     DIV2                                                             
         OC    DUB+4(4),DUB+4                                                   
         BZR   R9                                                               
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         D     RE,DUB+4                                                         
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         C     RF,=F'9999'                                                      
         BH    DIV21                                                            
         EDIT  (RF),(5,(R6)),2                                                  
         BAS   RE,SLOTIT                                                        
         BR    R9                                                               
DIV21    SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
DIV12    C     RF,=F'9999'                                                      
         BH    DIV22                                                            
         EDIT  (RF),(5,(R6)),1                                                  
         BAS   RE,SLOTIT                                                        
         BR    R9                                                               
DIV22    SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         C     RF,=F'99999'                                                     
         BH    DIV23                                                            
         EDIT  (RF),(5,(R6))                                                    
         BAS   RE,SLOTIT                                                        
         BR    R9                                                               
DIV23    MVC   0(5,R6),=C' HIGH'                                                
         BR    R9                                                               
         SPACE 2                                                                
DIV1     L     RE,DUB              EDIT TO 1 DECIMAL PLACE                      
         LTR   RE,RE                                                            
         BZR   R9                                                               
         OC    DUB+4(4),DUB+4                                                   
         BZR   R9                                                               
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,DUB+4                                                         
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         B     DIV12                                                            
         SPACE 2                                                                
DIV10    L     RE,DUB              REDUCE BOTH NUMBERS BY X10                   
         A     RE,=F'5'                                                         
         SRDA  RE,32                                                            
         D     RE,=F'10'                                                        
         ST    RF,DUB                                                           
         L     RE,DUB+4                                                         
         A     RE,=F'5'                                                         
         SRDA  RE,32                                                            
         D     RE,=F'10'                                                        
         ST    RF,DUB+4                                                         
         BR    R7                                                               
         SPACE 2                                                                
NXTSLOT  CLI   QOPT4,C'Y'          INDEX ALL TO FIRST DEMO                      
         BNE   NXTSLOT2                                                         
         L     RE,SLOTN                                                         
         SRDA  RE,32                                                            
         M     RE,=F'1000'                                                      
         OC    SLOT1,SLOT1                                                      
         BZ    NXTSLOT2                                                         
         D     RE,SLOT1                                                         
         LA    R6,132(R6)                                                       
         EDIT  (RF),(5,(R6)),1                                                  
         XC    SLOTN,SLOTN                                                      
         SH    R6,=H'132'                                                       
         MVI   SLOTIDX,0                                                        
*                                                                               
NXTSLOT2 LA    R6,6(R6)                                                         
         LA    R4,32(R4)                                                        
         BCT   R5,*+8                                                           
         B     EDITX                                                            
         BR    R9                                                               
         SPACE 2                                                                
SLOTIT   ST    RF,SLOTN                                                         
         CLI   SLOTIDX,1                                                        
         BNE   *+8                                                              
         ST    RF,SLOT1                                                         
         BR    RE                                                               
         SPCAE 2                                                                
EDITX    L     RE,PLADDR                                                        
         LA    RE,132(RE)                                                       
         CLI   QOPT4,C'Y'                                                       
         BNE   *+8                                                              
         LA    RE,132(RE)                                                       
         CLI   LIN2SW,0                                                         
         BE    *+8                                                              
         LA    RE,132(RE)                                                       
         ST    RE,PLADDR                                                        
         B     CPMDR2                                                           
         EJECT                                                                  
RPTDONE  MVC   ALLOWLIN,SVALIN                                                  
         GOTO1 REPORT                                                           
         L     RE,DPAREA                                                        
         L     RF,=F'512'                                                       
         XCEF                                                                   
         CLI   HLDSLN,63                                                        
         BNE   *+8                                                              
         MVI   CNT63,0                                                          
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
         EJECT                                                                  
*HEADLINES ROUTINES                                                             
         DS    0D                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=CP20RB                                                      
         DROP  RF                                                               
         L     R2,CP20R2                                                        
         LM    RA,RC,CP20RA                                                     
         GOTO1 =V(HEADING),DMCB,(RA),RR=RELO                                    
         XIT1                                                                   
CP20RA   DS    F                                                                
CP20RB   DS    F                                                                
CP20RC   DS    F                                                                
CP20R2   DS    F                                                                
         LTORG                                                                  
         EJECT                                                                  
SETHL    CSECT                                                                  
         NMOD1 0,SETHL                                                          
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     RE,4(R1)                                                         
         XC    MYH9,MYH9                                                        
         XC    MYH10,MYH10                                                      
         XC    MYH11,MYH11                                                      
         LA    R6,MYH9                                                          
         LA    R7,MYH10+6                                                       
         LA    R8,MYH11                                                         
         LA    RE,1(RE)                                                         
SETHL1   CLI   0(RE),0                                                          
         BE    SETHL2                                                           
         CLI   0(RE),X'FF'                                                      
         BE    SETHLX                                                           
         ZIC   RF,0(RE)            GET DEMO NUMBER                              
         BCTR  RF,0                                                             
         MH    RF,=H'7'                                                         
         A     RF,ADDEMBUF                                                      
         MVC   0(7,R6),0(RF)                                                    
SETHL2   LA    R6,12(R6)                                                        
         LA    RE,1(RE)                                                         
         LA    R8,1(R8)                                                         
         CLI   0(RE),0                                                          
         BE    SETHL3                                                           
         CLI   0(RE),X'FF'                                                      
         BE    SETHLX                                                           
         ZIC   RF,0(RE)                                                         
         BCTR  RF,0                                                             
         MH    RF,=H'7'                                                         
         A     RF,ADDEMBUF                                                      
         MVC   0(7,R7),0(RF)                                                    
SETHL3   LA    R7,12(R7)                                                        
         LA    RE,1(RE)                                                         
         LA    R8,6(R8)                                                         
         B     SETHL1                                                           
         SPACE 2                                                                
SETHLX   XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
HEADING  CSECT                                                                  
         NMOD1 0,HEADING                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         MVC   H9+13(96),MYH9                                                   
         MVC   H10+13(96),MYH10                                                 
         MVC   H11+13(96),MYH11                                                 
         MVC   H5+40(9),=C'UNIVERSE '                                           
         MVC   H5+50(3),=C'ALL'                                                 
         CLC   QTARGET,=C'   '                                                  
         BE    HDING2                                                           
         PACK  DUB,QTARGET                                                      
         CVB   R9,DUB                                                           
         BCTR  R9,0                                                             
         MH    R9,=H'7'                                                         
         A     R9,ADDEMBUF                                                      
         MVC   H5+50(7),0(R9)                                                   
         MVC   H5+57(13),=C' TARGETS ONLY'                                      
HDING2   DS    0H                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   *+10                                                             
         MVC   H3+40(19),=C'(TARGET DEMOS ONLY)'                                
         MVC   H6+44(12),=C'(WEIGHTED)  '                                       
         CLI   QOPT1,C'Y'                                                       
         BNE   *+10                                                             
         MVC   H6+44(12),=C'(UNWEIGHTED)'                                       
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
         EDIT  HALF,(3,H5+12),,ALIGN=LEFT                                       
         MVC   H5+18(5),=C'RANK='                                               
         MVC   HALF,2(RE)                                                       
         EDIT  HALF,(3,H5+23),,ALIGN=LEFT                                       
         SPACE 2                                                                
MYHEADX  XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
CP20WK   CSECT                                                                  
SRREC    DS    0CL54               SORT RECORD                                  
SRKEY    DS    0CL24               SORT KEY                                     
SRREN    DS    CL2                 END RANK   (0 IF SINGLE MARKET)              
SRRST    DS    CL2                 START RANK (MARKET SEQUENCE)                 
SRMSEQ   DS    CL13                MARKET SEQUENCE                              
SRMKT    DS    CL2                 MARKET NUMBER                                
SRDPT    DS    CL1                 DAYPART                                      
SRSLN    DS    CL1                 SPOT LENGTH                                  
SRTGT    DS    CL1                 TARGET                                       
SRDEMO   DS    CL1                 DEMO                                         
         DS    CL1                 SPARE                                        
SRDATA   DS    0CL24                                                            
SRSPOT   DS    CL4                 SPOTS                                        
SRDOL    DS    CL4                 DOLLARS                                      
SRPNT    DS    CL4                 POINTS                                       
SRIMP    DS    CL4                 IMPS                                         
SRPNTW   DS    CL4                 WEIGHTED POINTS                              
SRDOLE   DS    CL4                 EQUIVALENCED DOLLARS                         
SRWGHT   DS    CL4                 WEIGHT                                       
SRMKT2   DS    CL2                                                              
         DS    0F                                                               
         SPACE 2                                                                
HOLDREC  DS    0CL54                                                            
HLDKEY   DS    0CL24                                                            
HLDREN   DS    CL2                                                              
HLDRST   DS    CL2                                                              
HLDMSEQ  DS    CL13                                                             
HLDMKT   DS    CL2                                                              
HLDDPT   DS    CL1                                                              
HLDSLN   DS    CL1                                                              
HLDTGT   DS    CL1                                                              
HLDDEMO  DS    CL1                                                              
         DS    CL1                                                              
HLDDATA  DS    0CL24                                                            
HLDSPOT  DS    CL4                                                              
HLDDOL   DS    CL4                                                              
HLD2MKT  DS    CL2                                                              
HLDPNT   DS    CL4                                                              
HLDIMP   DS    CL4                                                              
HLDPNTW  DS    CL4                                                              
HLDDOLE  DS    CL4                                                              
HLDWGHT  DS    CL4                                                              
         DS    CL2                                                              
         DS    0F                                                               
HLD2REC  DS    0CL54                                                            
HLD2KEY  DS    0CL24                                                            
         DS    CL24                                                             
HLD2DATA DS    CL24                                                             
HLD2WGHT DS    CL4                                                              
HLD2MKT2 DS    CL2                                                              
RELO     DC    A(0)                                                             
MCOUNT   DC    F'0'                                                             
SVR5     DC    F'0'                                                             
HIGROUP  DC    H'0'                                                             
MSEQ     DS    CL13                                                             
SLTDEM   DS    C                                                                
CNT63    DC    X'00'                                                            
LIN2SW   DS    X'00'                                                            
SVALIN   DC    X'00'                                                            
VSORTER  DC    F'0'                                                             
SLOT     DC    F'0'                                                             
DPAREA   DC    F'0'                                                             
PLADDR   DS    F                                                                
SVDOLE   DS    F                                                                
SLOT1    DC    F'0'                                                             
SLOTN    DC    F'0'                                                             
SLOTIDX  DS    C                                                                
PCAP     DS    CL5                                                              
EOFSW    DS    CL1                                                              
ACTSW    DS    CL1                                                              
SVSLN    DS    CL1                                                              
SVRTRN   DC    F'0'                                                             
MYH9     DS    CL96                                                             
MYH10    DS    CL96                                                             
MYH11    DS    CL96                                                             
SORT     DC    C'SORT FIELDS=(1,24,BI,A,53,2,BI,A),WORK=2 '                     
RECCARD  DC    C'RECORD TYPE=F,LENGTH=54 '                                      
         EJECT                                                                  
MENU     DS    CL16                                                             
         DC    X'FF'                                                            
SVMENU   DS    CL8                                                              
         DC    X'00'                                                            
         LTORG                                                                  
         EJECT                                                                  
DPD      DSECT                                                                  
DPDNMKT  DS    F                                                                
DPDWGHT  DS    F                                                                
DPDSPT   DS    F                                                                
DPDDOL   DS    F                                                                
DPDDOLE  DS    F                                                                
DPDPNT   DS    F                                                                
DPDIMP   DS    F                                                                
DPDPNTW  DS    CL4                                                              
DPAREAC  CSECT                                                                  
         DS    530C                                                             
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CPREPWORKD                                                     
       ++INCLUDE CPREPMODES                                                     
       ++INCLUDE CPGENDPLUT                                                     
       ++INCLUDE CPGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'097CPREP2002 05/01/02'                                      
         END                                                                    
