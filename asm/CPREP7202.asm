*          DATA SET CPREP7202  AT LEVEL 135 AS OF 05/01/02                      
*PHASE CP7202A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'CPREP7202- CPP- COST ANALYSIS'                                  
         PRINT NOGEN                                                            
CP7202   CSECT                                                                  
         NMOD1 0,CP7202,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING CPWORKD,RA,RC                                                    
         L     R2,=V(CP20WK)                                                    
         USING CP20WK,R2                                                        
         AR    R2,R5                                                            
         ST    R5,RELO                                                          
         L     RF,=A(CP20R2)                                                    
         A     RF,RELO             RELO                                         
         ST    R2,0(RF)                                                         
         L     RF,=A(CP20RA)                                                    
         A     RF,RELO             RELO                                         
         STM   RA,RC,0(RF)                                                      
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   BK10                                                             
         MVI   ACTSW,0                                                          
         L     RE,=A(MYHEAD)                                                    
         A     RE,RELO                                                          
         ST    RE,HEADHOOK                                                      
         L     RE,=A(DEMLST)                                                    
         A     RE,RELO                                                          
         ST    RE,ADEMLST                                                       
         L     RE,=A(DBLOCKC)                                                   
         A     RE,RELO                                                          
         ST    RE,ADBLOCK                                                       
         L     RE,=A(DEMREC)                                                    
         A     RE,RELO                                                          
         ST    RE,ADEMREC                                                       
         XC    0(256,RE),0(RE)                                                  
         B     EXIT                                                             
         SPACE 2                                                                
BK10     CLI   MODE,REQFRST                                                     
         BNE   BK20                                                             
*                                                                               
         L     R6,=V(DPLUTAB)                                                   
         A     R6,RELO                                                          
         ST    R6,DAYTABL                                                       
         XC    MENU,MENU                                                        
         L     RE,=V(DEMGRP)       USE MY DEMO LIST                             
         A     RE,RELO                                                          
FMENU1   MVC   MENU+1(DEMGRPL),0(RE)                                            
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
*                                                                               
         L     R9,ACOMFACS                                                      
         USING COMFACSD,R9                                                      
         GOTO1 CDEMADDR,DMCB,(X'FF',ATABLES),ACOMFACS                           
*                                                                               
         B     EXIT                                                             
*                                                                               
ATABLES  DC    X'D1',3X'00',X'D2',3X'00',X'D3',3X'00'                           
         DC    X'D4',3X'00',X'D5',3X'00',X'D6',3X'00'                           
         DC    X'D7',3X'00',X'FF'                                               
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
         B     BK303B              **DONT ADD NEW DEMOS**                       
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
         CLI   SRDPT,C'N'          CHANGE PRIME TO SORT FIRST                   
         BNE   *+8                                                              
         MVI   SRDPT,X'01'                                                      
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
         L     R1,SRDOL            DIVIDE $ BY 10                               
         M     R0,=F'1'                                                         
         L     RF,=F'10'                                                        
         BAS   RE,DIVA                                                          
         ST    R1,SRDOL                                                         
*                                                                               
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
*                                                                               
         CLI   QOPT4,C'Y'          TEST TO USE MARKET LIST                      
         BNE   SUPSL4                                                           
         BAS   RE,MKTCHK           YES                                          
         B     EXIT                                                             
*                                                                               
SUPSL4   DS    0H                                                               
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
         MVC   BYTE,SRDPT                                                       
         MVI   SRDPT,X'FF'         DO ALL DPTS                                  
         GOTO1 (RF)                                                             
         MVC   SRDPT,BYTE                                                       
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
*                                                                               
MKTCHK   NTR1                                                                   
         L     R6,=V(MKTLST)                                                    
         A     R6,RELO                                                          
*                                                                               
MCK4     DS    0H                                                               
         CLI   0(R6),X'FF'         EOL                                          
         BE    MCK10                                                            
         CLI   0(R6),C'*'          TEST RANGE                                   
         BE    MCK6                YES                                          
         CLC   MKTNAME(5),0(R6)                                                 
         BNE   MCK9                                                             
         MVC   SRMKT,MARKET                                                     
         MVC   SRMSEQ,MSEQ                                                      
         XC    SRREN(4),SRREN                                                   
         B     MCK8                                                             
*                                                                               
MCK6     DS    0H                                                               
         CLC   MKTRANK,1(R6)                                                    
         BL    MCK9                                                             
         CLC   MKTRANK,3(R6)                                                    
         BH    MCK9                                                             
         XC    SRMKT,SRMKT                                                      
         XC    SRMSEQ,SRMSEQ                                                    
         MVC   SRRST,1(R6)                                                      
         XC    SRRST,=X'FFFF'                                                   
         MVC   SRREN,3(R6)                                                      
         CLC   SRREN,HIGROUP                                                    
         BL    *+10                                                             
         MVC   HIGROUP,SRREN                                                    
*                                                                               
MCK8     DS    0H                                                               
         BAS   R9,PSOK                                                          
*                                                                               
MCK9     DS    0H                                                               
         LA    R6,5(R6)                                                         
         B     MCK4                                                             
*                                                                               
MCK10    DS    0H                                                               
         XIT                                                                    
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
         SPACE 2                                                                
DIVA     DIV   (R0),(RF)                                                        
         EJECT                                                                  
BK40     DS    0H                                                               
         GOTO1 =V(BK40CSCT),DMCB,(RA),RR=RELO                                   
         B     EXIT                                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
BK40CSCT CSECT                                                                  
         NMOD1 0,BK40CSCT                                                       
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
*                                                                               
         CLI   MODE,REQLAST                                                     
         BNE   BK50                                                             
         XC    HLD2REC,HLD2REC                                                  
         XC    HOLDREC,HOLDREC                                                  
         XC    DP1PPS,DP1PPS                                                    
         XC    DP1IPS,DP1IPS                                                    
         L     RE,DPAREA                                                        
         L     RF,=F'512'                                                       
         XCEF                                                                   
         MVI   CNT63,0                                                          
         MVI   EOFSW,0                                                          
         CLI   ACTSW,0             ACTIVITY                                     
         BNE   BK40A                                                            
         MVI   ACTSW,2             BYPASS SORT OPEN                             
         B     EXIT2                                                            
         SPACE 2                                                                
BK50     CLI   MODE,RUNLAST        MAKE SURE SORT IS CLOSED                     
         BNE   EXIT2                                                            
         CLI   ACTSW,2             OPEN NOT CLOSED                              
         BNE   EXIT2                                                            
         GOTO1 VSORTER,DMCB,=C'END',SRREC                                       
         B     EXIT2                                                            
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
         CLC   SRREN(4),HLDREN        MARKET CHANGE                             
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
*                                                                               
         CLI   SRDPT,X'01'         IF FIRST DPT                                 
         BNE   SD3                                                              
*                                                                               
         L     R1,SRPNT                                                         
         M     R0,=F'10'                                                        
         L     RF,SRSPOT                                                        
         BAS   RE,DIVIDE                                                        
         L     RF,SLOT                                                          
         SLL   RF,2                                                             
         ST    R1,DP1PPS(RF)       PPS N.NN                                     
*                                                                               
         L     R1,SRIMP                                                         
         M     R0,=F'10'                                                        
         L     RF,SRSPOT                                                        
         BAS   RE,DIVIDE                                                        
         L     RF,SLOT                                                          
         SLL   RF,2                                                             
         ST    R1,DP1IPS(RF)       IPS N.N                                      
*                                                                               
*                                                                               
SD3      DS    0H                                                               
         MVC   HOLDREC,SRREC                                                    
*                                                                               
         CLC   SLOT,=F'1'          IF SLOT 1,SAVE PPS AND IPS                   
         BNZ   SD4                                                              
*                                                                               
         L     R1,SRPNT                                                         
         ST    R1,DM1PNTS                                                       
         M     R0,=F'10'                                                        
         L     RF,SRSPOT                                                        
         BAS   RE,DIVIDE                                                        
         ST    R1,DM1PPS           PPS N.NN                                     
*                                                                               
         L     R1,SRIMP                                                         
         ST    R1,DM1IMPS                                                       
         M     R0,=F'10'                                                        
         L     RF,SRSPOT                                                        
         BAS   RE,DIVIDE                                                        
         ST    R1,DM1IPS           IPS N.N                                      
*                                                                               
SD4      DS    0H                                                               
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
*                                                                               
         CLI   HLDDPT,X'01'        PRIME                                        
         BNE   *+14                                                             
         MVC   P(3),=C'PRI'                                                     
         B     CPMRA2A                                                          
         CLI   HLDDPT,X'FF'        ALL                                          
         BNE   *+14                                                             
         MVC   P(3),=C'ALL'                                                     
         B     CPMRA2A                                                          
*                                                                               
CPMRA1   DS    0H                                                               
         L     RE,DAYTABL                                                       
CPMRA1A  CLI   0(RE),X'FF'                                                      
         BNE   CPMRA1B                                                          
         MVC   P(3),=C'ZZZ'                                                     
         B     CPMRA2A                                                          
CPMRA1B  CLC   0(1,RE),HLDDPT                                                   
         BE    CPMRA2                                                           
         LA    RE,9(RE)                                                         
         B     CPMRA1A                                                          
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
         CLI   0(R5),C'1'          IF LESS THAN 1                               
         BNL   *+8                                                              
         LA    RF,9(RF)            ALPHA = 10-18                                
         BCTR  RF,0                                                             
         SLL   RF,2                X 4                                          
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
         B     EPPSCF              A. PPS -CONV FACTOR                          
         B     EIPSCF              B. IPS- CONV FACTOR                          
         B     EPPSDI              C. PPS -DPT INDEX                            
         B     EIPSDI              D. IPS- DPT INDEX                            
         B     EUNIV               E. UNIVERSES                                 
         B     EVPHM               F. VPH MARKET                                
         B     EVPHT               G. VPH TSA                                   
         B     EVPHI               H. VPH INDEX                                 
         EJECT                                                                  
ECPP     MVC   PCAP,=C'CPP  '      EDIT CPP                                     
         BAS   R9,ESET                                                          
ECPP1    L     RF,DPDDOLE                                                       
         OC    DPDDOLE,DPDDOLE                                                  
         BZ    ECPP2                                                            
         MH    RF,=H'10000'        UP TO UNITS                                  
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
         MH    RF,=H'1000'         UP TO UNITS                                  
         ST    RF,DUB                                                           
         MVC   DUB+4(4),DPDIMP                                                  
         BAS   R9,DIV2                                                          
         BAS   R9,NXTSLOT                                                       
         B     ECPM1                                                            
         SPACE 2                                                                
ECOST    MVC   PCAP,=C'$-000'                                                   
         BAS   R9,ESET                                                          
ECOST1   MVC   DUB(4),DPDDOL                                                    
         MVC   DUB+4(4),=F'1'                                                   
         BAS   R9,DIV2                                                          
         CLC   DPDDOL,DPDDOLE                                                   
         BE    ECOST2                                                           
         L     R9,PLADDR                                                        
         LA    R9,132(R9)                                                       
         MVI   LIN2SW,1                                                         
         MVC   8(5,R9),=C'E$000'                                                
         MVC   DUB(4),DPDDOLE                                                   
         MVC   DUB+4(4),=F'1'                                                   
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
         AH    R6,=H'3'                                                         
         B     EIMPS4                                                           
EIMPS3   EDIT  DPDIMP,(8,129(R6))  EDIT FOR SECOND LINE                         
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
         MH    RE,=H'10'           UP TO UNITS                                  
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,DPDSPT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(5,(R6))                                                    
ECPS2    BAS   R9,NXTSLOT                                                       
         B     ECPS1                                                            
         SPACE 2                                                                
EPPS     MVC   PCAP,=C'PPS  '                                                   
         BAS   R9,ESET                                                          
EPPS1    OC    DPDSPT,DPDSPT                                                    
         BZ    EPPS2                                                            
         L     RE,DPDPNT                                                        
         MH    RE,=H'10'                                                        
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,DPDSPT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(5,(R6)),2                                                  
EPPS2    BAS   R9,NXTSLOT                                                       
         MVC   DUB+4(4),=F'10'                                                  
         B     EPPS1                                                            
         SPACE 2                                                                
EIPS     MVC   PCAP,=C'IPS  '                                                   
         BAS   R9,ESET                                                          
EIPS1    OC    DPDSPT,DPDSPT                                                    
         BZ    EIPS2                                                            
         L     RE,DPDIMP                                                        
         MH    RE,=H'10'                                                        
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,DPDSPT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(5,(R6)),1                                                  
EIPS2    BAS   R9,NXTSLOT                                                       
         B     EIPS1                                                            
         SPACE 2                                                                
EPPSCF   MVC   PCAP,=C'PPSCF'      PPS CONVERSION FACTOR                        
         BAS   R9,ESET                                                          
EPPSCF1  L     R1,DPDPNT           POINTS                                       
         M     R0,=F'10'                                                        
         L     RF,DPDSPT           / SPOTS                                      
         BAS   RE,DIVIDE           = PPS                                        
         M     R0,=F'1000'                                                      
         L     RF,DM1PPS           / DEMO1 PPS                                  
         BAS   RE,DIVIDE                                                        
         EDIT  (R1),(5,(R6)),3,ZERO=BLANK                                       
         BAS   R9,NXTSLOT                                                       
         B     EPPSCF1                                                          
         SPACE 2                                                                
EIPSCF   MVC   PCAP,=C'IPSCF'      IPS CONVERSION FACTOR                        
         BAS   R9,ESET                                                          
EIPSCF1  L     R1,DPDIMP           IMPS                                         
         M     R0,=F'10'                                                        
         L     RF,DPDSPT           / SPOTS                                      
         BAS   RE,DIVIDE           = IPS                                        
         M     R0,=F'1000'                                                      
         L     RF,DM1IPS           / DEMO1 IPS                                  
         BAS   RE,DIVIDE                                                        
         EDIT  (R1),(5,(R6)),3,ZERO=BLANK                                       
         BAS   R9,NXTSLOT                                                       
         B     EIPSCF1                                                          
         SPACE 2                                                                
EPPSDI   MVC   PCAP,=C'PPSDI'      PPS DAYPART INDEX                            
         BAS   R9,ESET                                                          
         SR    R3,R3                                                            
EPPSDI1  L     R1,DPDPNT           IMPS                                         
         M     R0,=F'10'                                                        
         L     RF,DPDSPT           / SPOTS                                      
         BAS   RE,DIVIDE           = IPS                                        
         M     R0,=F'1000'                                                      
         L     RF,DP1PPS(R3)       / DEMO1 IPS                                  
         BAS   RE,DIVIDE                                                        
         EDIT  (R1),(5,(R6)),3,ZERO=BLANK                                       
         BAS   R9,NXTSLOT                                                       
         LA    R3,4(R3)                                                         
         B     EPPSDI1                                                          
         SPACE 2                                                                
EIPSDI   MVC   PCAP,=C'IPSDI'      IPS DAYPART INDEX                            
         BAS   R9,ESET                                                          
         SR    R3,R3                                                            
EIPSDI1  L     R1,DPDIMP           IMPS                                         
         M     R0,=F'10'                                                        
         L     RF,DPDSPT           / SPOTS                                      
         BAS   RE,DIVIDE           = IPS                                        
         M     R0,=F'1000'                                                      
         L     RF,DP1IPS(R3)       / DEMO1 IPS                                  
         BAS   RE,DIVIDE                                                        
         EDIT  (R1),(5,(R6)),3,ZERO=BLANK                                       
         BAS   R9,NXTSLOT                                                       
         LA    R3,4(R3)                                                         
         B     EIPSDI1                                                          
         SPACE 2                                                                
EUNIV    DS    0H                                                               
         CLI   HLDDPT,1            ONLY FOR PRIME                               
         BNE   EDITX                                                            
         OC    HLDMKT,HLDMKT       AND REAL MARKETS                             
         BZ    EDITX                                                            
*                                                                               
EUNIVA   DS    0H                                                               
         L     RF,ADBLOCK                                                       
         USING DBLOCK,RF                                                        
         XC    0(256,RF),0(RF)     CLEAR THE DBLOCK                             
         MVC   DBFILE,=C'TP '      SET TO READ MARKET LEVEL RECORDS             
         MVI   DBSELMED,C'T'                                                    
         MVI   DBSELSRC,C'N'       **RTG SERVICE**                              
         MVC   DBSELBK,=X'550B'                                                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBAREC,ADEMREC                                                   
         MVC   DBSELRMK,HLDMKT                                                  
         SR    R1,R1               CPP MARKETS ARE 400 TO HIGH                  
         ICM   R1,3,DBSELRMK                                                    
         SH    R1,=H'400'                                                       
         STCM  R1,3,DBSELRMK                                                    
         MVC   DBSELAGY,=C'SJ'                                                  
         MVI   DBSELDAY,X'40'      SET TO READ MON 5-515P                       
         MVC   DBSELTIM(2),=H'1700'                                             
         MVC   DBSELTIM+2(2),=H'1715'                                           
         MVI   DBFUNCT,DBGETTOT                                                 
         L     RE,UTL              SAVE CURRENT UTL                             
         MVC   SVUTL,0(RE)                                                      
         MVI   4(RE),X'02'                                                      
         L     R9,ACOMFACS                                                      
         USING COMFACSD,R9                                                      
         STM   RA,RC,SVURA                                                      
         GOTO1 CDEMAND,DMCB,ADBLOCK,SVUNIV                                      
         L     RE,UTL              RESTORE THE UTL                              
         MVC   0(8,RE),SVUTL                                                    
*                                                                               
         MVC   PCAP,=C'U000 '      UNIVERSES                                    
         BAS   R9,ESET                                                          
         SR    R3,R3                                                            
EUNIV1   DS    0H                                                               
         L     R1,UNIVS(R3)       THIS USIV                                     
         EDIT  (R1),(5,(R6))                                                    
         BAS   R9,NXTSLOT                                                       
         LA    R3,4(R3)                                                         
         B     EUNIV1                                                           
         SPACE 2                                                                
EVPHM    MVC   PCAP,=C'VPHM '      VPH MARKET                                   
         BAS   R9,ESET                                                          
         SR    R3,R3                                                            
EVPHM1   L     R1,DPDPNT           POINTS                                       
         M     R0,UNIVS(R3)        X DEMO UNIV                                  
         M     R0,=F'10000'                                                     
         L     RF,DM1PNTS          / (HH PNTS                                   
         M     RE,UNIVS+4              X HH UNIV)                               
         BAS   RE,DIVIDE            = VPHM                                      
         ST    R1,VPHMS(R3)        SAVE                                         
         L     RF,=F'10'                                                        
         M     R0,=F'1'                                                         
         BAS   RE,DIVIDE                                                        
         EDIT  (R1),(5,(R6)),3,ZERO=BLANK                                       
         BAS   R9,NXTSLOT                                                       
         LA    R3,4(R3)                                                         
         B     EVPHM1                                                           
         SPACE 2                                                                
EVPHT    MVC   PCAP,=C'VPHT '      VPH TSA                                      
         BAS   R9,ESET                                                          
         SR    R3,R3                                                            
EVPHT1   L     R1,DPDIMP           IMPS                                         
         M     R0,=F'10000'                                                     
         L     RF,DM1IMPS          / (HH IMPS                                   
         BAS   RE,DIVIDE            = VPHT                                      
         ST    R1,VPHTS(R3)        SAVE                                         
         M     R0,=F'1'                                                         
         L     RF,=F'10'                                                        
         BAS   RE,DIVIDE                                                        
         EDIT  (R1),(5,(R6)),3,ZERO=BLANK                                       
         BAS   R9,NXTSLOT                                                       
         LA    R3,4(R3)                                                         
         B     EVPHT1                                                           
         SPACE 2                                                                
EVPHI    MVC   PCAP,=C'VPHIX'      VPH INDEX                                    
         BAS   R9,ESET                                                          
         SR    R3,R3                                                            
EVPHI1   L     R1,VPHTS(R3)        VPH TSA                                      
         M     R0,=F'100'                                                       
         L     RF,VPHMS(R3)        / VPM                                        
         BAS   RE,DIVIDE            = INDEX                                     
         EDIT  (R1),(5,(R6)),2,ZERO=BLANK                                       
         BAS   R9,NXTSLOT                                                       
         LA    R3,4(R3)                                                         
         B     EVPHI1                                                           
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
         USING *,RF                                                             
SVUNIV   NTR1  BASE=SVURB                                                       
         L     RA,SVURA                                                         
         L     RC,SVURC                                                         
         DROP  RF                                                               
*                                                                               
         L     RF,ADBLOCK          SET TO GET UNIVERSE                          
         L     R9,ACOMFACS                                                      
         USING COMFACSD,R9                                                      
         L     R3,ADEMLST                                                       
         ST    R3,DMCB                                                          
         GOTO1 CDEMOUT,DMCB,(C'L',(R3)),ADBLOCK,X                               
*                                                                               
         B     SVU8              **SKIP DIVIDE                                  
         LA    R3,UNIVS                                                         
         LA    R4,DEMN                                                          
SVU6     DS    0H                                                               
         L     R1,0(R3)                                                         
         M     R0,=F'1'                                                         
         L     RF,=F'100'                                                       
         BAS   RE,DIVIDE                                                        
         ST    R1,0(R3)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SVU6                                                          
*                                                                               
SVU8     DS    0H                  SET IN UNIVS                                 
         XC    UNIVS,UNIVS                                                      
         LA    R3,X                                                             
         LA    R4,UNIVS+4          EVERY OTHER POSITION                         
         LA    RF,DEMN                                                          
*                                                                               
SVU9     DS    0H                                                               
         MVC   0(4,R4),0(R3)                                                    
         LA    R4,8(R4)                                                         
         LA    R3,4(R3)                                                         
         BCT   RF,SVU9                                                          
*                                                                               
         DROP  R9                                                               
SVUX     DS    0H                                                               
         XIT                                                                    
*                                                                               
SVURA    DS    F                                                                
SVURB    DS    F                                                                
SVURC    DS    F                                                                
*                                                                               
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
         BR    R9                                                               
DIV21    SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
DIV12    C     RF,=F'9999'                                                      
         BH    DIV22                                                            
         EDIT  (RF),(5,(R6)),1                                                  
         BR    R9                                                               
DIV22    SR    RE,RE                                                            
         SLA   RF,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         C     RF,=F'99999'                                                     
         BH    DIV23                                                            
         EDIT  (RF),(5,(R6))                                                    
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
NXTSLOT  LA    R6,6(R6)                                                         
         LA    R4,32(R4)                                                        
         BCT   R5,*+8                                                           
         B     EDITX                                                            
         BR    R9                                                               
         SPACE 2                                                                
DIVIDE   DIV   (R0),(RF)           RETURNS VIA RE                               
         SPACE 2                                                                
EDITX    DS    0H                                                               
         L     RE,PLADDR                                                        
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         MVI   0(RE),0             ENSURE LINE PRINTS                           
         LA    RE,132(RE)                                                       
         CLI   LIN2SW,0                                                         
         BE    EDITX2                                                           
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         MVI   0(RE),0             ENSURE LINE PRINTS                           
         LA    RE,132(RE)                                                       
EDITX2   DS    0H                                                               
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
         CLC   SRREN(4),HLDREN                                                  
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLC   SRMKT,HLDMKT        MKT CHANGE                                   
         BE    RPTD2                                                            
         MVI   FORCEHED,C'Y'                                                    
         XC    DP1PPS,DP1PPS                                                    
         XC    DP1IPS,DP1IPS                                                    
RPTD2    DS    0H                                                               
         MVC   HOLDREC,SRREC                                                    
         CLI   SRTGT,0             EOF                                          
         BE    EXIT2                                                            
         B     SD1                                                              
*                                                                               
EXIT2    DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*HEADLINES ROUTINES                                                             
         DS    0D                                                               
MYHEAD   CSECT                                                                  
         NMOD1 0,MYHEAD                                                         
         L     R2,CP20R2                                                        
         L     RA,CP20RA                                                        
         L     RC,CP20RC                                                        
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
MKTLST   CSECT                                                                  
         DC    CL5'ATLAN'                                                       
         DC    CL5'CHICA'                                                       
         DC    CL5'DALLA'                                                       
         DC    CL5'DETRO'                                                       
         DC    CL5'LOS A'                                                       
         DC    CL5'MIAMI'                                                       
         DC    CL5'NEW Y'                                                       
         DC    CL5'PHILA'                                                       
         DC    CL5'SAN F'                                                       
         DC    CL5'WASHI'                                                       
         DC    CL5'BUFFA'                                                       
         DC    CL5'CHARL'                                                       
         DC    CL5'COLUM'                                                       
         DC    CL5'DENVE'                                                       
         DC    CL5'ORLAN'                                                       
         DC    CL5'ALBAN'                                                       
         DC    CL5'ALBUQ'                                                       
         DC    CL5'FLINT'                                                       
         DC    CL5'FRESN'                                                       
         DC    CL5'PROVI'                                                       
         DC    CL5'BAKER'                                                       
         DC    CL5'BURLI'                                                       
         DC    CL5'EL PA'                                                       
         DC    CL5'TALLA'                                                       
         DC    CL5'TERRE'                                                       
*                                                                               
         DC    CL1'*',AL2(001,010)                                              
         DC    CL1'*',AL2(011,020)                                              
         DC    CL1'*',AL2(031,040)                                              
         DC    CL1'*',AL2(051,100)                                              
         DC    CL1'*',AL2(001,999)                                              
*                                                                               
         DC    X'FFFF'                                                          
         EJECT                                                                  
DEMGRP   CSECT                                                                  
DEMGRPL  EQU   8                                                                
         DC    AL1(001,042,048,092,142,148,000,000)                             
         DC    X'FF'                                                            
DEMLST   DS    0X                                                               
         DC    X'00',C'U',AL1(001)                                              
         DC    X'00',C'U',AL1(042)                                              
         DC    X'00',C'U',AL1(048)                                              
         DC    X'00',C'U',AL1(092)                                              
         DC    X'00',C'U',AL1(142)                                              
         DC    X'00',C'U',AL1(148)                                              
DEMN     EQU   (*-DEMLST)/3                                                     
*                                                                               
         DC    3X'FF'                                                           
*                                                                               
         DS    0D                                                               
DBLOCKC  DS    256X                                                             
DEMREC   DS    8000X                                                            
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
DM1PPS   DS    F                   FIRST DEMO PPSS                              
DM1IPS   DS    F                   FIRST DEMO IPS                               
DM1PNTS  DS    F                   FIRST DEMO PNTS                              
DM1IMPS  DS    F                   FIRST DEMO IMPS                              
DP1PPS   DS    XL80                FIRST DPT PPS'S (20 DEMOS)                   
DP1IPS   DS    XL80                FIRST DPT IPS (20 DEMOS)                     
UNIVS    DS    XL80                UNIVERSES                                    
VPHMS    DS    XL80                VPH'S (MARKET)                               
VPHTS    DS    XL80                VPH'S (TSA)                                  
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
PCAP     DS    CL5                                                              
EOFSW    DS    CL1                                                              
ACTSW    DS    CL1                                                              
SVSLN    DS    CL1                                                              
SVRTRN   DC    F'0'                                                             
SVUTL    DS    XL8                                                              
ADBLOCK  DS    A                                                                
ADEMREC  DS    A                                                                
ADEMLST  DS    A                                                                
MYH9     DS    CL96                                                             
MYH10    DS    CL96                                                             
MYH11    DS    CL96                                                             
         DS    0F                                                               
X        DS    XL256                                                            
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
       ++INCLUDE CPREPWORKD                                                     
       ++INCLUDE CPREPMODES                                                     
       ++INCLUDE CPGENDPLUT                                                     
       ++INCLUDE CPGENFILE                                                      
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135CPREP7202 05/01/02'                                      
         END                                                                    
