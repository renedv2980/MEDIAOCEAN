*          DATA SET CPREP2202  AT LEVEL 053 AS OF 05/01/02                      
*PHASE CP2202A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'CPREP2202-COST PER POINT TRENDS'                                
         PRINT NOGEN                                                            
CP2202   CSECT                                                                  
         NMOD1 0,CP2202,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING CPWORKD,RA,RC                                                    
         L     R2,=V(CP22WK)                                                    
         USING CP22WK,R2                                                        
         AR    R2,R5                                                            
         ST    R5,RELO                                                          
         ST    R2,CP22R2                                                        
         STM   RA,RC,CP22RA                                                     
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   BK10                                                             
         MVI   FCMONTH,C'Y'                                                     
         LA    RE,MYHEAD                                                        
         ST    RE,HEADHOOK                                                      
         B     EXIT                                                             
         SPACE 2                                                                
BK10     CLI   MODE,REQFRST                                                     
         BNE   BK20                                                             
         L     R6,=V(DPLUTAB)                                                   
         A     R6,RELO                                                          
         ST    R6,DAYTABL                                                       
         MVI   FCMONTH,C'Y'                                                     
         MVI   KTARGET,X'FF'                                                    
         CLC   QTARGET,=C'   '                                                  
         BE    BK10A1                                                           
         PACK  DUB,QTARGET                                                      
         CVB   RF,DUB                                                           
         STC   RF,KTARGET                                                       
BK10A1   CLI   ACTSW,2             SORT ALREADY OPEN                            
         BE    BK10AA                                                           
         L     R6,=V(SORTER)                                                    
         A     R6,RELO                                                          
         ST    R6,VSORTER                                                       
         GOTO1 VSORTER,DMCB,SORT,RECCARD,0                                      
BK10AA   MVI   ACTSW,0                                                          
         L     R6,=V(DPAREAC)                                                   
         A     R6,RELO                                                          
         ST    R6,DPAREA                                                        
         MVI   HAVDATA,0                                                        
         LA    R7,9                                                             
         LA    R6,QDATATYP                                                      
         LA    RE,4                                                             
BK10A    CLI   0(R6),C' '                                                       
         BE    BK10B                                                            
         LA    RE,1(RE)                                                         
         LA    R6,1(R6)                                                         
         BCT   R7,BK10A                                                         
BK10B    STC   RE,NUMDATA                                                       
         LA    RE,3(RE)            ADJUST ALLOWLIN FOR PRINT BLOCK              
         STC   RE,SVALLOWL                                                      
         B     EXIT                                                             
         SPACE 2                                                                
         EJECT                                                                  
BK30     CLI   MODE,PROCDATA                                                    
         BNE   BK40                                                             
         L     R3,ADDATA                                                        
         USING CPKEYD,R3                                                        
         MVI   ACTSW,1                                                          
         SPACE 2                                                                
BK304    XC    SRREC,SRREC         BUILD PROTOTYPE RECORD                       
         MVC   SRRST,MCOUNT+2                                                   
         MVC   SRTGT,CPKTARGT                                                   
         CLC   QTARGET,=C'129'                                                  
         BL    *+10                                                             
         MVC   SRTGT,KTARGET                                                    
         CLC   QTARGET,=C'   '                                                  
         BNE   *+10                                                             
         MVC   SRTGT,KTARGET                                                    
         MVC   SRDEMO,CPKDEMO                                                   
         MVC   SRDPT,CPKDAYPT                                                   
         MVC   SRSLN,CPKSPTLN                                                   
         MVC   SRMKT,CPKMKT                                                     
         MVC   SRWGHT+2(2),MKTWT                                                
         OC    SRWGHT,SRWGHT                                                    
         BNZ   *+10                                                             
         MVC   SRWGHT,=F'1'                                                     
         MVC   SRMKT2,CPKMKT                                                    
         LA    R4,PERTABLE                                                      
         USING CPERD,R4                                                         
         XC    CURRPER,CURRPER                                                  
BK20C    ZIC   RE,CURRPER                                                       
         MH    RE,WIDTHPER+2                                                    
         LA    R4,PERTABLE(RE)                                                  
         MVC   SRDOL,CPCASH                                                     
         MVC   SRSPOT,CPSPOTS                                                   
         MVC   SRPNT,CPOINTS                                                    
         MVC   SRIMP,CPIMPS                                                     
         MVC   SRSLOT,CURRPER                                                   
         OC    SRSPOT,SRSPOT                                                    
         BZ    PUTSORT1                                                         
         SPACE 2                                                                
         L     RE,SRDOL            EQUIVALENCE DOLLARS                          
         MH    RE,=H'1000'                                                      
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
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
         CLI   QOPT2,C'Y'          SUPPRESS SPOT LENGTH                         
         BNE   SUPSLX                                                           
         MVC   SRDOLE,SVDOLE                                                    
         MVI   SRSLN,X'FF'                                                      
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
PUTSORT1 ZIC   RE,CURRPER                                                       
         LA    RE,1(RE)                                                         
         STC   RE,CURRPER                                                       
         C     RE,NPERIODS                                                      
         BL    BK20C                                                            
         B     EXIT                                                             
         SPACE 2                                                                
PSOK     CLC   QUESTOR(6),=C'ZTRACE'                                            
         BNE   PSOK1                                                            
         GOTO1 HEXOUT,DMCB,SRREC,P,48,0,0                                       
         GOTO1 REPORT                                                           
PSOK1    GOTO1 VSORTER,DMCB,=C'PUT',SRREC                                       
         MVI   HAVDATA,1                                                        
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
BK40     CLI   QRANGE,C'C'                                                      
         BNE   BK401                                                            
         CLI   MODE,CLTLAST                                                     
         BE    BK40A                                                            
         B     EXIT                                                             
BK401    CLI   MODE,REQLAST                                                     
         BNE   BK50                                                             
BK40A    CLI   HAVDATA,1                                                        
         BNE   BK40B                                                            
         MVI   HAVDATA,0                                                        
         MVI   FORCEMKT,C'Y'                                                    
         MVI   FORCEHED,C'Y'                                                    
         XC    HLD2REC,HLD2REC                                                  
         XC    HOLDREC,HOLDREC                                                  
         L     RE,DPAREA                                                        
         L     RF,=F'500'                                                       
         XCEF                                                                   
         MVI   CNT63,0                                                          
         MVI   EOFSW,0                                                          
         CLI   ACTSW,0                                                          
         BNE   PR1                                                              
         GOTO1 VSORTER,DMCB,=C'END',SRREC                                       
         MVI   ACTSW,0                                                          
         B     EXIT                                                             
         SPACE 2                                                                
BK40B    MVI   ACTSW,2             SET TO SORT OPEN ALREADY                     
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
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     RE,4(R1)                                                         
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   PR3                                                              
         GOTO1 VSORTER,DMCB,=C'END',SRREC                                       
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
         LA    R9,4                                                             
         LA    R9,6                                                             
         LA    R1,0                                                             
PR4      L     R8,0(R1,RE)                                                      
         A     R8,0(R1,RF)                                                      
         ST    R8,0(R1,RF)                                                      
         LA    R1,4(R1)                                                         
         BCT   R9,PR4                                                           
         B     PR1                                                              
         SPACE 2                                                                
PRX      CLC   SRREN,HIGROUP       BYPASS HIGH GROUPS                           
         BH    PR1                                                              
         CLC   QUESTOR,=C'ZTRACE'                                               
         BNE   SD1                                                              
         GOTO1 HEXOUT,DMCB,SRREC,P,36                                           
         GOTO1 REPORT                                                           
         EJECT                                                                  
* SUM RECORDS INTO TABLE IF DETAIL ITEM                                         
SD1      DS    0H                                                               
         OC    HOLDREC,HOLDREC                                                  
         BZ    SD2                                                              
         CLC   SRRST,HLDRST        MARKET CHANGE                                
         BNE   CPMR                 DO - REPORT                                 
         CLC   SRREN,HLDREN                                                     
         BNE   CPMR                                                             
         CLC   SRDPT(2),HLDDPT     DAYPART/SLN CHANGE                           
         BNE   CPMR                 DO - REPORT                                 
SD2      ZIC   RE,SRSLOT           SET PROPER SLOT                              
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
*PRINT REPORT                                                                   
CPMR     CLI   FORCEMKT,C'Y'                                                    
         BNE   CPMRX                                                            
         MVI   FORCEMKT,C'N'                                                    
         OC    HLDMKT,HLDMKT                                                    
         BZ    CPMRC                                                            
         CLI   NUMDATA,3                                                        
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         L     RE,ADMKTTAB         GET MARKET NAME                              
CPMRA    CLC   HLDMKT,0(RE)                                                     
         BE    CPMRB                                                            
         CLI   0(RE),X'FF'                                                      
         BE    CPMRX                                                            
         LA    RE,34(RE)                                                        
         B     CPMRA                                                            
CPMRB    MVC   HLDMNAME,6(RE)                                                   
         MVC   P(24),HLDMNAME                                                   
         B     CPMRD                                                            
         SPACE 2                                                                
CPMRC    DS    0H                                                               
         CLI   NUMDATA,3                                                        
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(12),=C'MARKET GROUP'                                           
         MVC   HALF,HLDRST                                                      
         XC    HALF,=X'FFFF'                                                    
         EDIT  HALF,(3,P+13)                                                    
         MVI   P+16,C'-'                                                        
         EDIT  HLDREN,(3,P+17),,ALIGN LEFT                                      
CPMRD    MVC   SVMNAME(24),P                                                    
         CLI   NUMDATA,3                                                        
         BL    *+10                                                             
         XC    P(24),P                                                          
CPMRX    CLC   SRDPT(2),HLDDPT                                                  
         BE    CPMR1                                                            
         CLI   HLDSLN,30                                                        
         BE    *+8                                                              
         CLI   HLDSLN,60                                                        
         BNE   CPMA1                                                            
         ZIC   RE,CNT63                                                         
         LA    RE,1(RE)                                                         
         STC   RE,CNT63                                                         
CPMA1    CLI   HLDSLN,63                                                        
         BNE   CPMA2                                                            
         CLI   CNT63,1                                                          
         BE    RPTDONE2                                                         
CPMA2    DS    0H                                                               
         CLI   NUMDATA,1                                                        
         BE    *+12                                                             
         MVI   MID1,0                                                           
         MVI   FORCEMID,C'Y'                                                    
         L     RE,DAYTABL          GET DAYPART                                  
CPMR01   CLI   0(RE),X'FF'                                                      
         BNE   CPMR01A                                                          
         MVC   P+25(3),=C'ZZZ'                                                  
         B     CPMR02A                                                          
CPMR01A  CLC   0(1,RE),HLDDPT                                                   
         BE    CPMR02                                                           
         LA    RE,9(RE)                                                         
         B     CPMR01                                                           
CPMR02   MVC   P+25(3),5(RE)                                                    
CPMR02A  MVI   P+28,C'-'                                                        
         EDIT  HLDSLN,(3,P+29),,ALIGN=LEFT                                      
         CLI   HLDSLN,63                                                        
         BNE   *+10                                                             
         MVC   P+29(3),=C'3+6'                                                  
         CLI   HLDSLN,X'FF'                                                     
         BNE   *+10                                                             
         MVC   P+29(3),=C'   '                                                  
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
         MVC   33(5,R9),=C'E$000'                                               
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
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,DPDSPT                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(5,(R6)),1                                                  
EPPS2    BAS   R9,NXTSLOT                                                       
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
EIPS2    BAS   R9,NXTSLOT                                                       
         B     EIPS1                                                            
         EJECT                                                                  
* SET UP VARIOUS EDITS                                                          
ESET     L     R6,PLADDR                                                        
         MVI   LIN2SW,0                                                         
         LA    R6,33(R6)                                                        
         MVC   0(5,R6),PCAP        MOVE IN TITLE                                
         LA    R6,6(R6)                                                         
         L     R4,DPAREA                                                        
         USING DPD,R4                                                           
         LA    R5,12                                                            
         BR    R9                                                               
UNWEIGHT L     RE,DPDPNTW                                                       
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
EDITX    L     RE,PLADDR                                                        
         LA    RE,132(RE)                                                       
         CLI   LIN2SW,0                                                         
         BE    *+8                                                              
         LA    RE,132(RE)                                                       
         ST    RE,PLADDR                                                        
         B     CPMDR2                                                           
         EJECT                                                                  
RPTDONE  MVC   ALLOWLIN,SVALLOWL                                                
         ZIC   RE,LINE              FORCE TO SKIP A PAGE                        
         ZIC   RF,SVALLOWL                                                      
         AR    RE,RF                                                            
         ZIC   RF,MAXLINES                                                      
         CR    RE,RF                                                            
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
RPTDONE2 L     RE,DPAREA                                                        
         L     RF,=F'512'                                                       
         XCEF                                                                   
         CLI   HLDSLN,63                                                        
         BNE   *+8                                                              
         MVI   CNT63,0                                                          
         CLC   SRREN,HLDREN                                                     
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLC   SRRST,HLDRST                                                     
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLC   SRMKT,HLDMKT                                                     
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLI   FORCEHED,C'Y'                                                    
         BNE   *+8                                                              
         MVI   FORCEMKT,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         MVC   HOLDREC,SRREC                                                    
         CLI   SRTGT,0             EOF                                          
         BE    EXIT                                                             
         B     SD1                                                              
         EJECT                                                                  
*HEADLINES ROUTINES                                                             
         DS    0D                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=CP22RB                                                      
         DROP  RF                                                               
         L     R2,CP22R2                                                        
         LM    RA,RC,CP22RA                                                     
         MVC   P(24),SVMNAME                                                    
         LA    R5,12                                                            
         LA    R6,H8+39                                                         
         LA    R4,PERTABLE                                                      
         USING CPERD,R4                                                         
MYHEAD1  MVC   0(5,R6),CPSTART                                                  
         MVC   132(5,R6),=C'-----'                                              
         CLC   CPSTARTB,CPENDB                                                  
         BE    *+10                                                             
         MVC   132(5,R6),CPEND                                                  
         LA    R6,6(R6)                                                         
         A     R4,WIDTHPER                                                      
         OC    CPSTART,CPSTART                                                  
         BZ    MYHEAD2                                                          
         BCT   R5,MYHEAD1                                                       
LIN2SW   DS    C                                                                
         SPACE 2                                                                
MYHEAD2  L     RF,ADDEMBUF         PRINT DEMO NAMES                             
         MVC   H5+41(8),=C'UNIVERSE'                                            
         MVC   H5+61(12),=C'TARGETS ONLY'                                       
MYHEAD3  SR    R9,R9                                                            
         IC    R9,HLDTGT                                                        
         BCTR  R9,0                                                             
         MH    R9,=H'7'                                                         
         AR    R9,RF                                                            
         MVC   H5+53(3),=C'ALL'                                                 
         CLI   HLDTGT,X'FF'                                                     
         BE    *+10                                                             
         MVC   H5+53(7),0(R9)                                                   
         MVC   H6+41(11),=C'REPORT DEMO'                                        
         SR    R9,R9                                                            
         IC    R9,HLDDEMO                                                       
         BCTR  R9,0                                                             
         MH    R9,=H'7'                                                         
         AR    R9,RF                                                            
         MVC   H6+53(7),0(R9)                                                   
         MVC   H7+43(12),=C'(WEIGHTED)  '                                       
         CLI   QOPT1,C'Y'                                                       
         BNE   *+10                                                             
         MVC   H7+43(12),=C'(UNWEIGHTED)'                                       
         SPACE 2                                                                
MYHEADX  XIT1                                                                   
CP22RA   DS    F                                                                
CP22RB   DS    F                                                                
CP22RC   DS    F                                                                
CP22R2   DS    F                                                                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
CP22WK   CSECT                                                                  
HLDMNAME DS    CL24                                                             
SVMNAME  DS    CL24                                                             
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
SRSLOT   DS    CL1                 DATE SLOT INDEX                              
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
MCOUNT   DC    F'0'                                                             
SVR5     DC    F'0'                                                             
HIGROUP  DC    H'0'                                                             
HAVDATA  DS    C                                                                
CURRPER  DS    CL1                                                              
MSEQ     DS    CL13                                                             
NUMDATA  DS    C                                                                
SLTDEM   DS    C                                                                
MENU     DS    CL1                                                              
RELO     DC    A(0)                                                             
VSORTER  DC    F'0'                                                             
SLOT     DC    F'0'                                                             
DPAREA   DC    F'0'                                                             
PLADDR   DS    F                                                                
PCAP     DS    CL5                                                              
EOFSW    DS    CL1                                                              
ACTSW    DS    CL1                                                              
SVALLOWL DS    CL1                                                              
SVDOLE   DS    F                                                                
SVSLN    DS    CL1                                                              
CNT63    DS    C                                                                
FORCEMKT DS    CL1                                                              
KTARGET  DS    C                                                                
SVRTRN   DC    F'0'                                                             
SORT     DC    C'SORT FIELDS=(1,24,BI,A,53,2,BI,A),WORK=1 '                     
RECCARD  DC    C'RECORD TYPE=F,LENGTH=54 '                                      
         EJECT                                                                  
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
**PAN#1  DC    CL21'053CPREP2202 05/01/02'                                      
         END                                                                    
