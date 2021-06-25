*          DATA SET SPREP4402  AT LEVEL 011 AS OF 09/19/11                      
*PHASE SP4402A                                                                  
*INCLUDE UNDAY                                                                  
*INCLUDE UNTIME                                                                 
*INCLUDE XSORT                                                                  
*                                                                               
* PWES 007 18OCT01 NETWORK- NEW CANADIAN CABLE FORMAT / R9 2ND BASE REG         
* TZIH 006 09OCT01                                                              
*                                                                               
         TITLE 'SP4402 - NETWORK/SHOW/SPILL/ LISTING'                           
         PRINT NOGEN                                                            
SP4402   CSECT                                                                  
         NMOD1 0,SP4402,RR=R5                                                   
*                                                                               
         ST    R5,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING SP4402+4096,R9                                                   
*                                                                               
         L     RA,0(R1)                                                         
         LA    R8,2048(RA)                                                      
         LA    R8,2048(R8)                                                      
         USING SPWORKD,RA,R8                                                    
*                                                                               
         MVC   RCSUBPRG,MODE                                                    
         CLI   MODE,PROCSPL                                                     
         BH    EXIT                                                             
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         GOTO1 GET                                                              
         CLI   MODE,PROCNET                                                     
         BE    NETW                                                             
         CLI   MODE,PROCSHW                                                     
         BE    SHOW                                                             
         CLI   MODE,PROCSPL                                                     
         BE    SPILL                                                            
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
NETW     DS    0H                                                               
* RECORD CAN BE EITHER ORIG NETDEF -OR- NEW MKTDEF/CBLDEF VERSION               
         OC    KEY+4(4),KEY+4                                                   
         BZ    EXIT                IGNORE THE 'ALL' MKTDEF RECORD               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   NETLINE,KEY+4                                                    
         XC    CLTLINE,CLTLINE                                                  
         XC    ESTLINE,ESTLINE                                                  
         XC    REGTAB((MAXREGNS*2+1)*4),REGTAB                                  
         XC    REGCNT,REGCNT                                                    
         XC    PCTTOT,PCTTOT                                                    
         CLI   KEY+10,0            CHK FOR ESTIMATE                             
         BE    NETW5                                                            
         ZIC   R0,KEY+10                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTLINE,DUB                                                      
*                                                                               
NETW5    OC    KEY+8(2),KEY+8                                                   
         BZ    NETW10                                                           
         GOTO1 CLUNPK,DMCB,KEY+8,CLTLINE                                        
*                                                                               
NETW10   MVI   CABLENET,C'N'        DETERMINE IF CANADIAN CABLE NETWORK         
         LA    R2,REC+24                                                        
         MVI   ELCODE,X'02'                                                     
         CLI   0(R2),X'02'                                                      
         BE    NETW12                                                           
         CLI   0(R2),0                                                          
         BE    NET1                                                             
         BAS   RE,NEXTEL                                                        
         BNE   NET1                                                             
NETW12   CLI   NDEFNET-NDEFEL02(R2),NDEFCABQ                                    
         BNE   *+8                                                              
         MVI   CABLENET,C'Y'                                                    
*                                                                               
NET1     DS    0H                                                               
         LA    R2,REC+24                                                        
         MVI   ELCODE,X'01'                                                     
         CLI   0(R2),X'01'                                                      
         BE    NET4                                                             
         CLI   0(R2),0                                                          
         BE    NET6                                                             
*                                                                               
         USING NDEFEL01,R2                                                      
NET2     BAS   RE,NEXTEL                                                        
         BNE   NET5                                                             
NET4     MVC   P+3(4),NDEFSTA                                                   
         CLI   CABLENET,C'Y'                                                    
         BNE   NET4AA                                                           
         MVC   P+1(4),REC+NDEFKNET-NDEFRECD                                     
         MVI   P+5,C'/'                                                         
         MVC   P+6(2),NDEFMSUF                                                  
NET4AA   MVC   P+15(4),NDEFRGN                                                  
         EDIT  (B2,NDEFOSET),(4,P+25),0,FLOAT=-,ZERO=BLANK                      
         CLC   NDEFPCT,=F'-1'                                                   
         BNE   NET4A                                                            
         MVC   P+42(2),=C'NB'      NOT BOUGHT                                   
         B     NET4A1                                                           
*                                                                               
NET4A    DS    0H                                                               
         EDIT  (B4,NDEFPCT),(7,P+37),3                                          
         L     R0,PCTTOT                                                        
         MVC   FULL,NDEFPCT                                                     
         A     R0,FULL                                                          
         ST    R0,PCTTOT                                                        
*                                  ADD PCT TO EACH REGION                       
NET4A1   LA    R4,NDEFRGN                                                       
         LA    R3,4                FOR BCT                                      
*                                                                               
NET4A2   CLI   0(R4),C' '                                                       
         BNH   NET4E                                                            
         LA    R5,REGTAB                                                        
NET4B    CLI   0(R5),0             END OF TABLE                                 
         BNE   NET4C               NO                                           
         LH    R0,REGCNT           COUNT REGIONS                                
         AHI   R0,1                                                             
*                                                                               
         CHI   R0,MAXREGNS                                                      
         BH    NET4BDIE            TO AVOID CLOBBERING REC                      
*                                                                               
         MVC   0(1,R5),0(R4)       ADD TO TABLE                                 
         STH   R0,REGCNT                                                        
         CLC   NDEFPCT,=F'-1'                                                   
         BE    NET4E               NOT BOUGHT - SKIP ADD                        
         MVC   4(4,R5),NDEFPCT                                                  
         B     NET4E                                                            
*                                                                               
NET4BDIE DC    H'0'                                                             
*                                                                               
NET4C    CLC   0(1,R5),0(R4)       TRY AND MATCH REGIONS                        
         BE    NET4D               FOUND                                        
         LA    R5,8(R5)            NEXT REGION                                  
         B     NET4B                                                            
*                                                                               
NET4D    DS    0H                                                               
         CLC   NDEFPCT,=F'-1'                                                   
         BE    NET4E               NOT BOUGHT                                   
         L     R0,4(R5)                                                         
         A     R0,FULL                                                          
         ST    R0,4(R5)            TOTAL FOR ALL STATION IN THIS REG            
*                                                                               
NET4E    CLI   CABLENET,C'Y'       SHOW MARKETS FOR MKTDEF/CBLDEF RECS          
         BNE   NET4F                                                            
         CLC   NDEFSTA,=C'ZZZZ'                                                 
         BE    NET4F               IGNORE DUMMY                                 
         MVC   P+50(7),=CL7'MARKET='                                            
         LH    R1,NDEFMNUM                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+57(4),DUB                                                      
*                                                                               
         MVC   SVKEY1,KEY                                                       
         MVC   KEY(17),=17C'0'     NOW WE NEED THE MARKET NAME                  
         MVI   KEY,C'M'            MARKET                                       
         MVC   KEY+1(1),QMED       MEDIA                                        
         UNPK  KEY+2(4),DUB        MARKET CODE                                  
         MVC   KEY+6(2),AGY                                                     
         L     R6,ADMARKET                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,STATION,KEY,(R6)                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   0(15,R6),KEY                                                     
         BE    *+14                                                             
         MVC   P+62(15),=C'*** UNKNOWN ***'                                     
         B     *+10                                                             
         MVC   P+62(24),18(R6)                                                  
         MVC   KEY,SVKEY1                                                       
*                                                                               
NET4F    LA    R4,1(R4)            NEXT REGION OF STATION                       
         BCT   R3,NET4A2                                                        
NET4G    BAS   RE,PRINTIT                                                       
         B     NET2                                                             
*                                                                               
NET5     BAS   RE,PRINTIT                                                       
         MVC   P+30(6),=C'TOTAL='                                               
         EDIT  PCTTOT,(7,P+37),3                                                
         BAS   RE,PRINTIT                                                       
*                                                                               
NET6     OC    CLTLINE,CLTLINE     SEE IF DOING CLIENT EXCEPTION                
         BNZ   NET12                                                            
         SR    R5,R5                                                            
         BAS   RE,PRINTIT       SKIP A LINE                                     
         MVC   P+3(18),=C'EXCEPTION CLIENTS-'                                   
         LA    R6,P+22                                                          
         MVC   KEY(13),REC                                                      
         GOTO1 HIGH                                                             
NET7     GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE                                                   
         BNE   NET10                                                            
         C     R5,MAXCLTS                                                       
         BNH   NET7B                                                            
         BCTR  R6,0                                                             
         MVI   0(R6),C' '          BLANK LAST COMMA                             
         BAS   RE,PRINTIT                                                       
         MVC   P+3(18),=C'EXCEPTION CLIENTS-'                                   
         LA    R6,P+22                                                          
         SR    R5,R5                                                            
*                                                                               
NET7B    GOTO1 CLUNPK,DMCB,KEY+8,0(R6)                                          
         LA    R5,1(R5)                                                         
         CLI   2(R6),C' '                                                       
         BE    NET7C                                                            
         MVI   3(R6),C','                                                       
         LA    R6,4(R6)                                                         
         B     NET8                                                             
*                                                                               
NET7C    MVI   2(R6),C','                                                       
         LA    R6,3(R6)                                                         
         B     NET8                                                             
*                                                                               
NET8     CLI   KEY+10,0                                                         
         BE    NET7                                                             
         BCTR  R6,0                                                             
         MVI   0(R6),C'/'       CHANGE , TO /                                   
         EDIT  (B1,KEY+10),(3,1(R6)),0,FILL=0                                   
         MVI   4(R6),C','                                                       
         LA    R6,5(R6)                                                         
         LA    R5,1(R5)          COUNT AS 2 CLIENTS                             
         B     NET7                                                             
*                                                                               
NET10    LTR   R5,R5                                                            
         BZ    NET15                                                            
         BCTR  R6,0                                                             
         MVI   0(R6),C' '        BLANK LAST COMMA                               
         BAS   RE,PRINTIT          LAST LINE                                    
         B     NET15                                                            
*                                                                               
NET12    DS    0H                I GET HERE IF DOING CLIENT EXCEPTION           
         OC    ESTLINE,ESTLINE   SEE IF DOING ESTIMATE EXCEPTION                
         BNZ   NET15             YES - JUST SHOW NETWORK                        
*                                  LIST ESTIMATE EXCEPTIONS                     
*                                  FOR THIS CLIENT                              
         SR    R5,R5                                                            
         BAS   RE,PRINTIT       SKIP A LINE                                     
         MVC   P+3(20),=C'EXCEPTION ESTIMATES-'                                 
         LA    R6,P+24                                                          
         MVC   KEY(13),REC                                                      
         GOTO1 HIGH                                                             
NET12C   GOTO1 SEQ                                                              
         CLC   KEY(10),KEYSAVE                                                  
         BNE   NET13               END OF CLIENT                                
         C     R5,MAXCLTS                                                       
         BNH   NET12E                                                           
         BCTR  R6,0                                                             
         MVI   0(R6),C' '          BLANK LAST COMMA                             
         BAS   RE,PRINTIT                                                       
         MVC   P+3(20),=C'EXCEPTION ESTIMATES-'                                 
         LA    R6,P+24                                                          
         SR    R5,R5                                                            
*                                                                               
NET12E   ZIC   R0,KEY+10                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R6),DUB                                                      
         LA    R5,1(R5)                                                         
         MVI   3(R6),C','                                                       
         LA    R6,4(R6)                                                         
         B     NET12C                                                           
*                                                                               
*                                                                               
NET13    LTR   R5,R5                                                            
         BZ    NET15                                                            
         BCTR  R6,0                                                             
         MVI   0(R6),C' '        BLANK LAST COMMA                               
         BAS   RE,PRINTIT          LAST LINE                                    
         B     NET15                                                            
*                                                                               
*                                  PRINT EACH REGION IN REGION TABLE            
NET15    DS    0H                                                               
         MVC   P,SPACES            CLEAR UNUSED EXECPTION LINE                  
         CLC   REGCNT,=H'0'                                                     
         BE    NETX                NO REGIONS                                   
         CLC   REGCNT,=H'1'                                                     
         BE    NET15A                                                           
         LH    R3,REGCNT                                                        
         GOTO1 XSORT,DMCB,(0,REGTAB),(R3),8,1,0                                 
NET15A   DS    0H                                                               
         LA    R3,REGTAB                                                        
NET16    CLI   0(R3),0             END OF TABLE                                 
         BE    NETX                                                             
*                                                                               
NET18    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   H4+17(6),=C'REGION'                                              
         MVC   H4+24(1),0(R3)                                                   
         XC    PCTTOT,PCTTOT                                                    
         MVI   ELCODE,X'01'                                                     
         LA    R2,REC+24                                                        
         CLI   0(R2),X'01'                                                      
         BE    NET22                                                            
         CLI   0(R2),0                                                          
         BE    NET50               NO ELEMS                                     
*                                                                               
NET20    BAS   RE,NEXTEL                                                        
         BNE   NET40               DONE                                         
NET22    LA    R4,NDEFRGN                                                       
         LA    R5,4                FOR BCT                                      
NET24    CLC   0(1,R4),0(R3)                                                    
         BE    NET25                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,NET24                                                         
         B     NET20                                                            
*                                                                               
NET25    MVC   P+3(4),NDEFSTA                                                   
         MVC   P+15(4),NDEFRGN                                                  
         EDIT  (B2,NDEFOSET),(4,P+25),0,FLOAT=-,ZERO=BLANK                      
         CLC   NDEFPCT,=F'-1'                                                   
         BNE   NET27                                                            
         MVC   P+42(2),=C'NB'      NOT BOUGHT                                   
         B     NET32                                                            
*                                                                               
NET27    DS    0H                                                               
         MVC   FULL,NDEFPCT                                                     
         SR    R6,R6                                                            
         L     R7,FULL                                                          
         LTR   R7,R7                                                            
         BZ    NET30           ZERO PCT                                         
         M     R6,=F'10000'                                                     
         LR    R6,R7                                                            
         SR    R7,R7                                                            
         SRDA  R6,31                                                            
         SR    R5,R5                                                            
         L     R4,4(R3)                                                         
         SRDA  R4,31                                                            
         D     R4,=F'10'                                                        
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         ST    R5,FULL                                                          
         LTR   R5,R5                                                            
         BZ    NET32                                                            
         D     R6,FULL                                                          
         LTR   R7,R7                                                            
         BM    *+8                                                              
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
         L     R0,PCTTOT                                                        
         AR    R0,R7                                                            
         ST    R0,PCTTOT                                                        
NET30    EDIT  (R7),(7,P+37),3                                                  
NET32    BAS   RE,PRINTIT                                                       
         B     NET20               GO DO NEXT ELEM                              
*                                                                               
NET40    BAS   RE,PRINTIT                                                       
         MVC   P+30(6),=C'TOTAL='                                               
         EDIT  PCTTOT,(7,P+37),3                                                
         BAS   RE,PRINTIT                                                       
*                                                                               
NET50    DS    0H                                                               
         LA    R3,8(R3)            NEXT REGION IN TABLE                         
         B     NET16                                                            
*                                                                               
NETX     MVC   P,SPACES                                                         
         B     EXIT                                                             
         EJECT                                                                  
SHOW     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   NETLINE,KEY+4                                                    
         LA    R2,REC+24                                                        
         CLI   0(R2),X'01'                                                      
         BE    SHW2                                                             
         DC    H'0'                BAD REC                                      
*                                                                               
         USING NPGMEL01,R2                                                      
SHW2     XC    PGMLINE,PGMLINE                                                  
         MVI   PNTSW,0                                                          
         MVC   PGMLINE(4),REC+8                                                 
         MVC   PGMLINE+6(17),NPGMPGM                                            
         GOTO1 =V(UNDAY),DMCB,NPGMDAY,PGMLINE+25,RR=RELO                        
         GOTO1 =V(UNTIME),DMCB,NPGMSTR,PGMLINE+31,RR=RELO                       
         MVC   PGMLINE+43(4),=C'DPT='                                           
         MVC   PGMLINE+47(1),NPGMDPT                                            
*                                                                               
         LA    R2,REC+24                                                        
         MVI   ELCODE,X'05'                                                     
SHW4     BAS   RE,NEXTEL                                                        
         BNE   SHWX                                                             
         MVI   PNTSW,1                                                          
         USING NPGMEL05,R2                                                      
         MVC   P+3(4),NPGMSTA                                                   
         OC    NPGMSDAY,NPGMSDAY                                                
         BZ    SHW6                                                             
*                                                                               
         EDIT  NPGMSDAY,(2,P+14),0,FLOAT=-                                      
*                                                                               
SHW6     CLC   NPGMSTIM,=C'NO'                                                  
         BNE   SHW8                                                             
         MVC   P+25(9),=C'NOT SHOWN'                                            
         B     SHW10                                                            
*                                                                               
SHW8     MVC   WORK(2),NPGMSTIM                                                 
         XC    WORK+2(2),WORK+2                                                 
         GOTO1 =V(UNTIME),DMCB,WORK,P+24,RR=RELO                                
*                                                                               
SHW10    MVC   P+35(1),NPGMSDPT                                                 
         BAS   RE,PRINTIT                                                       
*                                                                               
         B     SHW4                                                             
*                                                                               
SHWX     CLI   PNTSW,1                                                          
         BE    EXIT                                                             
         BAS   RE,PRINTIT          NO STATIONS PRINT HEADS                      
         B     EXIT                                                             
         EJECT                                                                  
SPILL    DS    0H                                                               
         SR    R7,R7                                                            
         SR    R5,R5                                                            
         LA    R3,XSORTTAB                                                      
*                                                                               
         MVC   P,SPACES                                                         
*                                                                               
         CLC   KEY+4(1),QSTART       CHECK FOR CHANGE OF R/S                    
         BE    SPL1                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   QSTART(1),KEY+4                                                  
         MVC   RATSRVC,=C'NSI'                                                  
         CLI   QSTART,C'1'                                                      
         BNE   SPL1                                                             
         MVC   RATSRVC,=C'ARB'                                                  
         L     RF,ADAGY                                                         
         USING AGYRECD,RF                                                       
         CLI   AGYPROF+7,C'C'                                                   
         BNE   SPL1                                                             
         MVC   RATSRVC,=C'BBM'                                                  
         DROP  RF                                                               
*                                                                               
SPL1     MVC   SVKEY1,KEY          BACKUP THE KEY                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'            PREPARE KEY TO READ STATION RECORD           
         MVC   KEY+1,QMED          MEDIA                                        
         MVC   KEY+2(5),SVKEY1+5   STATION                                      
         CLI   KEY+6,C' '                                                       
         BH    *+10                                                             
         MVC   KEY+6(1),QMED       <-- USE MED IF BLANK                         
         MVC   KEY+7(2),SVKEY1+2   AGY                                          
         GOTO1 CLUNPK,DMCB,SVKEY1+10,KEY+9      CLT                             
         MVC   KEY+12(3),=X'000000' FILL CHARS                                  
*                                                                               
         L     R6,ADSTAT                                                        
         OC    SVKEY1+10(2),SVKEY1+10                                           
         BZ    SPL11                                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,STATION,KEY,(R6)                             
         B     SPL12                                                            
SPL11    GOTO1 DATAMGR,DMCB,DMREAD,STATION,KEY,(R6)                             
*                                                                               
SPL12    CLC   KEY(9),0(R6)                                                     
         BE    SPL1A                                                            
         MVC   P+19(23),=C'** STATION NOT VALID **'                             
         MVC   KEY,SVKEY1                                                       
         B     SPL2                                                             
*                                                                               
SPL1A    EDIT  (C4,18(R6)),(4,P+18)                                             
*                                                                               
         MVC   KEY(17),=17C'0'     NOW WE NEED THE MARKET NAME                  
         MVI   KEY,C'M'            MARKET                                       
         MVC   KEY+1(1),QMED       MEDIA                                        
         MVC   KEY+2(4),18(R6)     MARKET CODE                                  
         MVC   KEY+6(2),AGY                                                     
         L     R6,ADMARKET                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,STATION,KEY,(R6)                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   0(15,R6),KEY                                                     
         BE    SPL1C                                                            
         MVC   P+62(15),=C'*** UNKNOWN ***'                                     
         MVC   KEY,SVKEY1                                                       
         B     SPL2                                                             
*                                                                               
SPL1C    MVC   P+26(24),18(R6)                                                  
         MVC   KEY,SVKEY1                                                       
*                                                                               
SPL2     LA    R2,REC+24                                                        
         MVC   P+3(5),KEY+5                                                     
         OC    KEY+10(2),KEY+10                                                 
         BZ    SPL2B                                                            
         GOTO1 CLUNPK,DMCB,KEY+10,P+11                                          
SPL2B    MVI   ELCODE,X'05'                                                     
         CLI   0(R2),X'05'                                                      
         BE    SPL5                                                             
         CLI   0(R2),0                                                          
         BE    SPLX                NO EXCEPTIONS                                
         CLI   0(R2),X'F1'                                                      
         BE    SPLX                MAYBE ONE                                    
*                                                                               
SPL3     BAS   RE,NEXTEL                                                        
         BNE   SPLSORT             GET THE OUTPUTS FROM XSORT                   
         USING SDEFEL05,R2                                                      
*                                                                               
SPL5     DS    0H                                                               
         MVC   0(10,R3),0(R2)      STORE THE ENTIRE ELEMENT FOR SORTING         
         LA    R5,1(R5)                                                         
*                                                                               
         MVC   KEY(17),=17C'0'                                                  
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED       MEDIA                                        
         LH    R0,SDEFAMKT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB                                                     
         MVC   KEY+6(2),AGY                                                     
         L     R6,ADMARKET                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,STATION,KEY,(R6)                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   0(15,R6),KEY                                                     
         BE    SPL6                                                             
         MVC   10(15,R3),=C'*** UNKNOWN ***'                                    
         B     SPL7                GET THE NEXT ELEMENT                         
*                                                                               
SPL6     MVC   10(24,R3),18(R6)                                                 
*                                                                               
         LA    R3,34(R3)           BUMP TO NEXT ENTRY                           
*        LA    R5,1(R5)                                                         
*                                                                               
SPL7     B     SPL3                                                             
*                                                                               
SPLSORT  CLI   QOPT2,C'A'          SORT ALPHABETICALLY?                         
         BNE   SPLSORT2                                                         
         GOTO1 =V(XSORT),DMCB,(0,XSORTTAB),(R5),34,24,10                        
         B     SPL8                                                             
SPLSORT2 CLI   QOPT2,C'H'          SORT MARKET DECENDING?                       
         BNE   SPLSORT3                                                         
         GOTO1 =V(XSORT),DMCB,(1,XSORTTAB),(R5),34,2,2                          
         B     SPL8                DEFAULT SORT MKT ASCENDING!                  
SPLSORT3 GOTO1 =V(XSORT),DMCB,(0,XSORTTAB),(R5),34,2,2                          
*                                                                               
SPL8     LA    R3,XSORTTAB                                                      
*                                                                               
SPL9     EDIT  (2,2(R3)),(4,P+54),0                                             
         EDIT  (2,6(R3)),(4,P+88),0,FLOAT=-,ZERO=BLANK                          
         EDIT  (2,4(R3)),(4,P+99),0                                             
         CLI   SDEFCEX-SDEFEL05(R3),X'80'                                       
         BNE   *+8                                                              
         MVI   P+53,C'*'                                                        
         MVC   P+61(24),10(R3)                                                  
         BAS   RE,PRINTIT                                                       
         LA    R3,34(R3)                                                        
         CLI   0(R3),X'FF'         EXCEEDED 20 SPILL ENTRIES?                   
         BNE   *+6                                                              
         DC    H'0'                END SO WE KNOW WE NEED TO EXPAND             
         BCT   R5,SPL9             OUTPUT NEXT SPILL MKT                        
*                                                                               
SPLX     B     EXIT                                                             
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                                                               
         EJECT                                                                  
PRINTIT  NTR1                                                                   
         CLI   RCSUBPRG,1                                                       
         BNE   HL4                                                              
         MVC   H4+1(7),=C'NETWORK'                                              
         MVC   H4+11(4),NETLINE                                                 
         OC    CLTLINE(2),CLTLINE                                               
         BZ    HL2                                                              
         MVC   H6+1(6),=C'CLIENT'                                               
         MVC   H6+11(3),CLTLINE                                                 
*                                                                               
HL2      OC    ESTLINE,ESTLINE                                                  
         BZ    HLX                                                              
         MVC   H7+1(8),=C'ESTIMATE'                                             
         MVC   H7+11(3),ESTLINE                                                 
         B     HLX                                                              
*                                                                               
HL4      CLI   RCSUBPRG,2                                                       
         BNE   HL8                                                              
         MVC   H4+9(4),NETLINE                                                  
         MVC   H6+9(50),PGMLINE                                                 
         B     HLX                                                              
HL8      CLI   RCSUBPRG,3                                                       
         BNE   HLX                                                              
         MVC   H4+16(3),RATSRVC                                                 
*                                                                               
HLX      GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PROCNET  EQU   1                                                                
PROCSHW  EQU   2                                                                
PROCSPL  EQU   3                                                                
         SPACE 2                                                                
ELCODE   DS    CL1                                                              
MAXCLTS  DC    F'20'                                                            
PCTTOT   DS    F                                                                
*                                                                               
SVKEY1   DS    XL32                                                             
XSORTTAB DS    20CL34              TABLE FOR XSORT                              
         DC    X'FF'               MAYBE 20 ENTRIES ARE NOT ENOUGH              
*                                                                               
PNTSW    DS    CL1                                                              
PGMLINE  DS    CL50                                                             
RATSRVC  DS    CL3                                                              
NETLINE  DS    CL4                                                              
CLTLINE  DS    CL3                                                              
ESTLINE  DS    CL3                                                              
CABLENET DS    XL1                 'Y' IF CANADIAN CABLE NETWORK                
REGCNT   DS    H                                                                
MAXREGNS EQU   30                                                               
REGTAB   DS    (MAXREGNS*2+1)F     30 X 8 BYTES PER REGION +4                   
*                                                                               
REC      DS    CL2000                                                           
         SPACE 2                                                                
       ++INCLUDE SPGENNDEF                                                      
         SPACE 2                                                                
       ++INCLUDE SPGENNPGM                                                      
       ++INCLUDE SPGENSDEF                                                      
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
******** PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPREP4402 09/19/11'                                      
         END                                                                    
