*          DATA SET SPREPT902  AT LEVEL 078 AS OF 02/11/03                      
*PHASE SPT902A                                                                  
         TITLE 'SPREPT902-TRAFFIC PLAN REPORT'                                  
SPT902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPT902,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPT9RA                                                     
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   M2                                                               
         L     RE,=V(DTEGRID)                                                   
         ST    RE,VDTEGRID                                                      
         L     RE,=V(MKTTBL)                                                    
         ST    RE,VMKTTBL                                                       
         L     RE,=V(PRDTBL)                                                    
         ST    RE,VPRDTAB                                                       
         L     RE,=V(SMAXTAB)                                                   
         ST    RE,VSMAXTAB                                                      
         ST    RE,VSMAXTAB                                                      
         LA    RE,MYHEAD                                                        
         ST    RE,HEADHOOK                                                      
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         B     EXIT                                                             
         SPACE 2                                                                
M2       CLI   MODE,REQFRST                                                     
         BNE   M21                                                              
         CLI   QOPT1,C' '                                                       
         BNE   *+10                                                             
         MVC   QOPT1,PROGPROF                                                   
         OI    PROGPROF+1,X'F0'                                                 
         CLI   QOPT2,C' '                                                       
         BNE   *+10                                                             
         MVC   QOPT2,PROGPROF+1                                                 
         MVC   FCRDBUYS(2),=C'NY'                                               
         CLI   QOPT1,C'B'                                                       
         BNE   *+10                                                             
         MVC   FCRDBUYS(2),=C'YN'                                               
         L     RE,VMKTTBL                                                       
         L     RF,=F'40000'                                                     
         XCEF                                                                   
         XC    PCOUNT,PCOUNT                                                    
         B     EXIT                                                             
         SPACE 2                                                                
M21      CLI   MODE,ESTFRST                                                     
         BNE   M3                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDNUMWK,=F'18'                                                  
         MVC   MEDNUMPE,=F'0'                                                   
         MVC   MEDLCHNK,=F'168'                                                 
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   RQSTAFLT(1),QAFFIL                                               
         LA    R6,DTETBL           SET UP FOR SLOT TABLE                        
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         L     R7,MEDAFRST                                                      
         SR    R5,R5                                                            
M210     C     R7,MEDALAST                                                      
         BH    M210A                                                            
         OC    0(2,R7),0(R7)                                                    
         BNZ   *+12                                                             
         LA    R7,12(R7)                                                        
         B     M210                                                             
         LA    R5,1(R5)                                                         
         LA    R7,12(R7)                                                        
         B     M210                                                             
M210A    L     R7,MEDAFRST                                                      
         STC   R5,NUMWKS                                                        
         MVI   SLOTCTRL,0          SET FIRST SLOT                               
         SR    R8,R8                                                            
M21A     SRL   R8,1                MOVE BIT 1 POSITION                          
         LTR   R8,R8                                                            
         BNZ   M21B                ZERO - NO                                    
         ZIC   RF,SLOTCTRL                YES - BUMP SLOT CONTROL               
         LA    RF,1(RF)                                                         
         STC   RF,SLOTCTRL                                                      
         LA    R8,128                           SET X'80' BIT                   
M21B     OC    0(2,R7),0(R7)                                                    
         BZ    M21C                                                             
         MVC   0(4,R6),0(R7)       SET DATE                                     
         MVC   4(1,R6),SLOTCTRL    SET SLOT                                     
         STC   R8,5(R6)            SET BIT                                      
M21C     LA    R6,6(R6)            BUMP DATE TABLE POINTER                      
         LA    R7,12(R7)           BUMP MEDBLOCK POINTER                        
         OC    0(2,R7),0(R7)                                                    
         BZ    *-10                                                             
         C     R7,MEDALAST         END                                          
         BH    *+8                                                              
         B     M21A                 NO - DO NEXT                                
         B     EXIT                                                             
         EJECT                                                                  
M3       CLI   MODE,PROCBUY                                                     
         BNE   M3D                                                              
         CLI   QOPT1,C'B'                                                       
         BNE   EXIT                                                             
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         L     R8,ADBUY                                                         
         USING BUYREC,R8                                                        
         MVC   MEDBRAND,BPRD                                                    
         XC    PSLIST,PSLIST                                                    
         MVC   SVSPLN,BDSEC                                                     
         CLI   BPRD,X'FF'          POL USES BUY DESCRIPTION LENGTH              
         BE    M3SLOK                                                           
*        GET PRODUCT LENGTH - MAY BE A PIGGYBACK                                
         GOTO1 MEDPSL,DMCB,(RA),PSLIST                                          
         L     RE,MEDBUFF          RESTORE RE                                   
         LA    RF,PSLIST                                                        
M3PSL    CLI   0(RF),0                                                          
         BE    EXIT                                                             
         CLC   0(1,RF),BPRD                                                     
         BE    *+12                                                             
         LA    RF,2(RF)                                                         
         B     M3PSL                                                            
         MVC   SVSPLN,1(RF)                                                     
M3SLOK   MVC   MEDSPTLN,SVSPLN                                                  
         GOTO1 MEDGETBY,DMCB,(RA),0                                             
         L     RE,MEDBUFF                                                       
         CLI   MEDSPILL,C'Y'                                                    
         BE    EXIT                                                             
         L     R5,MEDAFRST                                                      
         LA    R6,MTABHLD                                                       
         USING HMKTBLD,R6                                                       
M3AA1    CLI   HMTSLN,0            FIND PROPER SLOT                             
         BE    M3A                                                              
         CLC   HMTSLN,MEDSPTLN                                                  
         BE    M3A                                                              
         LA    R6,17(R6)                                                        
         B     M3AA1                                                            
         SPACE 2                                                                
M3A      C     R5,MEDALAST         END                                          
         BH    EXIT                 YES - EXIT                                  
         OC    0(2,R5),0(R5)       OPEN SLOT                                    
         BZ    M3C                  YES - NEXT SLOT                             
         L     R4,4(R5)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYSPT,MEDBYSPT   ANY DATA                                     
         BZ    M3C                  NO - NEXT SLOT                              
         MVC   HMTSLN,MEDSPTLN     SET SPOT LENGTH                              
         MVC   HMTMKTN(5),BUYMSTA  SET MARKET/STATION                           
         OC    HMTSDTE,HMTSDTE                                                  
         BNZ   *+10                                                             
         MVC   HMTSDTE,0(R5)       SET START DATE                               
         CLC   HMTSDTE,0(R5)       SET START DATE                               
         BL    *+10                                                             
         MVC   HMTSDTE,0(R5)                                                    
         LA    RF,DTETBL                                                        
         USING DTETBLD,RF                                                       
M3B      CLC   DTDATE,0(R5)        GET SLOT AND BIT                             
         BE    M3B1                                                             
         LA    RF,LDTETAB(RF)                                                   
         CLI   0(RF),0                                                          
         BNE   M3B                                                              
         DC    H'0'                                                             
M3B1     LA    R9,HMTSLOTS         SET SLOT                                     
         ZIC   R1,DTSLOT                                                        
         BCTR  R1,0                                                             
         AR    R9,R1                                                            
         OC    0(1,R9),DTBIT       SET WEEK BIT                                 
M3C      LA    R5,12(R5)                                                        
         B     M3A                                                              
         DROP  RF                                                               
         DROP  R8                                                               
         SPACE 2                                                                
M3D      CLI   MODE,STALAST                                                     
         BNE   M4                                                               
         LA    R6,MTABHLD                                                       
M3D1     OC    HMTSDTE,HMTSDTE                                                  
         BZ    EXIT                                                             
         L     R9,MTCOUNT                                                       
         L     R8,VMKTTBL                                                       
         GOTO1 BINSRCH,DMCB,(X'01',(R6)),(R8),(R9),17,(0,8),2352                
         MVC   MTCOUNT,8(R1)                                                    
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R6,17(R6)                                                        
         B     M3D1                                                             
         EJECT                                                                  
         EJECT                                                                  
M4       CLI   MODE,PROCGOAL                                                    
         BNE   M6                                                               
         CLI   QOPT1,C'B'                                                       
         BE    EXIT                                                             
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         LA    R8,KEY                                                           
         USING GOALREC,R8                                                       
         MVC   MEDBRAND,GKEYPRD                                                 
         MVC   MEDSPTLN,GKEYSLN                                                 
         GOTO1 MEDGETGL,DMCB,(RA)                                               
         L     RE,MEDBUFF                                                       
         L     R5,MEDAFRST                                                      
         XC    MTABHLD,MTABHLD                                                  
         LA    R6,MTABHLD                                                       
         USING HMKTBLD,R6                                                       
M4A      C     R5,MEDALAST         END                                          
         BH    M4D                  YES - INSERT INTO TABLE                     
         OC    0(2,R5),0(R5)       OPEN SLOT                                    
         BZ    M4C                  YES-NEXT SLOT                               
         L     R4,4(R5)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDGLD(12),MEDGLD   ANY DATA                                     
         BZ    M4C                  NO - NEXT SLOT                              
         MVC   HMTSLN,MEDSPTLN     SET SPOT LENGTH                              
         PACK  DUB,MARKET                                                       
         CVB   RF,DUB                                                           
         STH   RF,HALF                                                          
         MVC   HMTMKTN,HALF        SET MARKET                                   
         OC    HMTSDTE,HMTSDTE                                                  
         BNZ   *+10                                                             
         MVC   HMTSDTE,0(R5)       SET START DATE                               
         LA    RF,DTETBL                                                        
         USING DTETBLD,RF                                                       
M4B      CLC   DTDATE,0(R5)        GET SLOT AND BIT                             
         BE    M4B1                                                             
         LA    RF,LDTETAB(RF)                                                   
         CLI   0(RF),0                                                          
         BNE   M4B                                                              
         DC    H'0'                                                             
M4B1     LA    R9,HMTSLOTS         SET SLOT                                     
         ZIC   R1,DTSLOT                                                        
         BCTR  R1,0                                                             
         AR    R9,R1                                                            
         OC    0(1,R9),DTBIT      SET WEEK BIT                                  
M4C      LA    R5,12(R5)                                                        
         B     M4A                                                              
         DROP  RF                                                               
         SPACE 2                                                                
M4D      OC    HMTSDTE,HMTSDTE                                                  
         BZ    EXIT                                                             
         L     R9,SMAXCNT                                                       
         MVC   FULL(1),HMTSLN                                                   
         MVC   FULL+1(2),HMTMKTN                                                
         L     R2,VSMAXTAB                                                      
         GOTO1 BINSRCH,DMCB,FULL,(R2),(R9),4,(0,3),700                          
         CLI   0(R1),1                                                          
         BE    NOINV                                                            
         L     RF,0(R1)                                                         
         MVC   HMTMAXS,3(RF)                                                    
         B     *+8                                                              
NOINV    MVI   HMTMAXS,X'FF'                                                    
         L     R9,MTCOUNT          CHECK FOR PREVIOUS ENTRY                     
         LTR   R9,R9                                                            
         BZ    M4E2                                                             
         L     RF,VMKTTBL                                                       
         USING MKTBLD,RF                                                        
M4E      CLC   HMTSLN,MTSLN                                                     
         BNE   M4E1                                                             
         CLC   HMTMKTN,MTMKTN                                                   
         BNE   M4E1                                                             
         OC    MTSLOTS,HMTSLOTS    PREVIOUS ENTRY - RESET DATA                  
         CLC   HMTSDTE,MTSDTE                                                   
         BNL   EXIT                                                             
         MVC   MTSDTE,HMTSDTE                                                   
         L     R9,MTCOUNT                                                       
         L     R8,VMKTTBL                                                       
         GOTO1 XSORT,DMCB,(0,(R8)),(R9),17,8,0                                  
         B     EXIT                                                             
M4E1     LA    RF,LMKTBL(RF)                                                    
         BCT   R9,M4E                                                           
M4E2     L     R9,MTCOUNT                                                       
         L     R8,VMKTTBL                                                       
         GOTO1 BINSRCH,DMCB,(X'01',MTABHLD),(R8),(R9),17,(0,8),1176             
         MVC   MTCOUNT,8(R1)                                                    
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         CLI   0(R1),1                                                          
         BE    EXIT                                                             
         L     RF,0(R1)                                                         
         USING MKTBLD,RF                                                        
         CLC   HMTSDTE,MTSDTE                                                   
         BNL   *+10                                                             
         MVC   MTSDTE,HMTSDTE      SET DATE                                     
         OC    MTSLOTS,HMTSLOTS    SET SLOTS                                    
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
M6       CLI   MODE,PRDFRST                                                     
         BNE   M8                                                               
         MVI   TOTALSW,0                                                        
         MVI   FORCEHED,C'Y'                                                    
         L     RE,VMKTTBL                                                       
         L     RF,=F'40000'                                                     
         XCEF                                                                   
         CLI   QOPT1,C'B'                                                       
         BE    GETMAXX                                                          
         CLI   QOPT2,C'0'                                                       
         BNE   GETMAXX                                                          
         XC    SMAXCNT,SMAXCNT                                                  
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING TEKEYD,RE           GET PRODUCT EQUIVALENCE                      
         MVC   TECLT,QCLT                                                       
         MVI   TEFCODE,X'0A'                                                    
         MVI   TERCODE,X'04'                                                    
         MVC   TEAGY,BAGY                                                       
         MVC   TEMED,QMED                                                       
         MVC   TECLT,QCLT                                                       
         L     RF,VSMAXTAB                                                      
         ST    RF,AREC                                                          
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETMAXX                                                          
         GOTO1 GET                                                              
         DROP  RE                                                               
         L     RE,AREC                                                          
         LA    RE,24(RE)                                                        
         MVC   ISCICLT,QCLT                                                     
         ZIC   RF,BPRD                                                          
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   CURRPRD,1(RF)                                                    
         MVC   ISCIPRD,1(RF)                                                    
M6A      CLI   0(RE),0             FIND EQUIVALENCE FOR CURRENT PRODUCT         
         BE    M6AX                                                             
         CLI   0(RE),X'30'                                                      
         BE    M6A1                                                             
         ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     M6A                                                              
M6A1     LR    R1,RE                                                            
         ZIC   R0,1(RE)                                                         
         AR    R1,R0                                                            
         SH    R1,=H'6'                                                         
         USING TEQELEM,RE                                                       
M6A2     CLC   CURRPRD,TEQACTP                                                  
         BE    M6A3                                                             
         LA    RE,7(RE)                                                         
         CR    RE,R1                                                            
         BH    M6AX                                                             
         B     M6A2                                                             
         SPACE 2                                                                
M6A3     MVC   ISCIPRD,TEQISCP                                                  
         MVC   ISCICLT,TEQISCC                                                  
M6AX     DS    0H                                                               
         DROP  RE                                                               
         EJECT                                                                  
* READ STATION INVENTORY FILE FOR MAXIMUM STATIONS                              
         DROP  R8                                                               
         XC    KEY,KEY                                                          
         XC    SMAXCNT,SMAXCNT                                                  
         L     RE,VSMAXTAB                                                      
         L     RF,=F'2800'                                                      
         XCEF                                                                   
         LA    R8,KEY                                                           
         USING SIKEY,R8                                                         
         MVI   SIFCODE,X'0A'                                                    
         MVI   SIRCODE,X'03'                                                    
         MVC   SIAGY,BAGY                                                       
         MVC   SIMED,QMED                                                       
         MVC   SICLT,ISCICLT                                                    
         L     R5,VSMAXTAB                                                      
         GOTO1 HIGH                                                             
         B     GETMAX1                                                          
GETMAX   GOTO1 SEQ                                                              
GETMAX1  CLC   SICLT,ISCICLT                                                    
         BNE   GETMAXX                                                          
         GOTO1 GET                                                              
         MVC   DUB(1),SIFLEN                                                    
         MVC   DUB+1(2),SIMKTST                                                 
         MVI   DUB+3,1                                                          
         L     RE,AREC                                                          
         LA    RE,24(RE)                                                        
         USING SISELEM,RE                                                       
GETMAX2  CLI   0(RE),0                                                          
         BE    GETMAX                                                           
         CLI   SISCODE,X'11'                                                    
         BE    GETMAX4                                                          
GETMAX3  ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     GETMAX2                                                          
GETMAX4  CLC   SISPRD,ISCIPRD                                                   
         BNE   GETMAX3                                                          
         DROP  RE                                                               
         L     R9,SMAXCNT                                                       
         L     R2,VSMAXTAB                                                      
         GOTO1 BINSRCH,DMCB,(X'01',DUB),(R2),(R9),4,(0,3),700                   
         MVC   SMAXCNT,8(R1)                                                    
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         CLI   0(R1),1                                                          
         BE    GETMAX                                                           
         L     R1,0(R1)                                                         
         SR    R9,R9                                                            
         IC    R9,3(R1)                                                         
         LA    R9,1(R9)            COUNT STATIONS                               
         STC   R9,3(R1)                                                         
         B     GETMAX                                                           
GETMAXX  XC    MTCOUNT,MTCOUNT                                                  
         B     EXIT                                                             
         EJECT                                                                  
M8       CLI   MODE,PRDLAST                                                     
         BNE   M10                                                              
         L     R4,VMKTTBL                                                       
         USING MKTBLD,R4                                                        
         CLI   0(R4),0             ANY ACTIVITY                                 
         BE    EXIT                                                             
         MVI   TOTALSW,0                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVI   PREVSLN,0                                                        
M8AA     CLI   PREVSLN,0           FIRST TIME                                   
         BE    M8A                  YES - BYPASS GRID PRINT                     
         CLC   PREVSLN,MTSLN                                                    
         BNE   M9                                                               
         BE    M8B                                                              
M8A      L     RE,VDTEGRID                                                      
         L     RF,=F'1800'                                                      
         XCEF                                                                   
M8B      XC    BUFFREC,BUFFREC                                                  
         MVC   PREVSLN,MTSLN                                                    
         MVC   BFSLN,MTSLN                                                      
         MVC   BFMKTN,MTMKTN                                                    
         MVC   BFSTAT,MTSTAT                                                    
         MVC   BFSDTE,MTSDTE                                                    
         MVC   BFMAXS,MTMAXS                                                    
         CLI   BFMAXS,X'FF'                                                     
         BNE   *+8                                                              
         MVI   BFMAXS,3                                                         
         CLI   QOPT2,C'0'                                                       
         BE    *+10                                                             
         MVC   BFMAXS,QOPT2                                                     
         NI    BFMAXS,X'0F'                                                     
         CLI   QOPT1,C'B'                                                       
         BNE   *+8                                                              
         MVI   BFMAXS,1                                                         
         MVC   BFLASTS,MTLASTS                                                  
         LA    R5,BFWEEKS                                                       
         LA    R6,MTSLOTS                                                       
         LA    RE,17                                                            
M8C      LA    R7,8                                                             
         ZIC   R9,0(R6)                                                         
         SLL   R9,24                                                            
M8D      SR    R8,R8                                                            
         SLDL  R8,1                                                             
         LTR   R8,R8                                                            
         BZ    *+8                                                              
         STC   R8,0(R5)                                                         
         LA    R5,1(R5)                                                         
         BCT   RE,*+8                                                           
         B     M8D1                                                             
         BCT   R7,M8D                                                           
         LA    R6,1(R6)                                                         
         B     M8C                                                              
         SPACE 2                                                                
M8D1     CLC   QPRD,=C'ALL'        BUILD PRODUCT BUFFER                         
         BNE   M8E                                                              
         MVC   PTSLN,BFSLN                                                      
         MVC   PTBRND,PRD                                                       
         XC    PTWEEKS(34),PTWEEKS                                              
         LA    R6,BFWEEKS                                                       
         LA    RE,17                                                            
         LA    RF,PTWEEKS                                                       
M8D2     CLI   0(R6),0                                                          
         BE    *+10                                                             
         MVC   1(1,RF),BFMAXS                                                   
         LA    R6,1(R6)                                                         
         LA    RF,2(RF)                                                         
         BCT   RE,M8D2                                                          
         L     R6,PCOUNT                                                        
         L     R7,VPRDTAB                                                       
         GOTO1 BINSRCH,DMCB,(X'01',PTHLD),(R7),(R6),38,(0,4),260                
         MVC   PCOUNT,8(R1)                                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),1                                                          
         BE    M8E                                                              
         L     R2,0(R1)            ADD STATIONS TO SLOT                         
         LA    R2,4(R2)                                                         
         LA    RE,17                                                            
         LA    RF,PTWEEKS                                                       
M8D3     MVC   HALF,0(RF)                                                       
         LH    R9,HALF                                                          
         MVC   HALF,0(R2)                                                       
         LH    R8,HALF                                                          
         AR    R9,R8                                                            
         STH   R9,HALF                                                          
         MVC   0(2,R2),HALF                                                     
         LA    R2,2(R2)                                                         
         LA    RF,2(RF)                                                         
         BCT   RE,M8D3                                                          
         SPACE 2                                                                
M8E      GOTO1 DATCON,DMCB,(X'02',BFSDTE),(X'05',P)                             
         MVC   HALF,BFMKTN                                                      
         LH    RE,HALF                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+9(4),DUB+5(3)                                                  
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),P+9                                                     
         MVC   KEY+6(2),RCAGENCY                                                
         GOTO1 READMKT                                                          
         MVC   P+14(24),MKTNM                                                   
         EDIT  BFMAXS,(4,P+40)                                                  
         CLI   QOPT1,C'B'                                                       
         BNE   M8E1                                                             
         GOTO1 MSUNPK,DMCB,BFMKTN,WORK,P+40                                     
M8E1     LA    RF,P+50                                                          
         LA    RE,BFWEEKS                                                       
         LA    R7,17                                                            
M8F      CLI   0(RE),0                                                          
         BE    *+8                                                              
         MVI   2(RF),C'X'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R7,M8F                                                           
         GOTO1 REPORT                                                           
         SPACE 2                                                                
* BUILD TOTAL TABLE                                                             
         MVI   WEEK,1                                                           
M8G      ZIC   RE,WEEK                                                          
         BCTR  RE,0                                                             
         MH    RE,=H'68'                                                        
         L     RF,VDTEGRID         SET RE TO DATE SLOT                          
         AR    RE,RF                                                            
         ZIC   RF,WEEK                                                          
         BCTR  RF,0                                                             
         LA    R9,BFWEEKS(RF)      SET TO FIRST WEEK                            
         MH    RF,=H'4'            SET TO CROSS WEEK                            
         AR    RF,RE                                                            
M8H      CLI   0(R9),X'FF'                                                      
         BE    M8J                                                              
         CLI   0(R9),0                                                          
         BNE   M8I                                                              
         LA    RF,4(RF)            BUMP CROSS WEEK SLOTS                        
         LA    R9,1(R9)                                                         
         B     M8H                                                              
M8I      ZIC   R1,BFMAXS           ADD STATION TOTAL                            
         A     R1,0(RF)                                                         
         ST    R1,0(RF)                                                         
M8J      ZIC   R1,WEEK            SET NEXT VERTICAL WEEK                        
         LA    R1,1(R1)                                                         
         STC   R1,WEEK                                                          
         CLI   WEEK,18                                                          
         BL    M8G                                                              
         LA    R4,LMKTBL(R4)                                                    
         CLI   0(R4),0             END                                          
         BNE   M8AA                 NO- GET NEXT MARKET                         
         EJECT                                                                  
*                                                                               
M9       MVI   WEEK,1                                                           
         MVI   WEEK1,0                                                          
         MVI   ALLOWLIN,19                                                      
         MVI   TOTALSW,1                                                        
         MVI   FORCEHED,C'Y'                                                    
         ZIC   R5,NUMWKS                                                        
         LA    R6,P+50                                                          
         L     R7,VDTEGRID                                                      
         LA    R3,DTETBL                                                        
M9A      OC    0(68,R7),0(R7)                                                   
         BZ    M9C                                                              
         OC    0(2,R3),0(R3)                                                    
         BZ    M9C                                                              
         GOTO1 DATCON,DMCB,(X'02',0(R3)),(X'05',P+40)                           
         LR    R8,R7                                                            
         SR    R2,R2                                                            
M9B      L     R9,0(R8)                                                         
         ZIC   R1,WEEK1                                                         
         LA    R1,1(R1)                                                         
         STC   R1,WEEK1                                                         
         AR    R9,R2                                                            
         A     R2,0(R8)                                                         
         EDIT  (R9),(3,0(R6))                                                   
         LTR   R9,R9                                                            
         BNZ   M9B1                                                             
         CLC   WEEK1,WEEK                                                       
         BL    M9B1                                                             
         MVI   2(R6),C'0'          PRINT ZEROS                                  
M9B1     DS    0H                                                               
         LA    R6,4(R6)                                                         
         LA    R8,4(R8)                                                         
         BCT   R5,M9B                                                           
         GOTO1 REPORT              PRINT GRID                                   
         MVI   ALLOWLIN,0                                                       
         SPACE 2                                                                
M9C      ZIC   R5,NUMWKS                                                        
         LA    R7,68(R7)           BUMP                                         
         LA    R6,P+50                                                          
         ZIC   R1,WEEK             COUNT WEEKS                                  
         LA    R1,1(R1)                                                         
         STC   R1,WEEK                                                          
         MVI   WEEK1,0                                                          
         LA    R3,LDTETAB(R3)                                                   
         CLI   WEEK,18                                                          
         BL    M9A                                                              
         MVI   TOTALSW,0                                                        
         MVI   FORCEHED,C'Y'                                                    
         XC    MTCOUNT,MTCOUNT                                                  
         CLI   0(R4),0                                                          
         BNE   M8A                                                              
         B     EXIT                                                             
         SPACE 2                                                                
M10      CLI   MODE,STAFRST                                                     
         BNE   M12                                                              
         XC    MTABHLD,MTABHLD                                                  
         B     EXIT                                                             
         SPACE 2                                                                
M12      CLI   MODE,CLTLAST                                                     
         BNE   M14                                                              
         CLC   QPRD,=C'ALL'                                                     
         BNE   EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         L     R7,PCOUNT                                                        
         LTR   R7,R7                                                            
         BZ    EXIT                                                             
         L     R2,VPRDTAB                                                       
         MVC   PREVSLN,0(R2)                                                    
M12A     CLC   PREVSLN,0(R2)                                                    
         BE    M12B                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PREVSLN,0(R2)                                                    
M12B     MVC   P+40(3),1(R2)                                                    
         LA    R3,4(R2)                                                         
         LA    R4,17                                                            
         LA    R5,P+50                                                          
M12C     MVC   HALF,0(R3)                                                       
         LH    R6,HALF                                                          
         EDIT  (R6),(3,0(R5))                                                   
         LA    R3,2(R3)                                                         
         LA    R5,4(R5)                                                         
         BCT   R4,M12C                                                          
         SPACE 2                                                                
         GOTO1 REPORT                                                           
         LA    R2,38(R2)                                                        
         BCT   R7,M12A                                                          
         B     EXIT                                                             
         SPACE 2                                                                
M14      DS    0H                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
         USING *,RF                                                             
MYHEAD   NTR1  BASE=SPT9RB                                                      
         DROP  RF                                                               
         LM    RA,RC,SPT9RA                                                     
         GOTO1 =V(PRTHEAD),DMCB,SMAXCNT,RR=RELO                                 
         XIT1                                                                   
SPT9RA   DC    F'0'                                                             
SPT9RB   DC    F'0'                                                             
SPT9RC   DC    F'0'                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
RELO     DS    F                                                                
SMAXCNT  DC    F'0'                                                             
TOTALSW  DC    X'00'                                                            
SLOTCTRL DS    C                                                                
WEEKS    DS    C                                                                
VDTEGRID DC    F'0'                                                             
VSMAXTAB DC    F'0'                                                             
VMKTTBL  DC    F'0'                                                             
ISCIPRD  DS    CL2                                                              
ISCICLT  DS    CL2                                                              
CURRPRD  DS    CL2                                                              
PTHLD    DS    0CL21                                                            
PTSLN    DS    CL1                                                              
PTBRND   DS    CL3                                                              
PTWEEKS  DS    CL34                                                             
MTCOUNT  DC    F'0'                                                             
PCOUNT   DS    F'0'                                                             
VPRDTAB  DS    F                                                                
PREVSLN  DS    C                                                                
WEEK     DS    C                                                                
WEEK1    DS    C                                                                
NUMWKS   DS    C                                                                
BUFFREC  DS    0CL27                                                            
BFSLN    DS    CL1                                                              
BFSDTE   DS    CL2                                                              
BFMKTN   DS    CL2                                                              
BFSTAT   DS    CL3                                                              
BFMAXS   DS    CL1                                                              
BFLASTS  DS    CL1                                                              
BFWEEKS  DS    CL17                                                             
         DC    X'FF'                                                            
MTABHLD  DS    CL85                                                             
PSLIST   DS    CL40                                                             
SVSPLN   DS    C                                                                
DTETBL   DS    1000C                                                            
SMAXTAB  CSECT                                                                  
         DS    2800C                                                            
MKTTBL   CSECT                                                                  
         DS    40000C                                                           
DTEGRID  CSECT                                                                  
         DS    1800C                                                            
PRDTBL   CSECT                                                                  
         DS    10000C                                                           
         EJECT                                                                  
PRTHEAD  CSECT                                                                  
         NMOD1 0,PRTHEAD                                                        
         L     R3,0(R1)                                                         
         USING SMAXCNT,R3                                                       
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         LA    R4,P+51                                                          
         LA    R4,H8+50                                                         
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         CLI   QOPT1,C'B'                                                       
         BNE   *+10                                                             
         XC    H8+40(7),H8+40                                                   
         CLI   TOTALSW,1                                                        
         BE    MYHEAD1                                                          
         CLI   MODE,CLTLAST                                                     
         BNE   MYHEAD1                                                          
         MVC   H6+56(20),=C'***CLIENT SUMMARY***'                               
         XC    H8+40(7),H8+40                                                   
         MVC   H9+40(8),=C'PRODUCTS'                                            
         XC    H8(15),H8                                                        
         XC    H9(15),H9                                                        
         XC    H10(15),H10                                                      
MYHEAD1  DS    0H                                                               
         MVC   H4+57(18),=C'BASED ON GOAL DATA'                                 
         CLI   QOPT1,C'B'                                                       
         BNE   *+10                                                             
         MVC   H4+58(18),=C'BASED ON BUY DATA '                                 
         MVC   H5+58(16),=C'    SECONDS ONLY'                                   
         EDIT  PREVSLN,(3,H5+58)                                                
         CLI   TOTALSW,1                                                        
         BE    MYHEAD4                                                          
         L     RE,MEDBUFF                                                       
         L     R5,MEDAFRST                                                      
MYHEAD2  L     RE,MEDBUFF                                                       
         C     R5,MEDALAST                                                      
         BH    MYHEADX                                                          
         OC    0(2,R5),0(R5)                                                    
         BZ    MYHEAD3                                                          
         GOTO1 DATCON,DMCB,(X'02',0(R5)),(X'05',WORK)                           
         CLI   TOTALSW,1                                                        
         BNE   XX2                                                              
         GOTO1 DATCON,DMCB,(X'02',0(R5)),(0,DUB)                                
         GOTO1 ADDAY,DMCB,DUB,WORK+8,6                                          
         GOTO1 DATCON,DMCB,(0,WORK+8),(X'05',WORK)                              
XX2      DS    0H                                                               
         MVC   0(3,R4),WORK                                                     
         LA    RE,132(R4)                                                       
         MVC   1(2,RE),WORK+3                                                   
         LA    RE,132(RE)                                                       
         MVC   0(3,RE),=C'---'                                                  
         LA    R4,4(R4)                                                         
MYHEAD3  LA    R5,12(R5)                                                        
         B     MYHEAD2                                                          
MYHEAD4  MVI   0(R4),C'-'                                                       
         MVC   1(71,R4),0(R4)                                                   
         MVC   29(14,R4),=C'E N D  D A T E'                                     
         XC    H8+40(8),H8+40                                                   
         XC    H9+40(8),H9+40                                                   
         XC    H8(15),H8                                                        
         XC    H9(15),H9                                                        
         XC    H10(15),H10                                                      
         MVC   H8+41(5),=C'START'                                               
         MVC   H9+42(4),=C'DATE'                                                
         LA    R4,132(R4)                                                       
         L     R5,MEDAFRST                                                      
         MVC   H6+49(34),=C'***COMMERCIALS REQUIRED SUMMARY***'                 
         B     MYHEAD2                                                          
MYHEADX  XIT1                                                                   
         EJECT                                                                  
DTETBLD  DSECT                                                                  
DTDATE   DS    CL4                 DATE                                         
DTSLOT   DS    CL1                 SLOT ASSOCIATED WITH THIS DATE               
DTBIT    DS    CL1                 BIT ASSOCIATED WITH THIS DATE                
         SPACE 2                                                                
MKTBLD   DSECT                                                                  
MTSLN    DS    CL1                 SPOT LENGTH                                  
MTSDTE   DS    CL2                 START DATE                                   
MTMKTN   DS    CL2                 MARKET NUMBER                                
MTSTAT   DS    CL3                 STATION CALL LETTERS                         
MTMAXS   DS    CL1                 MAXIMUM STATIONS                             
MTLASTS  DS    CL1                 LAST CYCLE STATIONS                          
MTSLOTS  DS    CL7                 WEEKLY BIT SLOTS                             
         SPACE 2                                                                
HMKTBLD  DSECT                     MARKET TABLE DSECT 2                         
HMTSLN   DS    CL1                                                              
HMTSDTE  DS    CL2                                                              
HMTMKTN  DS    CL2                                                              
HMTSTAT  DS    CL3                                                              
HMTMAXS  DS    CL1                                                              
HMTLASTS DS    CL1                                                              
HMTSLOTS DS    CL7                                                              
LDTETAB  EQU   6                                                                
LMKTBL   EQU   17                                                               
         EJECT                                                                  
       ++INCLUDE SIKEY                                                          
TEKEYD   DSECT                                                                  
       ++INCLUDE TEKEY                                                          
       ++INCLUDE TEQELEM                                                        
       ++INCLUDE SISELEM                                                        
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENMKT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'078SPREPT902 02/11/03'                                      
         END                                                                    
