*          DATA SET SPREPDR02  AT LEVEL 079 AS OF 05/01/02                      
*PHASE SPDR02T,+0,NOAUTO                                                        
         TITLE 'SPREPDR02 - DIRECT RESPONSE REPORT'                             
SPDR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPDR02,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R2,=V(SPDRWK)                                                    
         AR    R2,R5                                                            
         USING SPDRWK,R2                                                        
         ST    R5,RELO                                                          
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
         USING MEDDATA,R4                                                       
         USING SUMDSECT,R3                                                      
         STM   RA,RC,SP60RA                                                     
         ST    R2,SP60R2                                                        
         CLI   MODE,MKTLAST                                                     
         BL    BYPW                                                             
         MVI   BYPSUM,0                                                         
         GOTO1 MEDADDWT,DMCB,(RA)                                               
BYPW     DS    0H                                                               
         CLI   MODE,ESTFRST                                                     
         BNH   *+12                                                             
         CLI   QSTART,C' '                                                      
         BE    EXIT                                                             
         CLI   MODE,RUNFRST                                                     
         BNE   M1                                                               
         MVI   SORTREQ,1                                                        
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         L     RF,=V(SSTABLE)                                                   
         A     RF,RELO                                                          
         ST    RF,VSSTABLE                                                      
         L     RF,=V(SORTC)                                                     
         A     RF,RELO                                                          
         ST    RF,VRSORT                                                        
         L     RF,=V(BUFFALOC)                                                  
         A     RF,RELO                                                          
         ST    RF,BUFFBUFF                                                      
         LR    R9,RF                                                            
         GOTO1 BUFFALO,DMCB,=C'SET',(R9)                                        
         LA    RF,SPHOOK                                                        
         ST    RF,SPOTHOOK                                                      
         LA    RF,MYHEAD                                                        
         ST    RF,HEADHOOK                                                      
         MVI   FIRST,1                                                          
         B     EXIT                                                             
         SPACE 2                                                                
M1       CLI   MODE,MKTFRST                                                     
         BL    M2                                                               
         CLI   ESTACT,0            ANY ESTIMATES FOR PRODUCT                    
         BE    EXIT                                                             
         EJECT                                                                  
M2       CLI   MODE,REQFRST                                                     
         BNE   M4                                                               
         L     R9,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R9)                                      
         B     EXIT                                                             
         EJECT                                                                  
M4       CLI   MODE,ESTFRST                                                     
         BNE   M5                                                               
         MVI   ESTACT,1                                                         
         MVI   PASS,0                                                           
         GOTO1 =V(EFRSTC),RR=RELO                                               
         SR    RE,RE                                                            
         IC    RE,BPRD                                                          
         CLI   BPRD,X'FF'          GET DEMO NAMES FOR PRODUCT                   
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RE,28(RE,RF)                                                     
         MVC   3(3,RE),0(RE)       FORCE FOR RATINGS AND IMPS                   
         MVI   1(RE),C'R'                                                       
         MVI   4(RE),C'I'                                                       
         XC    DNAMES,DNAMES                                                    
         SPACE 2                                                                
* NEW FORMAT DEMOS                                                              
NEWDNAM  LA    R1,20               COUNT NUMBER OF ACTIVE DEMOS                 
         LR    R7,RE               SET START OF LIST                            
         LA    R9,0                                                             
NEWDNAM1 CLI   1(R7),0             END OF LIST                                  
         BE    NEWDNAM2                                                         
         LA    R9,1(R9)                                                         
         LA    R7,3(R7)                                                         
         BCT   R1,NEWDNAM1                                                      
NEWDNAM2 LA    R9,1(R9)                                                         
         SRL   R9,1                                                             
         LTR   R9,R9                                                            
         BNZ   *+8                                                              
         LA    R9,1                                                             
         ST    R9,NODEMS                                                        
NEWDNAM3 LR    R7,RE               RESTORE START OF LIST                        
         L     R6,ADBLOCK                                                       
         USING DBLOCK,R6                                                        
         XC    0(256,R6),0(R6)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         DROP  R6                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         LA    RF,EUSRNMS          SET FOR USER NAMES                           
         DROP  RF                                                               
         GOTO1 DEMOCON,DMCB,((R9),(R7)),(2,DNAME1),(C'S',ADBLOCK),     X        
               (SPOTPROF+9,(RF))                                                
         B     EXIT                                                             
         SPACE 2                                                                
M5       CLI   MODE,PRDFRST                                                     
         BNE   M6                                                               
         MVI   PASS,0                                                           
         MVI   ESTACT,0            RESET ESTIMATE ACTIVE SWITCH                 
M5LOOPX  DS    0H'0'                                                            
         B     EXIT                                                             
         EJECT                                                                  
M6       CLI   MODE,PROCBUY                                                     
         BNE   M8                                                               
         MVI   SORTPASS,1          SET SORT FOR STORE                           
         MVI   PRVDRB,0                                                         
M6A1SRT1 DS    0H'0'                                                            
M6A1SRT2 CLI   SORTREQ,1                                                        
         BNE   M6A1SRT3                                                         
         GOTO1 VRSORT                                                           
         CLI   SORTPASS,1          BUILD PASS                                   
         BE    SORTX                YES - EXIT                                  
         CLI   SORTPASS,3          END OF SORT                                  
         BE    SORTX                YES - EXIT                                  
         MVI   MEDEXTCS,C'R'                                                    
         MVI   RCSUBPRG,1                                                       
         L     R4,ADBUY            GET DATE SPREAD FROM BUY REC.                
         USING BUYREC,R4                                                        
         ZIC   R5,BDDAY                                                         
         DROP  R4                                                               
         SLL   R5,25                                                            
         LA    R1,0                                                             
         SR    R4,R4                                                            
         SLDA  R4,1                GET FIRST DAY                                
         LTR   R4,R4                                                            
         BZ    *-6                                                              
         LTR   R5,R5               END OF STRING                                
         BZ    *+16                                                             
         LA    R1,1(R1)                                                         
         SLDA  R4,1                                                             
         B     *-14                                                             
         ST    R1,BYDAYS                                                        
         USING MEDDATA,R4                                                       
M6A1SRT3 DS    0H                                                               
PRTLOOP  OC    THISELEM,THISELEM                                                
         BZ    SORTX                                                            
         LA    R8,P1                                                            
         USING PLINED,R8                                                        
         L     R5,THISELEM                                                      
         USING REGELEM,R5          SAVE REGELEM DATE                            
         MVC   SVBYDAY,RDATE                                                    
         LR    RE,R5                                                            
         ZIC   RF,1(R5)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),X'10'         LOOK FOR AFFID                               
         BNE   SVRSVPER                                                         
         ST    RE,THISAFFD                                                      
         ZIC   RF,1(RE)            LOOK FOR RSVP                                
         AR    RE,RF                                                            
         CLI   0(RE),X'17'                                                      
         BNE   SVRSVPER                                                         
         MVC   MEDBRAND,BPRD                                                    
         GOTO1 MEDGETBY,DMCB,(RA),7                                             
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M6A1SRT2                                                         
         L     RE,THISSLT                                                       
         CLC   PRVDRB,0(RE)        SKIP LINE IF CHANGE OF DAYPART               
         BE    PRTLOP1                                                          
         MVI   P,0                                                              
         GOTO1 REPORT                                                           
         L     RE,THISSLT                                                       
PRTLOP1  MVC   PRVDRB,0(RE)                                                     
         MVC   PLDPT,0(RE)                                                      
         L     RF,MEDBYD                                                        
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,MEDBYSPT                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(9,PLCPS),2                                                 
         OC    MEDCSPAY,MEDCSPAY                                                
         BZ    PRTLOP1A                                                         
         L     RF,MEDBYD                                                        
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,MEDCSPAY         $/RESP                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(6,PLCPR),2                                                 
PRTLOP1A L     RF,MEDBY1                                                        
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(3,PLRTG)                                                   
         EDIT  MEDBY2,(7,PLIMPR),1                                              
         EDIT  MEDCSPAY,(6,PLRSVP)                                              
         L     RF,MEDCSPAY                                                      
         SR    RE,RE                                                            
         M     RE,=F'100'                                                       
         OC    MEDBY2,MEDBY2                                                    
         BZ    PRTLOP2                                                          
         SLDA  RE,1                                                             
         D     RE,MEDBY2                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(7,PLRPV),1                                                 
         L     RF,MEDBYD                                                        
         SR    RE,RE                                                            
         M     RE,=F'20'                                                        
         D     RE,MEDBY2                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(7,PLCPM),2                                                 
PRTLOP2  DS    0H'0'                                                            
         L     R5,THISAFFD                                                      
         USING AFFELEM,R5                                                       
         GOTO1 DATCON,DMCB,(2,ADATE),DUB                                        
         GOTO1 GETDAY,DMCB,DUB,WORK                                             
         MVC   PLDAY,WORK                                                       
         GOTO1 DATCON,DMCB,(2,ADATE),(5,PLDATE)                                 
         XC    FULL,FULL                                                        
         MVC   FULL(2),ATIME                                                    
         GOTO1 UNTIME,DMCB,FULL,PLTIME                                          
         CLC   SVBYDAY,ADATE       AFFID LESS THAN ELEM DATE                    
         BH    RSVPERR                                                          
         GOTO1 DATCON,DMCB,(2,SVBYDAY),(0,WORK)                                 
         L     R6,BYDAYS                                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R6)                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,HALF)                                  
         CLC   ADATE,HALF                                                       
         BNH   *+14                                                             
RSVPERR  MVC   PLPROG,=C'RSVP DATE ERR.'                                        
         B     PRTLOOPP                                                         
         L     RE,MEDADEMO                                                      
         DROP  R5                                                               
         L     R5,4(RE)                                                         
         USING NDELEM,R5                                                        
         MVC   PLPROG,NDPROG                                                    
         OC    PLPROG,PLPROG                                                    
         BNZ   PRTLOOPP                                                         
         MVC   PLPROG,=C'NO DEMOS FOUND'                                        
PRTLOOPP GOTO1 REPORT                                                           
         LA    R5,MEDAFRST                                                      
PUTLOOP  OC    0(2,R5),0(R5)                                                    
         BZ    PUTLOOP2                                                         
         LA    R9,MYBUFIO                                                       
         USING DRBUFD,R9                                                        
         XC    MYBUFIO,MYBUFIO                                                  
         L     RE,THISSLT                                                       
         MVC   DRBDPT,0(RE)                                                     
         L     R4,4(R5)                                                         
         CLI   DRBDPT,C'Z'                                                      
         BE    PUTLOOP2                                                         
         OC    MEDBYD(12),MEDBYD                                                
         BZ    PUTLOOP2                                                         
         MVC   DRDATE,0(R5)                                                     
         MVC   DRDOL,MEDBYD                                                     
         MVC   DRSPOTS,MEDBYSPT                                                 
         MVC   DRRESP,MEDCSPAY                                                  
         MVC   DRRTG,MEDBY1                                                     
         MVC   DRVWRS,MEDBY2                                                    
         MVI   DRBCODE,1                                                        
         LA    RF,MEDMON01                                                      
         CR    R5,RF                                                            
         BL    *+8                                                              
         MVI   DRBCODE,2                                                        
         LA    RF,MEDQRT01                                                      
         CR    R5,RF                                                            
         BL    *+8                                                              
         MVI   DRBCODE,3                                                        
         LA    RF,MEDPERD                                                       
         CR    R5,RF                                                            
         BL    *+8                                                              
         MVI   DRBCODE,4                                                        
         BAS   R8,PUTBUFF                                                       
         MVC   BYTE,DRBDPT                                                      
         MVI   DRBDPT,X'FF'                                                     
         BAS   R8,PUTBUFF                                                       
         MVC   DRBDPT,BYTE                                                      
         CLI   DRBCODE,4                                                        
         BE    PUTLOOP2                                                         
         MVC   DRDATE,=X'FFFFFFFF'                                              
         BAS   R8,PUTBUFF                                                       
         MVI   DRBDPT,X'FF'                                                     
         BAS   R8,PUTBUFF                                                       
PUTLOOP2 LA    R5,12(R5)                                                        
         LA    RF,MEDPERD                                                       
         CR    R5,RF                                                            
         BNH   PUTLOOP                                                          
         B     M6A1SRT2                                                         
PUTBUFF  L     R9,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'PUT',(R9),MYBUFIO                                
         LA    R9,MYBUFIO                                                       
         BR    R8                                                               
SORTX    B     EXIT                                                             
         DROP  R5                                                               
         USING REGELEM,R5                                                       
         SPACE 2                                                                
SVRSVPER GOTO1 DATCON,DMCB,(2,RDATE),(5,PLDATE)                                 
         MVC   PLPROG(12),=C'MISSING RSVP'                                      
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         MVC   P(4),=C'EST='                                                    
         ZIC   RE,BUYKEST                                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+4(3),DUB+6(2)                                                  
         MVC   P+8(5),=C'LINE='                                                 
         ZIC   RE,BUYKBUY                                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+13(3),DUB+6(2)                                                 
         B     PRTLOOPP                                                         
         DROP  R5                                                               
         DROP  R8                                                               
         EJECT                                                                  
M8       CLI   MODE,STAFRST                                                     
         BNE   M10                                                              
         XC    PDNCNTR,PDNCNTR                                                  
         XC    SSCNTR,SSCNTR                                                    
         MVI   BYPSUM,0            RESET BYPASS SUMMARY SWITCH                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
M10      CLI   MODE,STALAST                                                     
         BNE   M12                                                              
         CLI   SORTREQ,1           SORT REQUIRED                                
         BNE   M10A1                                                            
         MVI   SORTPASS,2           YES - SET SORT PASS FOR EXTRACT             
         MVI   MODE,PROCBUY                                                     
         GOTO1 SORT2                                                            
         MVI   MODE,STALAST                                                     
M10A1    DS    0H                                                               
         MVI   LEVEL,1                                                          
         MVI   RCSUBPRG,4                                                       
         MVI   TOTSW,1                                                          
         ZIC   RE,NUMSTA           COUNT THE STATIONS                           
         LA    RE,1(RE)                                                         
         STC   RE,NUMSTA                                                        
         BAS   RE,DOSUM                                                         
         MVI   TOTSW,0                                                          
         L     R8,BUFFBUFF                                                      
         MVC   DMCB+8(24),LVCNTRL                                               
         GOTO1 BUFFALO,DMCB,=C'ADD',(R8)                                        
         BAS   RE,DOSUM                                                         
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         SPACE 2                                                                
*        TRAP FINAL SORT EXIT                                                   
SORT2    NTR1                                                                   
         B     M6A1SRT1                                                         
         SPACE 2                                                                
         SPACE 2                                                                
M12      CLI   MODE,MKTFRST                                                     
         BNE   M14                                                              
         MVI   NUMSTA,0                                                         
         MVI   BUYACT,0                                                         
         MVI   MKTACT,0                                                         
         B     EXIT                                                             
         SPACE 2                                                                
M14      CLI   MODE,MKTLAST                                                     
         BNE   M16                                                              
M14B     MVI   PASS,0                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   LEVEL,2                                                          
         MVI   BYPSUM,0            BYPASS PRINT IF 1 STATION                    
         CLI   NUMSTA,1                                                         
         BH    *+8                                                              
         MVI   BYPSUM,1                                                         
         BAS   RE,DOSUM                                                         
         B     EXIT                                                             
         EJECT                                                                  
M16      CLI   MODE,PROCGOAL                                                    
         BNE   M18                                                              
         CLI   BUYACT,1                                                         
         BNE   EXIT                                                             
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
M18      CLI   MODE,PRDLAST                                                     
         BNE   M20                                                              
         MVI   LEVEL,3                                                          
         BAS   RE,DOSUM                                                         
         B     EXIT                                                             
*                                                                               
M20      CLI   MODE,MGR1LAST                                                    
         BNE   M22                                                              
         MVI   LEVEL,4                                                          
         BAS   RE,DOSUM                                                         
         B     EXIT                                                             
*                                                                               
M22      CLI   MODE,MGR2LAST                                                    
         BNE   M24                                                              
         MVI   LEVEL,5                                                          
         BAS   RE,DOSUM                                                         
         B     EXIT                                                             
*                                                                               
M24      CLI   MODE,MGR3LAST                                                    
         BNE   M26                                                              
         MVI   LEVEL,6                                                          
         BAS   RE,DOSUM                                                         
         B     EXIT                                                             
         SPACE 2                                                                
M26      CLI   MODE,CLTLAST                                                     
         BNE   M28                                                              
         B     M34                                                              
*                                                                               
M28      CLI   MODE,PGR1LAST                                                    
         BNE   M30                                                              
         B     M34                                                              
*                                                                               
M30      CLI   MODE,PGR2LAST                                                    
         BNE   M32                                                              
         B     M34                                                              
M32      CLI   MODE,PGR3LAST                                                    
         BNE   M34                                                              
         B     M34                                                              
M34      CLI   MODE,REQLAST                                                     
         BNE   M36                                                              
         CLI   FOOT1,C' '                                                       
         BE    M36                                                              
         MVI   FORCEHED,C'N'                                                    
         MVI   P,0                                                              
         MVC   P2(132),FOOT1                                                    
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
M36      DS    0H'0'                                                            
EXIT     XMOD1 1                                                                
         EJECT                                                                  
         EJECT                                                                  
* DO SUMMARIES FOR VARIOUS BREAKS                                               
DOSUM    NTR1                                                                   
         CLI   TOTSW,1                                                          
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,P                                                             
         USING SLINED,R4                                                        
         XC    MYBUFIO,MYBUFIO                                                  
         MVI   PRVDRB,0            FORCE OUT HEADING                            
         L     R8,BUFFBUFF                                                      
         ZIC   R9,LEVEL                                                         
         CLI   BYPSUM,1            DONT PRINT DUPLICATE TOTALS                  
         BE    DOSUM2                                                           
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R8),MYBUFIO,(R9)                          
         B     GETBUF2                                                          
GETBUF1  L     R8,BUFFBUFF                                                      
         ZIC   R9,LEVEL                                                         
         GOTO1 BUFFALO,DMCB,=C'SEQ',(R8),MYBUFIO,(R9)                           
GETBUF2  TM    DMCB+8,X'80'                                                     
         BO    DOSUM2                                                           
         LA    R9,MYBUFIO                                                       
         USING DRBUFD,R9                                                        
         CLI   DRBCODE,3           BYPASS QUARTERLY SUMMARY                     
         BE    GETBUF1                                                          
         CLI   TOTSW,1                                                          
         BNE   *+12                                                             
         CLI   DRBCODE,4                                                        
         BNE   GETBUF1                                                          
         CLC   PRVDRB,DRBCODE                                                   
         BE    GETBUF3                                                          
         CLI   PRVDRB,0                                                         
         BE    GETBUF2A                                                         
         MVI   P,0                                                              
         GOTO1 REPORT                                                           
GETBUF2A DS    0H'0'                                                            
         CLI   TOTSW,1                                                          
         BE    GETBUF5                                                          
         MVI   FORCEMID,C'Y'                                                    
         MVC   MID1+7(20),=C'***WEEKLY SUMMARY***'                              
         CLI   DRBCODE,2                                                        
         BNE   *+10                                                             
         MVC   MID1+7(20),=C'**MONTHLY SUMMARY***'                              
         CLI   DRBCODE,3                                                        
         BNE   *+10                                                             
         MVC   MID1+7(20),=C'*QUARTERLY SUMMARY**'                              
         CLI   DRBCODE,4                                                        
         BNE   *+10                                                             
         MVC   MID1+7(20),=C'***PERIOD SUMMARY***'                              
         MVC   MID2+7(20),=C'--------------------'                              
         MVC   PRVDRB,DRBCODE                                                   
GETBUF3  DS    0H'0'                                                            
         MVC   SLDPT,DRBDPT                                                     
         CLI   SLDPT,X'FF'                                                      
         BNE   *+10                                                             
         MVC   SLDPT(3),=C'TOT'                                                 
         MVC   SLSDTE,=C'*TOTAL*'                                               
         MVI   P2,0                                                             
         CLC   DRDATE,=X'FFFFFFFF'                                              
         BE    GETBUF4                                                          
         GOTO1 DATCON,DMCB,(2,DRDATE),(5,SLSDTE)                                
         GOTO1 DATCON,DMCB,(2,DRDATE+2),(5,SLEDTE)                              
         MVI   P2,C' '                                                          
GETBUF4  L     RF,DRDOL                                                         
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,DRSPOTS                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(9,SLCPS),2                                                 
         EDIT  DRDOL,(9,SLTOTDL),2                                              
         OC    DRRESP,DRRESP                                                    
         BZ    GB4A                                                             
         L     RF,DRDOL                                                         
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,DRRESP                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(6,SLCPR),2                                                 
GB4A     L     RF,DRRTG                                                         
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(6,SLRTG)                                                   
         EDIT  DRRESP,(7,SLRSVP)                                                
         EDIT  DRVWRS,(7,SLIMPR),1                                              
         OC    DRVWRS,DRVWRS                                                    
         BZ    GB4                                                              
         L     RF,DRRESP                                                        
         SR    RE,RE                                                            
         M     RE,=F'100'                                                       
         SLDA  RE,1                                                             
         D     RE,DRVWRS                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(7,SLRPV),1                                                 
GB4      GOTO1 REPORT                                                           
         B     GETBUF1                                                          
         DROP  R4                                                               
         USING PLINED,R4                                                        
         SPACE 2                                                                
GETBUF5  MVC   PLDPT,DRBDPT                                                     
         CLI   PLDPT,X'FF'                                                      
         BNE   *+10                                                             
         MVC   PLDPT-2(3),=C'TOT'                                               
         MVC   PLTCAP(15),=C'*PERIOD TOTALS*'                                   
         MVC   PLTDLCAP,=C'TOTAL$='                                             
         EDIT  DRDOL,(8,PLTDLAMT),2,ALIGN=LEFT                                  
         MVC   PLTARCAP,=C'AVE RTG='                                            
         L     RF,DRRTG                                                         
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,DRSPOTS                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(2,PLTARAVE),,ALIGN=LEFT                                    
         L     RF,DRDOL                                                         
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,DRSPOTS                                                       
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
*        EDIT  (RF),(9,PLCPS),2                                                 
         OC    DRRESP,DRRESP                                                    
         BZ    GB5A                                                             
         L     RF,DRDOL                                                         
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,DRRESP                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(6,PLCPR),2                                                 
GB5A     L     RF,DRRTG                                                         
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
*        EDIT  (RF),(3,PLRTG)                                                   
         EDIT  DRRESP,(6,PLRSVP)                                                
         EDIT  DRVWRS,(7,PLIMPR),1                                              
         L     RF,DRRESP                                                        
         OC    DRVWRS,DRVWRS                                                    
         BZ    GB6                                                              
         SR    RE,RE                                                            
         M     RE,=F'100'                                                       
         SLDA  RE,1                                                             
         D     RE,DRVWRS                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(7,PLRPV),1                                                 
GB6      GOTO1 REPORT                                                           
         B     GETBUF1                                                          
         SPACE 2                                                                
DOSUM2   SR    R9,R9                                                            
         CLI   TOTSW,1                                                          
         BE    DOSUMX                                                           
         IC    R9,LEVEL                                                         
         L     R8,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(R8),(X'80',(R9))                         
         MVI   FORCEHED,C'Y'                                                    
DOSUMX   XIT1                                                                   
         SPACE 2                                                                
         DS    0D                                                               
         USING *,RF                                                             
SPHOOK   NTR1  BASE=SP60RB                                                      
         DROP  RF                                                               
         L     R2,SP60R2                                                        
         LM    RA,RC,SP60RA                                                     
         MVI   SPOTYORN,C'N'                                                    
         CLC   SPOTADDR,THISELEM                                                
         BNE   *+8                                                              
         MVI   SPOTYORN,C'Y'                                                    
         XIT1                                                                   
         EJECT                                                                  
* HEADLINE ROUTINES                                                             
         DS    0D                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=SP60RB                                                      
         DROP  RF                                                               
         L     R2,SP60R2                                                        
         LM    RA,RC,SP60RA                                                     
         CLI   RCSUBPRG,3          SET DEMOS IN HEADLINES                       
         BH    MHRC4                                                            
         MVC   H11+63(7),DNAME1                                                 
         MVC   H11+97(7),DNAME2                                                 
         MVC   H11+105(7),DNAME2                                                
         MVC   H12+105(7),=C'  CPM  '                                           
         MVC   H13+105(7),=C'-------'                                           
         B     MHRCX                                                            
MHRC4    MVC   H11+47(7),DNAME1                                                 
         MVC   H11+80(7),DNAME2                                                 
MHRCX    DS    0H'0'                                                            
         CLI   MODE,STALAST                                                     
         BH    MH1                                                              
         MVC   H4+50(7),=C'STATION'                                             
         MVC   H4+58(7),STAPRINT                                                
         B     MYHEADX                                                          
MH1      DS    0H'0'                                                            
MYHEAD4  DS    0H                                                               
MYHEADX  XIT1                                                                   
SP60RA   DC    F'0'                                                             
SP60RB   DC    F'0'                                                             
SP60RC   DC    F'0'                                                             
SP60R2   DC    F'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
EFRSTC   CSECT                                                                  
         NMOD1 0,EFRSTC                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SPDRWK,R2                                                        
         MVC   NOINGRID,=H'60'      FORCE 4 WEEK CHUNKS                         
         MVI   MEDEXTDM,4                                                       
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
EFPOLOK  DS    0C                                                               
         MVC   REASTART,QSTART                                                  
         MVC   PASSQST(12),QSTART                                               
         MVC   PASSTAB(12),QSTART                                               
* SET NUMBER OF LEVELS                                                          
         LA    RF,LVCNTRL                                                       
         LA    RE,6                                                             
         NI    0(RF),X'7F'                                                      
         LA    RF,4(RF)                                                         
         BCT   RE,*-8                                                           
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         L     RE,BUFFROWS                                                      
         BCTR  RE,0                                                             
         MH    RE,=H'4'                                                         
         LA    RE,LVCNTRL(RE)                                                   
         OI    0(RE),X'80'                                                      
         DROP  RF                                                               
         SPACE 2                                                                
* CREATE WEEKLY TABLES FOR ALL REPORTS                                          
         MVC   MEDNUMWK,=F'75'     SET UP MEDDATE                               
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMQT,=F'5'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDLCHNK,=F'128'                                                 
EFRSTA   DS    0H                                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
         MVC   SVRDTE,MEDPERD      SAVE REQUEST DATES                           
         MVI   PASS,0                                                           
         MVI   MAXPASS,1                                                        
         LA    RE,1                                                             
         LA    R6,PASSTAB                                                       
         L     R9,MEDAFRST                                                      
SETPASS  STC   RE,MAXPASS          SAVE HIGHEST PASS                            
         LH    R8,NOINGRID         SET TO NUMBER OF WEEKS IN PASS               
         GOTO1 DATCON,DMCB,(X'02',(R9)),(X'00',0(R6))                           
SETPASS1 GOTO1 DATCON,DMCB,(X'02',2(R9)),(X'00',6(R6))                          
SETPASS2 LA    R9,12(R9)                                                        
         C     R9,MEDALAST                                                      
         BH    SETPASSX                                                         
         CLI   0(R9),0                                                          
         BE    SETPASS2                                                         
         BCT   R8,SETPASS1                                                      
         ZIC   RE,MAXPASS                                                       
         LA    RE,1(RE)            BUMP MAXPASS                                 
         LA    R6,12(R6)           BUMP DATE SAVE                               
         B     SETPASS                                                          
SETPASSX MVC   PASSQST(12),PASSTAB                                              
EFRSTX   MVC   QSTART(12),PASSQST                                               
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
         EJECT                                                                  
SORTC    CSECT                                                                  
         NMOD1 0,SORTC                                                          
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         USING SPDRWK,R2                                                        
         CLI   SORTPASS,1          INPUT PHASE                                  
         BNE   SORTOUT              NO - DO OUTPUT                              
         OC    SSCNTR,SSCNTR                                                    
         BNZ   SORTIN1                                                          
         L     RE,VSSTABLE                                                      
         L     RF,=F'20000'                                                     
         XCEF                                                                   
SORTIN1  DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING PNAMD,RE                                                         
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         MVC   HALF,BDTIMST        CALCULATE START/END QTR HR                   
         LH    RF,HALF                                                          
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         MH    RF,=H'4'                                                         
         LR    R0,RF                                                            
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         AR    R0,RF                                                            
         STC   R0,FULL                                                          
         MVC   HALF,BDTIMEND                                                    
         LH    RF,HALF                                                          
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         MH    RF,=H'4'                                                         
         LR    R0,RF                                                            
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         AR    R0,RF                                                            
         MVC   SORTKLEN,=F'10'                                                  
         MVC   SORTRLEN,=F'14'                                                  
         MVC   DADRDISP,=F'6'                                                   
         LA    R1,WORK                                                          
         USING SQSTART,R1                                                       
         XC    WORK,WORK                                                        
         MVC   FULL+2(1),BDDAY                                                  
         LA    R5,24(R5)                                                        
         USING REGELEM,R5                                                       
SIN1     CLI   0(R5),0             END OF RECORD                                
         BE    SORTCX                                                           
         CLI   0(R5),6                                                          
         BL    SORTADD2                                                         
         CLI   0(R5),13                                                         
         BH    SORTADD2                                                         
         CLC   RDATE,SVRDTE                                                     
         BL    SORTADD2                                                         
         CLC   RDATE,SVRDTE+2                                                   
         BH    SORTADD2                                                         
         TM    RSTATUS,X'40'       BYPASS MISSED AND PREEMPT                    
         BO    SORTADD2                                                         
         TM    RSTATUS,X'80'       BYPASS MINUS SPOTS                           
         BO    SORTADD2                                                         
         TM    RSTATUS,X'04'       BYPASS HIATUS                                
         BO    SORTADD2                                                         
         MVC   SQ1DATE,RDATE                                                    
         MVC   SQ1DAY,FULL+2       NOVE IN BDDAY                                
         MVC   SQ1TIME,FULL                                                     
         STCM  R5,15,SQ1ELADR                                                   
         MVC   SQ1DADDR,KEY+14                                                  
         MVI   SQ1DPT,C'Z'                                                      
         LR    R6,R5               LOOK FOR AFFID                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'10'         NO AFFID                                     
         BNE   SORTADD              INSERT UNDER Z                              
         MVI   SQ1DPT,C'X'                                                      
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'17'         NO RSVP                                      
         BNE   SORTADD              INSERT UNDER X                              
         USING RSVPELEM,R6                                                      
         MVC   SQ1DPT,RSVPDPT                                                   
         SPACE 2                                                                
* ADD A RECORD TO THE SORT BUFFER                                               
SORTADD  L     RF,SSCNTR                                                        
         SR    RE,RE                                                            
         M     RE,SORTRLEN                                                      
         A     RF,VSSTABLE                                                      
         MVC   0(20,RF),WORK                                                    
         L     RF,SSCNTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,SSCNTR                                                        
         MVI   SOUTFRST,1                                                       
SORTADD2 ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     SIN1                                                             
         EJECT                                                                  
SORTOUT  CLI   SOUTFRST,1                                                       
         BNE   SRTOUT1                                                          
         L     R4,VSSTABLE                                                      
         L     R5,SSCNTR                                                        
         L     R6,SORTRLEN                                                      
         L     R7,SORTKLEN                                                      
         GOTO1 XSORT,DMCB,(R4),(R5),(R6),(R7),0                                 
         MVI   SOUTFRST,0                                                       
         MVC   NEXTSSLT,VSSTABLE                                                
         L     RE,ADBUY                                                         
         ST    RE,AREC                                                          
         SPACE 2                                                                
SRTOUT1  L     RE,NEXTSSLT                                                      
         L     R1,NEXTSSLT                                                      
         A     RE,DADRDISP                                                      
         MVC   THISELEM,SQ1ELADR                                                
         MVC   KEY+14(4),0(RE)                                                  
         OC    KEY+14(4),KEY+14                                                 
         BNZ   SRTOUT2                                                          
         MVI   SORTPASS,3                                                       
         MVI   SOUTFRST,1                                                       
         XC    SSCNTR,SSCNTR                                                    
         B     SORTCX                                                           
SRTOUT2  L     RE,ADBUY            GET A BUY RECORD                             
         ST    RE,AREC                                                          
         CLC   KEY+14(4),PREVDADR                                               
         BE    SRTOUT3                                                          
         MVC   PREVDADR,KEY+14                                                  
         GOTO1 GET                                                              
SRTOUT3  L     RE,ADBUY                                                         
         MVC   KEY(13),0(RE)                                                    
         L     RE,NEXTSSLT                                                      
         ST    RE,THISSLT                                                       
         A     RE,SORTRLEN         BUMP TO NEXT SLOT                            
         ST    RE,NEXTSSLT                                                      
         MVI   SORTPASS,2                                                       
SORTCX   XMOD1 1                                                                
         LTORG                                                                  
SORTKLEN DC    F'0'                SORT KEY LENGTH                              
SORTRLEN DC    F'0'                SORT RECORD LENGTH                           
DADRDISP DC    F'0'                DISK ADDRESS DISPLACEMENT                    
NEXTSSLT DC    F'0'                NEXT SORT SLOT                               
         BUFF  LINES=500,ROWS=6,COLUMNS=5,FLAVOR=BINARY,KEYLIST=(8,A)           
         EJECT                                                                  
SPDRWK   CSECT                                                                  
BUFHI    DS    C                                                                
BUFCDE   DS    C                                                                
BUFRTYP  DS    C                                                                
LEVEL    DS    C                                                                
ESTACT   DS    C                                                                
TOTSW    DS    C                                                                
BYDAYS   DS    F                   SAVE NUMBER OF DAYS FROM BDDAY               
SVBYDAY  DS    CL2                 SAVE DATE FROM REGELEM                       
RELO     DS    F                                                                
LVCNTRL  DC    F'1'                                                             
         DC    A(2,3,4,5,6)                                                     
DNAMES   DS    0CL28                                                            
DNAME1   DS    CL7                                                              
DNAME2   DS    CL7                                                              
DNAME3   DS    CL7                                                              
DNAME4   DS    CL7                                                              
MYBUFIO  DS    CL200                                                            
FIRST    DS    C                                                                
PASS     DS    C                                                                
MAXPASS  DS    C                                                                
VSUMMARY DS    F                                                                
HPSNO    DS    C                                                                
PRVDRB   DS    C                                                                
LASTGSLT DS    F                                                                
SVRDTE   DS    F                                                                
VMDADDWT DS    F                                                                
PGHILLN  DS    F                                                                
PGNOENT  DS    F                                                                
PGCNDSW  DS    C                                                                
WEIGHT   DS    F                                                                
VSUMMRY  DS    F                                                                
NUMWK    DS    F                                                                
PRTLINE  DS    CL132                                                            
STRDTE   DS    CL2                                                              
ENDDTE   DS    CL2                                                              
BRDPOLSW DS    CL1                                                              
VPGRID   DS    F                                                                
VFOOT    DS    F                                                                
VARFRMT  DC    X'00'                                                            
NODEMS   DS    F                                                                
DDESC    DS    F                                                                
VSTATOT  DS    F                                                                
PGWMAX   DC    F'0'                                                             
DSTAGRID DC    F'0'                                                             
PGELEM   DS    CL20                                                             
PDNCNTR  DC    F'0'                                                             
SOUTFRST DC    X'01'                                                            
PREVDADR DS    F                                                                
PGCURLNO DS    F                                                                
BUYACT   DS    F                                                                
MKTACT   DS    F                                                                
VPLAREA  DS    F                                                                
VMRGPL   DS    F                                                                
NOINGRID DS    H                                                                
LENGRID  DS    C                                                                
HLDNOSP  DS    C                                                                
THISSLT  DC    F'0'                CURRENT SORT SLOT                            
CURRSORT DS    F                                                                
NEXTSORT DS    F                                                                
VPNTABLE DS    F                                                                
VPDNCNTR DS    F                                                                
VSSTABLE DS    F                                                                
SSCNTR   DS    F                                                                
VRSORT   DS    F                                                                
THISELEM DS    F                                                                
THISAFFD DS    F                                                                
SORTPASS DS    C                                                                
SORTREQ  DS    C                                                                
SORTFRMT DS    C                                                                
CURRPNUM DS    C                                                                
BYPSUM   DC    X'00'                                                            
NUMSTA   DC    X'00'                                                            
PASSTAB  DS    CL144               LIST OF PASS START-END DATES                 
PASSQST  DS    CL12                THIS PASS START-END                          
REASTART DS    CL12                REQUEST START-END DATES                      
         EJECT                                                                  
PTSGD    DSECT                                                                  
GRIDST   DS    F                   START OF GRID                                
PRPGWMAX DS    F                                                                
PRPGDWK  DS    CL2                 PREVIOUS BLOCK DATES                         
PGPRLNO  DS    CL3                 PREVIOUS LINE/SUN LINE/SLOT                  
PGWNOL   DS    C                   NUMBER OF LINES IN THIS BLOCK                
GRIDLEN  DS    C                                                                
GRIDSW1  DS    C                                                                
GRIDSLN  DS    C                                                                
PGWKCNT  DS    F                   WEEKLY SLOT COUNTER                          
         SPACE 2                                                                
DRBUFD   DSECT                                                                  
DRBCODE  DS    C                   1=WK,2=MO,3=QTR,4=PERIOD                     
DRBDPT   DS    C                   DAYPART CODE                                 
         DS    CL2                                                              
DRDATE   DS    CL4                                                              
DRDOL    DS    CL4                                                              
DRSPOTS  DS    CL4                                                              
DRRESP   DS    CL4                                                              
DRRTG    DS    CL4                                                              
DRVWRS   DS    CL4                                                              
         SPACE 2                                                                
PLINED   DSECT                                                                  
PLBASE   DS    0C                                                               
         DS    CL3                                                              
PLDPT    DS    CL1                                                              
         ORG   PLBASE+8                                                         
PLTIME   DS    CL5                                                              
         ORG   PLBASE+20                                                        
PLDAY    DS    CL3                                                              
         ORG   PLBASE+29                                                        
PLDATE   DS    CL8                                                              
         ORG   PLBASE+38                                                        
PLPROG   DS    CL14                                                             
         ORG   PLBASE+55                                                        
PLCPS    DS    CL9                                                              
         ORG   PLBASE+65                                                        
PLRTG    DS    CL3                                                              
         ORG   PLBASE+72                                                        
PLRSVP   DS    CL5                                                              
         ORG   PLBASE+79                                                        
PLCPR    DS    CL6                                                              
         ORG   PLBASE+86                                                        
PLRPV    DS    CL7                                                              
         ORG   PLBASE+94                                                        
PLIMPR   DS    CL7                                                              
         ORG   PLBASE+105                                                       
PLCPM    DS    CL7                                                              
         ORG   PLBASE+5                                                         
PLTCAP   DS    CL14                                                             
         DS    CL2                                                              
PLTDLCAP DS    CL7                                                              
PLTDLAMT DS    CL8                                                              
         DS    CL2                                                              
PLTARCAP DS    CL8                                                              
PLTARAVE DS    CL2                                                              
         EJECT                                                                  
SLINED   DSECT                                                                  
SLBASE   DS    0C                                                               
         DS    CL3                                                              
SLDPT    DS    CL1                                                              
         ORG   SLBASE+8                                                         
SLSDTE   DS    CL8                                                              
         DS    CL1                                                              
SLEDTE   DS    CL8                                                              
         DS    CL1                                                              
SLTOTDL  DS    CL9                                                              
         DS    CL1                                                              
SLCPS    DS    CL9                                                              
         DS    CL1                                                              
SLRTG    DS    CL6                                                              
         DS    CL3                                                              
SLRSVP   DS    CL7                                                              
         DS    CL1                                                              
SLCPR    DS    CL6                                                              
         DS    CL1                                                              
SLRPV    DS    CL7                                                              
         DS    CL3                                                              
SLIMPR   DS    CL7                                                              
         EJECT                                                                  
SSTABLE  CSECT                                                                  
         DS    20000C                                                           
         LTORG                                                                  
         EJECT                                                                  
BPRTD    DSECT                                                                  
BPRTSPT  DS    CL10                 0                                           
BPRTD1   DS    CL7                 11                                           
BPRTDL   DS    CL9                 18                                           
BPRTD1C  DS    CL8                 27                                           
BPRTD2   DS    CL7                 35                                           
BPRTD2C  DS    CL8                 43                                           
BPRTD3   DS    CL7                 51                                           
BPRTD3C  DS    CL8                 58                                           
BPRTD4   DS    CL7                 68                                           
BPRTD4C  DS    CL8                 76                                           
         EJECT                                                                  
PGRIDD   DSECT                                                                  
PGSORT   DS    CL11                                                             
PGLINNO  DS    C                   PRINT BLOCK LINE NUMBER                      
PGSUBLI  DS    C                   SUB-LINE NUMBER                              
PGLSLOT  DS    C                   PRINT BLOCK SLOT NUMBER                      
PGDWK    DS    CL2                 WEEK OF                                      
PGDIND   DS    CL1                 REG/MISSD/MG INDICATOR                       
PGDSBRN  DS    CL1                 SORT BRAND                                   
PGDSSLN  DS    CL1                 SORT SPOT LENGTH                             
PGDSNO   DS    CL1                 SORT SPOT NUMBER                             
PGDELAD  DS    CL4                 ELEMENT ADDRESS                              
PGDFDAY  DS    CL1                                                              
PGDFNO   DS    CL2                                                              
PGDNOSP  DS    CL1                 NUMBER OF SPOTS                              
PGD2BRN  DS    CL1                 PIGGYBACK BRAND                              
PGD2SLN  DS    CL1                 PIGGYBACK SPOT LENGTH                        
PGDEND   DS    0C                                                               
PGDLEN   EQU   PGDEND-PGSORT                                                    
PGDLN1   EQU   PGDELAD-PGLINNO                                                  
PGSRTLN  EQU   L'PGSORT                                                         
         SPACE 2                                                                
PGSRT1D  DSECT                                                                  
PGDS1WK  DS    CL2                                                              
PGDS1DY  DS    C                                                                
PGDS1SLT DS    C                                                                
PGDS1BR  DS    CL1                                                              
PGDS1SL  DS    CL1                                                              
PGDS1IND DS    CL1                                                              
         SPACE 2                                                                
PGSRT2D  DSECT                                                                  
PGDS2SLT DS    CL1                                                              
PGDS2WK  DS    CL2                                                              
PGDS2DY  DS    C                                                                
PGDS2SNO DS    C                                                                
PGDS2BR  DS    CL1                                                              
PGDS2SL  DS    CL1                                                              
PGDS2IND DS    CL1                                                              
         EJECT                                                                  
PNAMD    DSECT                                                                  
PNDCODE  DS    CL1                 PROGRAM NUMBER                               
PNDNAME  DS    CL17                                                             
         SPACE 2                                                                
SEQSORT  DSECT                                                                  
SQSTART  DS    0C                                                               
SQ1DPT   DS    CL1                 DAYPART                                      
SQ1DATE  DS    CL2                 DATE                                         
SQ1DAY   DS    CL1                 DAY                                          
SQ1TIME  DS    CL2                 TIME                                         
SQ1DADDR DS    CL4                 DISKADDRESS                                  
SQ1ELADR DS    CL4                 ELEMENT ADDRESS                              
SQ1END   DS    0C                                                               
         ORG   SQSTART                                                          
SQ2TIME  DS    CL2                                                              
SQ2DAY   DS    CL1                                                              
SQ2PNUM  DS    CL1                                                              
SQ2DADDR DS    CL4                                                              
SQ2END   DS    0C                                                               
         EJECT                                                                  
SUMDSECT DSECT                                                                  
SUMKEY   DS    0CL15                                                            
SUMCODE  DS    CL1                 X'90'                                        
SUMDPGNO DS    CL1                 DAYPART GROUP NO.                            
SUMDPGRP DS    CL3                 DAYPART GROUP CODE                           
SUMDPNO  DS    CL1                 DAYPART NO.                                  
SUMDPART DS    CL3                 DAYPART CODE                                 
SUMSLN   DS    CL1                 SPOT LENGTH                                  
SUMRTYP  DS    CL1                 1=WEEKLY,2=MONTHLY,3=PERIOD                  
SUMDT    DS    CL4                 START-END DATES(FFFF FOR TOTAL)              
SUMRPT   DS    CL1                 REPORT CODE                                  
SUMDATA  DS    0CL60                                                            
SUMSPOTS DS    CL4                 SPOTS                                        
SUMDL    DS    CL4                 DOLLARS                                      
SUMDLEQ  DS    CL4                 DOLLARS EQU                                  
SUMD1    DS    CL4                 DEMO 1                                       
SUMD1EQ  DS    CL4                 DEMO 1 EQU                                   
SUMD2    DS    CL4                 DEMO 2                                       
SUMD2EQ  DS    CL4                 DEMO 2 EQU                                   
SUMD3    DS    CL4                 DEMO 3                                       
SUMD3EQ  DS    CL4                 DEMO 3 EQU                                   
SUMD4    DS    CL4                 DEMO 4                                       
SUMD4EQ  DS    CL4                 DEMO 4 EQU                                   
SUMGDL   DS    CL4                 GOAL $                                       
SUMGDLE  DS    CL4                 GOAL $ EQU                                   
SUMGD1   DS    CL4                 GOAL DEMO                                    
SUMGD1E  DS    CL4                 GOAL DEMO EQU                                
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENADD                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPMEDBDESD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDCOMFACS                                                      
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'079SPREPDR02 05/01/02'                                      
         END                                                                    
