*          DATA SET PPBUY07    AT LEVEL 047 AS OF 04/20/16                      
*PHASE T41107A                                                                  
*INCLUDE OUTER                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPBUY07 - TEARSHEET RECALL - DISPLAY'                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* NEW LIMIT ACCESS VALUES                                                       
* T411FFD+12  X'02' = WESTERN BUYER  BUYBILL = B                                
*             X'04' = WESTERN BILLER  BUYBILL = L                               
*             X'08' = NO TEARSHEET CHANGES                                      
*                                                                               
* NOTE: IF BOTH X'02' AND X'04' ARE ON                                          
*       BUYBILL SET TO 'X'                                                      
*       ALLOW BOTH BUYER AND BILLER ACCESS                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 04/12/16 LINE NUMBER FIX FOR PBU                                         
*                                                                               
* SMYE 6/17/10  DELETE REFERENCE TO POWER CODES NO LONGER ON DDS SYSTEM         
*                                                                               
* SMYE 08/25/05 CHANGES FOR AD-ID IN FMTJOB                                     
*                                                                               
* KWAN 03/05/01 RELINK WITH MODIFIED PPBUYWRK1 (4000K BUY REC)                  
*                                                                               
* BPLA 01/98    NO LONGER TREAT SJR AS A WESTERN AGENCY                         
*                                                                               
* BPLA 03/97    ADD AGENCY MX FOR WESTERN FEATURE                               
*                                                                               
* BPLA 03/96    CHANGE TO ALLOW 4 T/S COMMENTS                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41107   CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,*T41107*                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         LA    R8,4095(RB)                                                      
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         LA    R8,BUYHDH                                                        
         USING PPF4D,R8            TEARSHEET LOWER SCREEN                       
*                                                                               
         MVI   BUYBILL,0                                                        
*                                                                               
         CLC   AGYALPHA,=C'WJ'     SEE IF WESTERN - TEST                        
         BE    WEST                                                             
         CLC   AGYALPHA,=C'WR'     SEE IF WESTERN  WRLA                         
         BE    WEST                                                             
         CLC   AGYALPHA,=C'WI'     SEE IF WESTERN                               
         BNE   NOTWEST                                                          
WEST     MVC   TSHSTOK+7(18),=CL18'(A,I,M,R)'                                   
         FOUT  TSHSTOKH                                                         
*                                                                               
         TM    T411FFD+12,X'06'                                                 
         BNO   WEST3                                                            
         MVI   BUYBILL,C'X'        BOTH BUYER AND BILLER                        
         B     NOTWEST                                                          
*                                                                               
WEST3    TM    T411FFD+12,X'02'                                                 
         BNO   WEST5                                                            
         MVI   BUYBILL,C'B'        BUYER                                        
         B     NOTWEST                                                          
*                                                                               
WEST5    TM    T411FFD+12,X'04'    BILLER                                       
         BNO   NOTWEST                                                          
         MVI   BUYBILL,C'L'                                                     
*                                                                               
NOTWEST  DS    0H                                                               
         RELOC RELO05              CAN USE REL05                                
*                                                                               
         XC    SVINS,SVINS         CLEAR INSERTION DISPLAY TABLE                
         XC    KEY,KEY                                                          
         MVC   TRCODE,BUYTR1       SAVE RECALL MODE                             
*                                                                               
* CLEAR DISPLAY AREA OF SCREEN                                                  
* MUST BE SURE FIELDS ARE UNPROTECTED                                           
* SINCE THEY MAY HAVE BEEN PROTECTED BY LAST DISPLAY                            
*                                                                               
         NI    TSHSTATH+1,X'DF'                                                 
         NI    TSHTS1H+1,X'DF'                                                  
         NI    TSHTS2H+1,X'DF'                                                  
         NI    TSHTS3H+1,X'DF'                                                  
         NI    TSHTS4H+1,X'DF'                                                  
         NI    TSHREPOH+1,X'DF'                                                 
         NI    TSHTS5H+1,X'DF'                                                  
         NI    TSHPAGEH+1,X'DF'                                                 
         NI    TSHCOM1H+1,X'DF'                                                 
         NI    TSHCOM2H+1,X'DF'                                                 
         NI    TSHCOM3H+1,X'DF'                                                 
         NI    TSHCOM4H+1,X'DF'                                                 
*                                                                               
         LA    R4,TSHADCH          POINT TO AD CODE                             
*                                                                               
CLRSC1   SR    R5,R5                                                            
CLRSC2   IC    R5,0(R4)                                                         
         AHI   R5,-9                                                            
         TM    1(R4),X'20'         SEE IF PROTECTED FIELD                       
         BZ    CLRSC3                                                           
         LA    RE,TSHPOS5H                                                      
         CR    R4,RE               SEE IF PAST POSITION                         
         BH    CLRSC4              IF SO LEAVE PROTECTED FIELDS ALONE           
         LA    RE,TSHCAPAH                                                      
         CR    R4,RE               POINTING TO CAPTION ANNOTATION?              
         BE    CLRSC4                                                           
         LA    RE,TSHPOSAH                                                      
         CR    R4,RE               POINTING TO POSITION ANNOTATION?             
         BE    CLRSC4                                                           
*                                                                               
CLRSC3   EX    R5,CLROC                                                         
         BZ    CLRSC4                                                           
         EX    R5,CLRXC                                                         
         FOUT  (R4)                                                             
         TM    1(R4),X'20'         SEE IF PROTECTED                             
         BNZ   CLRSC4                                                           
         MVI   5(R4),0             ZERO INPUT LENGHT                            
*                                                                               
CLRSC4   LA    R4,9(R4,R5)         POINT TO NEXT FIELD                          
         CLI   0(R4),0                                                          
         BNE   CLRSC2                                                           
         B     DSPL                                                             
CLRXC    XC    8(0,R4),8(R4)                                                    
CLROC    OC    8(0,R4),8(R4)                                                    
*                                                                               
DSPL     DS    0H                                                               
         LA    R2,BUYTR1H                                                       
         ST    R2,TRADDR           SET FIRST TRADDR AND R2.                     
         SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         AR    R4,R2               POINT TO INSERTION DATE                      
DSPL3    BAS   R9,EDTINS           EDIT INSERTION DATE                          
*                                                                               
         BAS   R9,NXTINS                                                        
         BNE   NOINS                                                            
         BAS   R9,FMTTR                                                         
         BAS   R9,FMTINS                                                        
         BAS   R9,FMTJOB                                                        
         CLI   BUYMD,C'N'          SEE IF NEWSPAPERS                            
         BE    DSPL3N                                                           
         CLI   BUYMD,C'O'          SEE IF OUTDOOR                               
         BE    DSPL3O                                                           
         BAS   R9,FMTSP                                                         
         B     DSPL6                                                            
*                                                                               
DSPL3N   BAS   R9,FMTLNS                                                        
         B     DSPL6                                                            
*                                                                               
DSPL3O   BAS   R9,FMTOSPC          FORMAT OUTDOOR SPACE                         
*                                                                               
DSPL6    DS    0H                                                               
         BAS   R9,FMTCOPY          FORMAT COPY                                  
         BAS   R9,FMTMCL           FORMAT MATERIAL CLOSING DATE                 
         BAS   R9,BUMPFLD          PAST CAPTION ANNO                            
         BAS   R9,FMTCAP           FORMAT CAPTIONS                              
         BAS   R9,BUMPFLD2         POSITION ANNO +1                             
         BAS   R9,FMTPOS           FORMAT POSITION INSTRUCTIONS                 
         BAS   R9,BUMPFLD2         TEARSHEET ANNO (2 FIELDS)                    
         BAS   R9,BUMPFLD                                                       
         BAS   R9,FMTTEAR          FORMAT TEARSHEET INFO                        
         BAS   R9,BUMPFLD2                                                      
         BAS   R9,FMTTCOM          FORMAT TEARSHEET COMMENTS                    
         B     DSPL10                                                           
         EJECT                                                                  
*                                                                               
NXTINS   DS    0H                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         CLI   KEY,0               TEST FIRST TIME                              
         BNE   NXTINS2                                                          
         BRAS  RE,SETBYKEY                                                      
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   NXTINSX                                                          
         CLI   KEY+25,X'FF'                                                     
         BNE   NXTINS2+4                                                        
         LTR   RE,RE                                                            
         B     NXTINSX                                                          
NXTINS2  BAS   RE,SEQ                                                           
         CLI   KEY+25,X'FF'                                                     
         BE    NXTINS2                                                          
         CLC   KEY(16),KEYSAVE     TEST SAME THRU PUB                           
         BNE   NXTINSX                                                          
*                                                                               
         CLC   KEY+19(2),BEST      TEST RIGHT EST                               
         BNE   NXTINS2                                                          
         OC    KEY+21(3),KEY+21    TEST ACTIVE                                  
         BNZ   NXTINS2             NO                                           
         BAS   RE,GETREC                                                        
*                                                                               
NXTINS2X DS    0H                  SAVE DATA IN SVINS LIST                      
NXTINS3  LA    R0,DUMEL                                                         
         C     R0,TRADDR           TEST FOR DUMMY LINE                          
         BE    NXTINSX                                                          
         LA    R1,SVINS            FIND A SLOT                                  
         OC    0(6,R1),0(R1)                                                    
         BZ    *+12                                                             
         LA    R1,6(R1)                                                         
         B     *-14                                                             
         L     R0,TRADDR           GET REL TWA ADDR                             
         SR    R0,RA                                                            
         STH   R0,0(R1)                                                         
*                                                                               
NXTINS4  DS    0H                                                               
         MVC   2(4,R1),KEY+27      SAVE DISK ADDRESS                            
         CR    R2,R2               SET CC                                       
*                                                                               
NXTINSX  DS    0H                                                               
         LA    R1,1                PRESERVE CC                                  
         BNZ   *+6                                                              
         SR    R1,R1                                                            
         NI    DMINBTS,X'F7'       RESET DELETES                                
*                                                                               
         LTR   R1,R1                                                            
         BR    R9                                                               
         EJECT                                                                  
*                                                                               
BUMPFLDS SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,BUMPFLDS                                                      
         BR    R9                                                               
*                                                                               
BUMPFLD2 SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
BUMPFLD  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BR    R9                                                               
*                                                                               
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                                                             
         BCTR  R1,0                                                             
         EX    R1,VARMOVE                                                       
         BR    RE                                                               
*                                                                               
VARMOVE  MVC   WORK(0),8(R2)                                                    
*                                                                               
FMTTR    GOTO1 VFMTTR,DMCB,REC                                                  
         BR    R9                                                               
*                                                                               
FMTINS   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 VFMTINS,DMCB,REC                                                 
         OI    4(R2),X'20'         SET VALIDATED BIT                            
         BR    R9                                                               
*                                                                               
FMTOSPC  DS    0H                  FORMAT OUTDOOR SPACE                         
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 =V(OUTER),DMCB,(0,PBUYREC),(1,8(R2)),RR=RELO05                   
         OI    4(R2),X'20'         SET VALIDATED BIT                            
         BR    R9                                                               
         EJECT                                                                  
*                                                                               
FMTSP    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         FOUT  (R2),PBDSPACE                                                    
         OI    4(R2),X'20'                                                      
         BR    R9                                                               
*                                                                               
FMTJOB   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(6,R2),PBDJOB                                                   
         FOUT  (R2)                                                             
         CLI   PBDJOB,X'FF'        AD-ID ALONE (NO "REAL" JOB CODE) ?           
         BNE   *+10                NO                                           
         MVC   8(6,R2),=C'*ADID*'                                               
         BR    R9                                                               
*                                                                               
FMTCAP   DS    0H                  FORMAT CAPTION                               
         ST    R9,SAVER9                                                        
         ST    R2,FULL             SAVE R2 IN FULL                              
*                                                                               
         MVI   DUB,0                                                            
         LA    R5,PBUYREC+33                                                    
         MVI   ELCODE,X'66'        FIRST CHECK FOR OVERRIDES                    
         LA    R6,2                2 LINES                                      
*                                                                               
FMTCAP5  DS    0H                                                               
         BAS   R9,NEXTEL                                                        
         BNE   FMTCAP8                                                          
         CLC   2(4,R5),=C'CAP='                                                 
         BNE   FMTCAP5                                                          
         OI    DUB,X'01'           CAPTION OVERRIDE FOUND                       
         ZIC   R1,1(R5)                                                         
         AHI   R1,-7               FOR EXECUTE                                  
         BNP   FMTCAP6                                                          
         BAS   R9,BUMPFLD                                                       
         LA    R5,4(R5)            TO GET PAST 'CAP='                           
         EX    R1,MVCOM                                                         
         AHI   R5,-4               MUST RESET TO START OF ELEM                  
         FOUT  (R2)                                                             
*                                                                               
FMTCAP6  DS    0H                                                               
         BCT   R6,FMTCAP5                                                       
         B     FMTCAPX                                                          
*                                                                               
FMTCAP8  DS    0H                                                               
         CLI   DUB,0               SEE IF OVERRIDES FOUND                       
         BNE   FMTCAPX             YES                                          
         CLC   PBDJOB,=6C' '       NO JOB CODE                                  
         BNH   FMTCAPX                                                          
*                                                                               
         BAS   R9,BUMPFLD                                                       
         BAS   R9,GETJOB           GO READ JOBREC INTO JOBIO                    
         L     R6,AJOBIO                                                        
         USING PJOBREC,R6                                                       
         MVC   8(25,R2),PJOBCAP1                                                
         FOUT  (R2)                                                             
         BAS   R9,BUMPFLD                                                       
         MVC   8(25,R2),PJOBCAP2                                                
         FOUT  (R2)                                                             
         DROP  R6                                                               
*                                                                               
FMTCAPX  DS    0H                                                               
         L     R2,FULL             RESTORE R2                                   
         BAS   R9,BUMPFLD2                                                      
         L     R9,SAVER9                                                        
         BR    R9                                                               
*                                                                               
FMTCOPY  DS    0H                  FORMAT COPY                                  
         ST    R9,SAVER9                                                        
         BAS   R9,BUMPFLD                                                       
         LA    R5,PBUYREC+33                                                    
         MVI   ELCODE,X'66'        FIRST CHECK FOR OVERRIDES                    
FMTCPY5  BAS   R9,NEXTEL                                                        
         BNE   FMTCPY8                                                          
         CLC   2(5,R5),=C'COPY='                                                
         BNE   FMTCPY5                                                          
         ZIC   R1,1(R5)                                                         
         AHI   R1,-8               FOR EXECUTE                                  
         BNP   FMTCPYX                                                          
         LA    R5,5(R5)            TO GET PAST 'COPY='                          
         EX    R1,MVCOM                                                         
         AHI   R5,-5               MUST RESET TO START OF ELEM                  
         B     FMTCPYX                                                          
*                                                                               
FMTCPY8  DS    0H                                                               
         CLC   PBDJOB,=6C' '                                                    
         BNH   FMTCPYX                                                          
*                                                                               
         BAS   R9,GETJOB           GO READ JOBREC INTO JOBIO                    
         L     R6,AJOBIO                                                        
         USING PJOBREC,R6                                                       
         MVC   8(17,R2),PJOBCPY                                                 
         DROP  R6                                                               
*                                                                               
FMTCPYX  L     R9,SAVER9                                                        
         FOUT  (R2)                                                             
         BR    R9                                                               
*                                                                               
FMTMCL   DS    0H                                                               
         LA    R3,PBDMDATE         MATERIALS CLOSING DATE                       
         B     FMTDATE                                                          
*                                                                               
FMTDATE  SR    R0,R0               POINT TO NEXT FIELD                          
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    8(5,R2),8(R2)                                                    
         OC    0(3,R3),0(R3)                                                    
         BCR   8,R9                                                             
         GOTO1 VDATCON,DMCB,(3,(R3)),(7,8(R2))                                  
         FOUT  (R2)                                                             
         BR    R9                                                               
*                                                                               
FMTLNS   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R4,8(R2)                                                         
         XC    0(8,R4),0(R4)                                                    
         LA    R5,X                                                             
         XC    X(10),X                                                          
*                                                                               
         CLC   PBDSPACE(2),=X'7B00'                                             
         BE    FMTLNS2                                                          
         CLC   PBDSPACE(2),=C'# '  # AND SPACE- TREAT AS NONE-SPACE             
         BE    FMTLNS2             BUY                                          
         CLC   PBDSPACE(2),=C'* '  TEST SPACE BUY                               
         BH    FMTLNS4             YES                                          
*                                                                               
         CLC   PBDSPACE(2),=C'$$'  FOR JWT DELETES                              
         BE    FMTLNS4             YES                                          
*                                                                               
FMTLNS2  DS    0H                                                               
         CLI   PBDSPACE,C'*'                                                    
         BNE   *+12                                                             
         MVI   0(R5),C'*'                                                       
         LA    R5,1(R5)                                                         
         CLI   PBDSPACE,C'#'       SPECIAL FOR NO ASC CHECKING                  
         BNE   *+12                                                             
         MVI   0(R5),C'#'                                                       
         LA    R5,1(R5)                                                         
         LR    RF,R9                                                            
         ZAP   DUB,PBDUNITS                                                     
         BNZ   *+16                                                             
         MVI   0(R5),C'0'                                                       
         LA    R5,1(R5)                                                         
         B     *+10                                                             
         BAS   R9,EDALIN                                                        
         AR    R5,R0                                                            
*                                                                               
         MVC   0(1,R5),PBDUIND                                                  
         OI    0(R5),X'40'         TO SET X'89' TO X'C9'                        
         LA    R5,1(R5)                                                         
*                                                                               
         OC    PBDCLMS,PBDCLMS                                                  
         BNZ   *+10                                                             
         ZAP   PBDCLMS,=P'0'                                                    
         ZAP   DUB,PBDCLMS                                                      
         BZ    FMTLNS3                                                          
         MVI   0(R5),C'/'                                                       
         LA    R5,1(R5)                                                         
         BAS   R9,EDAL                                                          
         AR    R5,R0                                                            
FMTLNS3  LA    R0,X                                                             
         SR    R5,R0                                                            
         CHI   R5,8                                                             
         BNH   *+8                                                              
         LA    R5,8                                                             
         LR    R9,RF                                                            
         MVC   0(8,R4),X                                                        
         STC   R5,5(R2)                                                         
         FOUT  (R2)                                                             
         BR    R9                                                               
*                                                                               
FMTLNS4  DS    0H                                                               
         MVC   8(8,R2),PBDSPACE                                                 
         FOUT  (R2)                                                             
         BR    R9                                                               
*                                                                               
EDALIN   DS    0H                                                               
         CLI   PBDUIND,X'89'                                                    
         BNE   EDAL                                                             
*                                                                               
EDALIN2  DS    0H                                                               
         EDIT  (P8,DUB),(6,0(R5)),2,ALIGN=LEFT                                  
         BR    R9                                                               
*                                                                               
EDAL     DS    0H                                                               
         EDIT  (P8,DUB),(5,0(R5)),ALIGN=LEFT                                    
         BR    R9                                                               
         EJECT                                                                  
*                                                                               
FMTPOS   DS    0H                  FORMAT POSITION COMMENTS                     
         ST    R9,SAVER9           STORE RETURN REGISTER                        
         LA    R4,5                FOR BCT                                      
         MVI   ELCODE,X'68'                                                     
         LA    R5,REC+33                                                        
FMTPOS1  BAS   R9,NEXTEL                                                        
         BC    7,FMTPOS2           GO BUMP LINE                                 
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AHI   R1,-3               SET FOR EX                                   
         EX    R1,MVCOM                                                         
         FOUT  (R2)                                                             
*                                                                               
FMTPOS2  BCT   R4,FMTPOS3                                                       
         L     R9,SAVER9                                                        
         BR    R9                  RETURN                                       
*                                                                               
FMTPOS3  BAS   R9,BUMPFLD                                                       
         B     FMTPOS1                                                          
         EJECT                                                                  
*                                                                               
FMTTCOM  DS    0H                  FORMAT TEARSHEET COMMENTS                    
         ST    R9,SAVER9           STORE RETURN REGISTER                        
         LA    R4,4                FOR BCT                                      
         MVI   ELCODE,X'69'                                                     
         LA    R5,REC+33                                                        
FMTTC1   BAS   R9,NEXTEL                                                        
         BC    7,FMTTC2            GO BUMP LINE                                 
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AHI   R1,-3               SET FOR EX                                   
         EX    R1,MVCOM                                                         
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATED BIT                            
*                                                                               
FMTTC2   BCT   R4,FMTTC3                                                        
         L     R9,SAVER9                                                        
         BR    R9                  RETURN                                       
*                                                                               
FMTTC3   BAS   R9,BUMPFLD                                                       
         LA    R6,1(R6)                                                         
         B     FMTTC1                                                           
*                                                                               
MVCOM    MVC   8(0,R2),2(R5)                                                    
*                                                                               
EDTINS   DS    0H                                                               
         GOTO1 VEDTINS,DMCB,(RC),(RA)                                           
         CLI   ERRAREA,0                                                        
         BCR   8,R9                                                             
         B     EXXMOD                                                           
         EJECT                                                                  
*                                  FORMAT TEARSHEET INFO                        
FMTTEAR  DS    0H                                                               
         ST    R9,SAVER9           STORE RETURN REGISTER                        
         MVI   ELCODE,X'95'                                                     
         LA    R5,REC+33                                                        
FMTT1    BAS   R9,NEXTEL                                                        
         BNE   FMTT50              NO ELEM                                      
         USING PTSHTELD,R5                                                      
         MVC   8(1,R2),PTSHSTAT                                                 
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET ON VALIDATED                             
         BAS   R9,BUMPFLD2         TO GET PAST ANNOTATION FIELD                 
*                                                                               
         LA    R4,4                FORMAT FIRST 4 EVALUATIONS                   
         LA    R6,PTSHIND1                                                      
FMTT5    MVC   8(1,R2),0(R6)                                                    
         FOUT  (R2)                                                             
         OI    4(R2),X'20'                                                      
         BAS   R9,BUMPFLD2                                                      
         LA    R6,1(R6)                                                         
         BCT   R4,FMTT5                                                         
*                                                                               
         CLI   PTSHREPO,0          CHECK FOR REPO DATA                          
         BE    FMTT10                                                           
         EDIT  (B1,PTSHREPO),(2,8(R2)),0,ALIGN=LEFT                             
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATE                                     
*                                                                               
FMTT10   BAS   R9,BUMPFLD2                                                      
         MVC   8(1,R2),PTSHIND5    ZONES                                        
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATE                                     
*                                                                               
         BAS   R9,BUMPFLD2                                                      
         MVC   8(10,R2),PTSHPAGE                                                
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATE                                     
         B     FMTTX                                                            
         DROP  R5                                                               
*                                                                               
FMTT50   DS    0H                  HERE IF NO TEARSHEET ELEM                    
         LHI   RF,14               BUMP 15 FIELDS                               
         BAS   R9,BUMPFLDS                                                      
*                                                                               
FMTTX    L     R9,SAVER9                                                        
         BR    R9                  RETURN                                       
         EJECT                                                                  
*                                                                               
* SUBROUTINE TO FORMAT POL PRD ALLOCATIONS                                      
*                                                                               
FMTPRDS  NTR                                                                    
*                                                                               
         XC    DUMEL+8(47),DUMEL+8                                              
         SR    R0,R0                                                            
         LA    R3,DUMEL+8                                                       
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATED BIT                            
*                                                                               
         TM    PBDWTSUM,X'80'      TEST UNEQUAL SPLIT                           
         BZ    FMTP2               NO-EQUAL                                     
         IC    R0,PBDWTSUM         GET SUM OF WEIGHTS                           
         N     R0,=F'127'                                                       
         BAS   R9,FMTPEDT                                                       
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
FMTP2    MVI   ELCODE,X'21'                                                     
         LA    R5,REC+33                                                        
         BAS   R9,NEXTEL                                                        
FMTP4    MVC   0(3,R3),2(R5)       PRD CODE                                     
         CLI   2(R3),C' '                                                       
         BNE   *+10                                                             
         MVI   2(R3),0                                                          
         BCTR  R3,0                                                             
         LA    R3,3(R3)                                                         
         TM    PBDWTSUM,X'80'      TEST UNEQUAL SPLIT                           
         BZ    FMTP6                                                            
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         SR    R0,R0                                                            
         IC    R0,3+2(R5)          COST SHARE                                   
         BAS   R9,FMTPEDT                                                       
         CLI   5(R5),0                                                          
         BNZ   *+12                                                             
         MVI   0(R3),C'0'                                                       
         LA    R3,1(R3)                                                         
         CLC   5(1,R5),6(R5)       TEST COST SHARE=SPACE SHARE                  
         BE    FMTP6                                                            
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         IC    R0,4+2(R5)          SPACE SHARE                                  
         BAS   R9,FMTPEDT                                                       
FMTP6    BAS   R9,NEXTEL                                                        
         BNE   FMTPX                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         B     FMTP4                                                            
*                                                                               
* ADD PRDS TO SVPRD LIST                                                        
*                                                                               
FMTPX    MVI   ELCODE,X'21'                                                     
         LA    R5,REC+33                                                        
FMTPX2   BAS   R9,NEXTEL                                                        
         BNE   EXXMOD                                                           
         LA    R1,SVPRDS                                                        
         LA    R0,L'SVPRDS/3                                                    
FMTPX4   CLI   0(R1),0             SLOT EMPTY                                   
         BE    FMTPX6                                                           
         CLC   0(3,R1),2(R5)       PRD MATCH                                    
         BE    FMTPX2                                                           
         LA    R1,3(R1)                                                         
         BCT   R0,FMTPX4                                                        
         B     EXXMOD              LIST FULL                                    
FMTPX6   MVC   0(3,R1),2(R5)       ADD TO LIST                                  
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* ON ENTRY R0 HAS INPUT, R3 HAS OUTPUT ADDRESS                                  
*                                                                               
FMTPEDT  EDIT  (R0),(3,0(R3)),ALIGN=LEFT                                        
         AR    R3,R0               POINT TO NEXT OUTPUT POSITION                
         BR    R9                                                               
*                                                                               
NEXTEL   CLI   0(R5),0                                                          
         BC    8,NEXTELX                                                        
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BCR   8,R9                EXIT WITH CC EQUAL                           
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
NEXTELX  LTR   R5,R5               SET CC TO NOT EQUAL                          
         BR    R9                                                               
         EJECT                                                                  
*                                                                               
* FETCH ADDITIONAL DISPLAY LOGIC                                                
* NOTE - HERE ONLY AFTER SINGLE DISPLAY                                         
*                                                                               
DSPL10   TM    PBUYCNTL,X'80'      TEST DELETED                                 
         BZ    DSPL10B                                                          
         OI    WARN,X'20'          SET DELETED WARNING                          
         B     DSPL10D                                                          
*                                                                               
DSPL10B  DS    0H                                                               
         OI    TRCODE+1,C' '                                                    
*                                                                               
DSPL10D  DS    0H                                                               
         TM    T411FFD+12,X'08'    NO T/S CHG - GO PROTECT EVERYTHING           
         BO    PROTECT                                                          
*                                                                               
         CLI   BUYBILL,C'X'        SEE IF BOTH BUYER AND BILLER                 
         BE    DSPL10X             PROTECT NOTHING                              
*                                                                               
         CLI   BUYBILL,C'L'        SEE IF WESTERN BILLER                        
         BE    DSPL10P             GO PROTECT EVERYTHING BUT STATUS             
*                                                                               
         CLI   BUYBILL,C'B'        SEE IF WESTERN BUYER                         
         BNE   DSPL10X                                                          
*                                                                               
         CLI   TSHSTAT,C'I'        SEE IF STATUS IS 'I'                         
         BE    PROTECT             GO PROCTECT EVERYTHING                       
*                                                                               
         B     DSPL10X                                                          
*                                                                               
PROTECT  DS    0H                  PROTECT EVERYTHING                           
*                                                                               
DSPL10S  DS    0H                                                               
         OI    TSHSTATH+1,X'20'    PROTECT STATUS                               
DSPL10T  OI    TSHTS1H+1,X'20'                                                  
         OI    TSHTS2H+1,X'20'                                                  
         OI    TSHTS3H+1,X'20'                                                  
         OI    TSHTS4H+1,X'20'                                                  
         OI    TSHTS5H+1,X'20'                                                  
         OI    TSHREPOH+1,X'20'                                                 
         OI    TSHPAGEH+1,X'20'                                                 
         OI    TSHCOM1H+1,X'20'                                                 
         OI    TSHCOM2H+1,X'20'                                                 
         OI    TSHCOM3H+1,X'20'                                                 
         OI    TSHCOM4H+1,X'20'                                                 
         B     DSPL10X                                                          
*                                                                               
DSPL10P  DS    0H                  HERE IF BILLER                               
         B     DSPL10T             GO PROTECT EVERYTHING EXCEPT STATUS          
*                                                                               
DSPL10X  B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
NOINS    FOUT  BUYMSGH,NOINSMSG                                                 
         LA    R2,BUYPBH                                                        
         MVI   ERRAREA,C'M'        FAKE ERROR                                   
         B     EXIT                                                             
*                                                                               
NOINSMSG DC    C'NO INSERTIONS ON FILE'                                         
         EJECT                                                                  
*                                                                               
GETJOB   DS    0H                                                               
         ST    R9,DUB              SAVE RETURN REGISTER IN DUB                  
*                                  BUILD JOB KEY                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BUYMD                                                   
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+4(3),BUYCL                                                   
         MVC   KEY+7(3),BUYPR                                                   
         MVC   KEY+10(6),PBDJOB                                                 
         CLC   SVJOB,PBDJOB                                                     
         BE    GETJ4                                                            
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+12                                                             
         LA    R3,NFNDERR                                                       
         B     ERROR                                                            
*                                                                               
         MVC   SVJOB,WORK                                                       
         MVC   SVJOBDA,KEY+27      SAVE DA                                      
         MVI   KEY+16,X'FF'                                                     
         BAS   RE,HIGH                                                          
         CLC   KEY(16),KEYSAVE                                                  
         BE    GETJ4                                                            
         XC    SVJOB(10),SVJOB     CLEAR SVJOB AND SVJOBDA                      
         LA    R3,JOBERR2          NO INSTRUCTION RECORD                        
         B     ERROR                                                            
GETJ4    DS    0H                                                               
*                                                                               
GETJ6    DS    0H                                                               
         L     R6,AJOBIO                                                        
         USING PJOBRECD,R6                                                      
*                                                                               
         CLC   KEY(16),PJOBKEY                                                  
         BE    GETJ7               ALREADY HAVE RECORD                          
         MVC   KEY+27(4),SVJOBDA                                                
         MVC   AREC,AJOBIO         READ INTO JOBIO                              
         BAS   RE,GETREC                                                        
         LA    RF,REC                                                           
         ST    RF,AREC             RESTORE AREC                                 
*                                                                               
GETJ7    DS    0H                                                               
*                                  NOW DO PREMIUMS                              
GETJOBX  L     R9,DUB                                                           
         BR    R9                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (DIRECTORY)                                   
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (FILE)                                        
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
*                                                                               
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* DATA MANAGER ERRORS AND EXIT                                                  
*                                                                               
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
*                                                                               
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* EXITS FROM PROGRAM                                                            
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETBYKEY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,KEY                                                           
         MVC   0(2,R6),AGYALPHA                                                 
         MVC   2(1,R6),BUYMD                                                    
         MVI   3(R6),X'20'                                                      
         MVC   4(3,R6),BUYCL                                                    
         MVC   7(3,R6),BUYPR                                                    
         MVC   10(6,R6),BPUB                                                    
         MVC   16(3,R6),BINSDT                                                  
         MVC   19(2,R6),BEST                                                    
         MVC   24(1,R6),BSUBLN                                                  
         CLI   24(R6),0                                                         
         BNE   *+8                                                              
         MVI   24(R6),1                                                         
*                                                                               
         CLI   MADSW,C'Y'          SCRIPT UPLOAD?                               
         JNE   X_XIT1                                                           
         L     RE,ATHISTMP         POINT TO UPLOAD OBJECT                       
         LA    RE,2(RE)            POINT PASS LENGTH                            
         USING PINSD,RE                                                         
         CLC   =C'DEL',8(RE)       DELETE OBJECT?                               
         JE    *+10                                                             
         MVC   24(1,R6),PINSLINE   USE LINE NUMBER FROM UPLOAD OBJECT           
         DROP  RE                                                               
*                                                                               
X_XIT1   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPBUYWRK1                                                      
*                                                                               
         ORG   REC                 MAP BUY RECORD TO REC                        
*                                                                               
       ++INCLUDE PPBUYWRK2                                                      
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
PPF4D    DSECT                                                                  
       ++INCLUDE PPBUYF4D                                                       
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047PPBUY07   04/20/16'                                      
         END                                                                    
