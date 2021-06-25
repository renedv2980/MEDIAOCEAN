*          DATA SET SPGOL24    AT LEVEL 028 AS OF 12/18/17                      
*PHASE T20224B                                                                  
*INCLUDE MOBILE                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE HEXOUT                                                                 
         TITLE 'SPGOL24 - SPOTPAK GOALS - NEW CPP ADD/CHANGE/RECALL'            
T20224   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20224,RR=R8                                                   
         L     RC,0(R1)                                                         
         LA    R7,2048(RC)                                                      
         LA    R7,2048(R7)                                                      
         USING GENOLD,RC,R7                                                     
*                                                                               
         L     RA,4(R1)                                                         
         USING T202FFD,RA                                                       
         ST    R8,RELO                                                          
         B     *+8                                                              
RELO     DC    A(0)                                                             
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T20224+4096,R9                                                   
*                                                                               
CP3      XC    BDATA,BDATA                                                      
         XC    BPERS,BPERS                                                      
* GENERATE WEEK/MONTH LISTS                                                     
         L     R0,=V(GETBROAD)                                                  
         A     R0,RELO                                                          
         ST    R0,WORK                                                          
         MVC   WORK+4(4),VADDAY                                                 
         MVC   WORK+8(4),VGETDAY                                                
         MVC   WORK+12(4),VDATCON                                               
         SR    R0,R0               SET FOR STD BDCST MONTHS                     
         CLI   SVSPPROF+2,6          ALLOW ONLY BROADCAST MONTHS                
         BL    CP4                  0,6,7,8                                     
         CLI   SVSPPROF+2,8                                                     
         BH    CP4                                                              
         IC    R0,SVSPPROF+2                                                    
* GET MONTHS                                                                    
CP4      DS    0H                                                               
         GOTO1 =V(MOBILE),DMCB,(13,SVSTART),((R0),BPERS),WORK,         X        
               SVSPPROF,RR=RB                                                   
         MVI   ERRCD,BADCPPYR                                                   
         CLI   0(R1),12            PERIOD NO MORE THAN 12 MONTHS                
         BE    CP5                                                              
         BL    GLERR                                                            
* HIGH - TRY FOR NON-FISCAL PERIODS IF POSSIBLE                                 
         CLI   SVSPPROF+2,0                                                     
         BE    GLERR                                                            
         MVI   SVSPPROF+2,0        SET FOR GOOD OLD BROADCAST MONTHS            
         B     CP3                                                              
* GET WEEKS                                                                     
CP5      DS    0H                                                               
         GOTO1 (RF),(R1),(53,SVSTART),(4,REC)                                   
         CLI   0(R1),52            AND AT LEAST 52 WEEKS                        
         BL    GLERR                                                            
         CLI   0(R1),53                                                         
         BH    GLERR                                                            
         EJECT                                                                  
* CREATE 2 BYTE LIST IN BWEEKS                                                  
         LA    R3,BWEEKS                                                        
         LA    R8,REC                                                           
CP6      MVC   0(2,R3),0(R8)                                                    
         LA    R3,2(R3)                                                         
         LA    R8,4(R8)                                                         
         CLI   0(R8),X'FF'                                                      
         BNE   CP6                                                              
*                                                                               
         CLI   SVESTYP,5           TEST QTRLY                                   
         BE    QCP                                                              
*                                                                               
CP20     DS    0H                                                               
         LA    R2,GOLACT1H                                                      
         MVI   ERRCD,INVERR                                                     
         CLI   8(R2),C'R'                                                       
         BE    RCL                                                              
         CLI   8(R2),C'A'                                                       
         BE    CP22                                                             
         B     GLERR                                                            
         EJECT                                                                  
CP22     DS    0H                                                               
         MVI   ERRCD,MSSNGERR                                                   
         LA    R2,GOLMKT1H                                                      
         CLI   5(R2),0                                                          
         BE    GLERR                                                            
*                                                                               
         GOTO1 USER1               'EDTMKT'                                     
*                                                                               
         TM    4(R2),X'20'                                                      
         BO    CP24                                                             
         FOUT  GOLNAM1H,SVMKTNAM,16                                             
         OI    4(R2),X'20'                                                      
*                                                                               
CP24     MVI   ERRCD,MSSNGERR                                                   
         LA    R2,GOLDPT1H                                                      
         CLI   5(R2),0                                                          
         BE    GLERR                                                            
*                                                                               
         GOTO1 USER2               'EDTDPTLN'                                   
*                                                                               
         CLI   BSLN,30                                                          
         BE    CP26                                                             
         MVI   ERRCD,INVERR                                                     
         CLI   BSLN,0                                                           
         BNE   GLERR                                                            
         MVI   BSLN,30                                                          
*                                                                               
CP26     DS    0H                                                               
         BAS   RE,BLDKEY                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST REC ON FILE                             
         BNE   CP28                NO-BUILD NEW                                 
         GOTO1 GETREC                                                           
         BAS   RE,CHKELDT                                                       
         CLI   BYTE,51             CHECK NUMBER OF X'21' ELEMS                  
         BH    CP30                SHOULD BE 52 OR 53                           
*                                  ELSE BUILD NEW RECORD                        
CP28     DS    0H                                                               
         BAS   RE,BLDREC                                                        
         EJECT                                                                  
CP30     DS    0H                                                               
         LA    R2,GOLACT1H                                                      
         CLC   =C'A$',8(R2)        TEST EXPLICIT $ REQUEST                      
         BNE   CP32                NO                                           
         CLC   KEY(13),KEYSAVE     TEST REC ON FILE                             
         BE    CP40                YES                                          
         TM    SVESTYP,X'01'       TEST NORMAL INPUT TYPE                       
         BO    CP40                IF DOLLARS, OK                               
         MVI   ERRCD,BADADD                                                     
         B     GLERR                                                            
CP32     TM    SVESTYP,X'01'       TEST NORMAL INPUT TYPE                       
         BZ    CP80                PERCENTS                                     
* EDIT DOLLARS                                                                  
*                                                                               
CP40     DS    0H                                                               
         LA    R2,GOLDOL1H                                                      
         LA    R3,WORK2                                                         
         XC    WORK2,WORK2                                                      
         LA    R8,BPERS                                                         
*                                                                               
CP42     LA    R4,8(R2)                                                         
         ZIC   R5,5(R2)                                                         
         TM    SVESTYP,X'01'       TEST NORMAL INPUT TYPE                       
         BO    CP44                DOLLARS                                      
         MVI   ERRCD,NOCHGBAS                                                   
         CLI   0(R4),C'$'                                                       
         BNE   CP43A                                                            
         CLC   GDBASMON,0(R8)                                                   
         BNE   GLERR                                                            
         MVI   ERRCD,NOCHGBAS                                                   
         TM    4(R2),X'20'                                                      
         BZ    GLERR                                                            
         LA    R4,1(R4)                                                         
         BCTR  R5,0                                                             
         B     CP44                                                             
CP43A    CLC   GDBASMON,0(R8)                                                   
         BE    GLERR                                                            
*                                                                               
CP44     MVI   ERRCD,MSSNGERR                                                   
         LTR   R5,R5                                                            
         BNZ   CP45                                                             
         OC    BDOLS,BDOLS                                                      
         BZ    GLERR                                                            
         B     CP46                                                             
         EJECT                                                                  
CP45     DS    0H                                                               
         GOTO1 VCASHVAL,DMCB,(2,(R4)),(R5)                                      
         MVI   ERRCD,INVERR                                                     
         CLI   0(R1),X'FF'                                                      
         BE    GLERR                                                            
         L     R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BNP   GLERR                                                            
         ST    R0,BDOLS                                                         
         CLI   8(R2),C'$'                                                       
         BNE   *+10                                                             
         MVC   BPTS,BDOLS          SAVE BASE MONTH DOLLARS                      
*                                                                               
CP46     MVC   0(4,R3),BDOLS                                                    
         CLI   8(R2),C'$'                                                       
         BNE   *+10                                                             
         XC    BDOLS,BDOLS         DO NOT CARRY BASE DOLS FORWARD               
*                                                                               
         LA    R3,4(R3)                                                         
         LA    R8,4(R8)                                                         
         CLI   0(R8),X'FF'                                                      
         BE    CP50                                                             
         LA    RF,7                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,*-6                                                           
         B     CP42                                                             
         EJECT                                                                  
* CREATE PCTGS IF NORMAL INPUT TYPE IS PCTS                                     
*                                                                               
CP50     TM    SVESTYP,X'01'       TEST NORMAL INPUT TYPE                       
         BO    CP100               DOLLARS                                      
*                                                                               
         LA    R2,GOLDOL1H                                                      
         LA    R4,WORK2                                                         
         LA    R3,GDMONPCT                                                      
         LA    R8,BPERS                                                         
         MVI   ERRCD,BADPCTG                                                    
*                                                                               
CP52     L     R1,0(R4)                                                         
         M     R0,=F'200'          X 100 X 2                                    
         D     R0,BPTS             BASE MONTH $ SAVED HERE                      
         AH    R1,=H'1'            ROUND                                        
         SRA   R1,1                                                             
         CH    R1,=H'255'                                                       
         BH    GLERR                                                            
         LTR   R1,R1                                                            
         BZ    GLERR                                                            
         STC   R1,0(R3)                                                         
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R3,1(R3)                                                         
         LA    R8,4(R8)                                                         
         CLI   0(R8),X'FF'                                                      
         BE    CP90                GO GENERATE WEEKLY ELEMS FROM PCTS           
*                                                                               
         LA    RF,7                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,*-6                                                           
         B     CP52                                                             
         EJECT                                                                  
* EDIT PCTS                                                                     
*                                                                               
CP80     DS    0H                                                               
         LA    R2,GOLDOL1H                                                      
         LA    R3,GDMONPCT                                                      
         LA    R8,BPERS                                                         
         XC    GDBASMON,GDBASMON                                                
*                                                                               
CP82     CLI   5(R2),0                                                          
         BNE   CP84                                                             
         MVI   ERRCD,MSSNGERR                                                   
         OC    BDOLS,BDOLS                                                      
         BZ    GLERR                                                            
CP84     LA    R4,8(R2)                                                         
         ZIC   R5,5(R2)                                                         
         CLI   0(R4),C'$'          TEST BASE MONTH                              
         BNE   CP86                                                             
         MVI   ERRCD,DUPBAS                                                     
         OC    GDBASMON,GDBASMON                                                
         BNZ   GLERR                                                            
         MVC   GDBASMON,0(R8)      SET BASE MONTH START DATE                    
         GOTO1 VCASHVAL,DMCB,(2,(R4)),(R5)                                      
         MVI   ERRCD,INVERR                                                     
         CLI   0(R1),X'FF'                                                      
         BE    GLERR                                                            
         L     R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BNP   GLERR                                                            
         ST    R0,BPTS             SAVE BASE MONTH DOLLARS                      
         MVI   0(R3),100           SET PCT=100                                  
         B     CP88                                                             
*                                                                               
CP86     DS    0H                                                               
         CLI   5(R2),0                                                          
         BNE   CP87                                                             
         OC    BDOLS,BDOLS                                                      
         BNZ   CP87X                                                            
         MVI   ERRCD,MSSNGERR                                                   
         B     GLERR                                                            
CP87     GOTO1 PACK                                                             
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BNP   GLERR                                                            
         CH    R0,=H'255'                                                       
         BH    GLERR                                                            
         ST    R0,BDOLS                                                         
CP87X    MVC   0(1,R3),BDOLS+3                                                  
         EJECT                                                                  
CP88     LA    R3,1(R3)                                                         
         LA    R8,4(R8)                                                         
         CLI   0(R8),X'FF'                                                      
         BE    CP88X               GO CHECK BASE MONTH ENTERED                  
*                                                                               
         LA    RF,7                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,*-6                                                           
         B     CP82                                                             
*                                                                               
CP88X    LA    R2,GOLACT1H                                                      
         MVI   ERRCD,NOBASMON                                                   
         OC    GDBASMON,GDBASMON                                                
         BZ    GLERR                                                            
         SPACE 2                                                                
* GENERATE AMOUNT FIELDS, THEN USE THEM AS IF ENTERED                           
*                                                                               
CP90     LA    R4,WORK2                                                         
         XC    WORK2,WORK2                                                      
         LA    R3,GDMONPCT                                                      
         LA    R8,BPERS                                                         
CP92     DS    0H                                                               
         ZIC   R1,0(R3)                                                         
         M     R0,BPTS             BASE MONTH DOLLARS SAVED HERE                
         AH    R1,=H'50'                                                        
         D     R0,=F'100'                                                       
         ST    R1,0(R4)                                                         
         LA    R4,4(R4)                                                         
         LA    R3,1(R3)                                                         
         LA    R8,4(R8)                                                         
         CLI   0(R8),X'FF'                                                      
         BNE   CP92                                                             
         EJECT                                                                  
*                                                                               
* GENERATE WEEKLY ELEMENTS                                                      
*                                                                               
CP100    DS    0H                                                               
         LA    R6,GDELEM                                                        
         MVI   ELCODE,X'21'                                                     
CP102    BAS   RE,NEXTEL                                                        
         BNE   CP104                                                            
CP102X   GOTO1 VRECUP,DMCB,GOALREC,(R6)                                         
         CLI   0(R6),X'21'                                                      
         BE    CP102X                                                           
         B     CP102                                                            
*                                                                               
CP104    LA    R4,WORK2                                                         
         LA    R3,BWEEKS                                                        
         LA    R8,BPERS                                                         
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'210C'                                                 
         MVC   ELEM+4(4),=F'1000'  SET 100X10 POINTS                            
*                                                                               
CP106    MVC   ELEM+2(2),0(R3)     SET WEEK DATE                                
         L     R0,0(R4)                                                         
         MH    R0,=H'100'          SET PENNIES X 100                            
         ST    R0,ELEM+8                                                        
         GOTO1 VRECUP,DMCB,GOALREC,ELEM,(R6)                                    
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               POINT PAST ELEM                              
         LA    R3,2(R3)            NEXT WEEK                                    
         OC    0(2,R3),0(R3)       TEST LAST WEEK                               
         BZ    CP106X              YES - DONE                                   
         CLC   0(2,R3),2(R8)       TEST WEEK IN MONTH                           
         BNH   CP106                                                            
         LA    R4,4(R4)                                                         
         LA    R8,4(R8)                                                         
         CLI   0(R8),X'FF'                                                      
         BNE   CP106                                                            
         EJECT                                                                  
* SET ACTIVITY DATE                                                             
CP106X   DS    0H                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,GACTDATE)                                  
*                                                                               
         OC    GREDATE,GREDATE                                                  
         BNZ   *+10                                                             
         MVC   GREDATE,GACTDATE                                                 
         MVC   GBUYNAME,GOLPLNR                                                 
*                                                                               
         CLC   KEY(13),KEYSAVE     TEST REC ON FILE                             
         BE    CP108                                                            
         GOTO1 ADDREC                                                           
         B     CP110                                                            
*                                                                               
CP108    DS    0H                                                               
         GOTO1 PUTREC                                                           
*                                                                               
CP110    DS    0H                                                               
         LA    R2,GOLACT1H                                                      
         B     EXXMOD                                                           
         EJECT                                                                  
QCP      DS    0H                                                               
         LA    R2,GOLACT1H                                                      
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),0                                                          
         BE    GLERR                                                            
         CLI   8(R2),C'R'                                                       
         BE    RCL                                                              
*                                                                               
* CLEAR ALL MKT NAMES WITH CHANGED MKTS                                         
*                                                                               
         LA    R4,GOLACT1H                                                      
QCP1A    LR    R8,R4               SAVE PREVIOUS HDR ADDRESS                    
         ZIC   R5,0(R4)                                                         
         AR    R4,R5                                                            
         CLI   0(R4),0                                                          
         BE    QCP1X                                                            
         TM    1(R4),X'20'         TEST PROT                                    
         BZ    QCP1A                                                            
         TM    4(R8),X'20'         UNP FIELD VALID                              
         BO    QCP1A                                                            
         SH    R5,=H'9'                                                         
         EX    R5,GLOC                                                          
         BZ    QCP1A                                                            
         EX    R5,GLXC                                                          
         FOUT  (R4)                                                             
         B     QCP1A                                                            
*                                                                               
GLOC     OC    8(0,R4),8(R4)                                                    
GLXC     XC    8(0,R4),8(R4)                                                    
*                                                                               
QCP1X    CLI   8(R2),C'A'                                                       
         BE    QCPADD                                                           
QCP4     CLI   8(R2),C'*'                                                       
         BNE   GLERR                                                            
* MAKE SURE NO CHANGED FIELDS ON * LINE                                         
         MVI   ERRCD,CHDTAERR                                                   
         LR    R4,R2                                                            
         SR    R5,R5                                                            
         LA    R0,7                TEST MKT/NAME/DPT/DOLS/.../PER               
         B     QCP4B                                                            
*                                                                               
QCP4A    TM    1(R4),X'20'         TEST PROT                                    
         BO    QCP4B                                                            
         TM    4(R4),X'20'         TEST VALIDATED                               
         BZ    GLERR                                                            
QCP4B    IC    R5,0(R4)                                                         
         AR    R4,R5                                                            
         BCT   R0,QCP4A                                                         
*                                                                               
         EJECT                                                                  
QCPADD   ST    R2,BLNADDR                                                       
*                                                                               
QCPMKT   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BNE   QCPMKT2                                                          
         MVI   ERRCD,MSSNGERR                                                   
         OC    BMKT,BMKT                                                        
         BZ    GLERR                                                            
         B     QCPMKTX                                                          
*                                                                               
QCPMKT2  DS    0H                                                               
         GOTO1 USER1               EDTMKT                                       
*                                                                               
         TM    4(R2),X'20'                                                      
         BO    QCPMKTX                                                          
         ZIC   R4,0(R2)                                                         
         AR    R4,R2                                                            
         FOUT  (R4),SVMKTNAM,16                                                 
*                                                                               
QCPMKTX  OI    4(R2),X'20'                                                      
         EJECT                                                                  
QCPDPT   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)            SKIP MKT NAME                                
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BNE   QCPDPT2                                                          
         CLI   BDPT,0                                                           
         BNZ   QCPDPTX                                                          
         MVI   ERRCD,MSSNGERR                                                   
         B     GLERR                                                            
*                                                                               
QCPDPT2  DS    0H                                                               
         GOTO1 USER2               'EDTDPTLN'                                   
         CLI   BSLN,30                                                          
         BE    QCPDPTX                                                          
         MVI   ERRCD,INVERR                                                     
         CLI   BSLN,0                                                           
         BNE   GLERR                                                            
         MVI   BSLN,30                                                          
*                                                                               
QCPDPTX  OI    4(R2),X'20'                                                      
         EJECT                                                                  
QCPDOLS  DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BNE   QCPDOL2                                                          
         MVI   ERRCD,MSSNGERR                                                   
         OC    BDOLS,BDOLS                                                      
         BZ    GLERR                                                            
         B     QCPDOLX                                                          
*                                                                               
QCPDOL2  DS    0H                                                               
         LA    R4,8(R2)                                                         
         ZIC   R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,(R4)),(R5)                                      
         CLI   0(R1),X'FF'                                                      
         BE    GLERR                                                            
         L     R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BNP   GLERR                                                            
         MH    R0,=H'100'                                                       
         ST    R0,BDOLS                                                         
*                                                                               
QCPDOLX  OI    4(R2),X'20'                                                      
         EJECT                                                                  
QCPPER   DS    0H                                                               
         ZIC   R0,0(R2)            SKIP POINTS                                  
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BNE   QCPPER2                                                          
         MVI   ERRCD,MSSNGERR                                                   
         OC    BWEEKS,BWEEKS                                                    
         BZ    GLERR                                                            
         B     QCPPERX                                                          
*                                                                               
QCPPER2  MVI   ERRCD,INVERR                                                     
         CLI   5(R2),2                                                          
         BNE   GLERR                                                            
         CLC   8(2,R2),=C'Q1'                                                   
         BL    GLERR                                                            
         CLC   8(2,R2),=C'Q4'                                                   
         BH    GLERR                                                            
*                                                                               
QCPPERX  OI    4(R2),X'20'                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'210C'                                                 
         MVC   ELEM+4(4),=F'1000'  SET 100X10 PTS                               
         MVC   ELEM+8(4),BDOLS                                                  
*                                                                               
         BAS   RE,BLDKEY                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   QCP100                                                           
         GOTO1 GETREC                                                           
         BAS   RE,CHKELDT                                                       
         CLI   BYTE,51             CHECK NUMBER OF X'21' ELEMS                  
         BH    QCP110              SHOULD BE 52 OR 53                           
*                                  ELSE BUILD NEW RECORD                        
*                                                                               
QCP100   BAS   RE,BLDREC           BUILD NEW REC                                
*                                                                               
* ADD ELEMENT FOR EVERY WEEK                                                    
*                                                                               
         LA    R3,BWEEKS                                                        
         LA    R6,GDELEM                                                        
         MVI   ELCODE,0                                                         
QCP102   BAS   RE,NEXTEL           POINT TO E-O-R                               
         MVC   ELEM+2(2),0(R3)                                                  
         GOTO1 VRECUP,DMCB,GOALREC,ELEM,(R6)                                    
         LA    R3,2(R3)                                                         
         OC    0(2,R3),0(R3)                                                    
         BNZ   QCP102                                                           
         B     QCP120                                                           
         EJECT                                                                  
* REPLACE APPROPRIATE ELEMENTS                                                  
*                                                                               
QCP110   MVI   ERRCD,MSSNGERR                                                   
         CLI   5(R2),0                                                          
         BE    GLERR                                                            
*                                                                               
         LA    R8,BPERS                                                         
         MVC   BYTE,9(R2)          GET QUARTER NUMBER                           
         NI    BYTE,X'0F'                                                       
         ZIC   R0,BYTE                                                          
         BCTR  R0,0                                                             
         MH    R0,=H'12'           EACH MONTH 4 BYTES, EACH QTR 12              
         AR    R8,R0                                                            
*                                                                               
         LA    R6,GDELEM                                                        
         MVI   ELCODE,X'21'                                                     
QCP112   BAS   RE,NEXTEL                                                        
         BNE   QCP120                                                           
         CLC   2(2,R6),0(R8)       ELEM PRIOR TO FIRST MONTH START              
         BL    QCP112              NO-CONTINUE                                  
         CLC   2(2,R6),10(R8)       OR AFTER LAST MONTH END                     
         BH    QCP120              YES-DONE                                     
         MVC   8(4,R6),BDOLS       ELSE UPDATE                                  
         B     QCP112                                                           
         EJECT                                                                  
QCP120   DS    0H                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,GACTDATE)                                  
*                                                                               
         OC    GREDATE,GREDATE                                                  
         BNZ   *+10                                                             
         MVC   GREDATE,GACTDATE                                                 
         MVC   GBUYNAME,GOLPLNR                                                 
*                                                                               
         CLC   KEY(13),KEYSAVE     TEST REC ON FILE                             
         BE    QCP122                                                           
         GOTO1 ADDREC                                                           
         B     QCP124                                                           
*                                                                               
QCP122   DS    0H                                                               
         GOTO1 PUTREC                                                           
*                                                                               
QCP124   L     R4,BLNADDR                                                       
         MVC   9(1,R4),8(R4)                                                    
         MVI   10(R4),C' '                                                      
         MVI   8(R4),C'*'                                                       
         FOUT  (R4)                                                             
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    EXXMOD                                                           
* TEST NO INPUT FIELDS ON NEXT LINE                                             
         LA    RE,7                                                             
         LR    R4,R2                                                            
QCP126   TM    1(R4),X'20'         TEST PROT                                    
         BO    QCP128                                                           
         CLI   5(R4),0                                                          
         BNE   QCP1X               GO EDIT                                      
         EJECT                                                                  
QCP128   IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         BCT   RE,QCP126                                                        
* CLEAR REMAINING PROTECTED FIELDS                                              
QCP130   TM    1(R2),X'20'                                                      
         BZ    QCP132                                                           
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2) *EXECUTED*                                         
*                                                                               
         BZ    QCP132                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         FOUT  (R2)                                                             
*                                                                               
QCP132   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   QCP130                                                           
         B     EXXMOD                                                           
         EJECT                                                                  
RCL      DS    0H                                                               
*                                                                               
* CLEAR THE SCREEN                                                              
*                                                                               
         LA    R2,GOLMKT1H                                                      
         TM    4(R2),X'20'         TEST SAME MARKET                             
         BO    RCL2                                                             
         XC    GOLNAM1,GOLNAM1                                                  
         FOUT  GOLNAM1H                                                         
RCL2     XC    GOLDOL1,GOLDOL1     CLEAR DOLLARS ON  LINE 1                     
         FOUT  GOLDOL1H                                                         
         SPACE 1                                                                
* CLEAR ALL UNPROT FIELDS AND SVMKTNAMS ON SUBSEQUENT LINES *                   
         SPACE 1                                                                
         LA    R2,GOLPER1H                                                      
RCL3     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BNH   RCL4                                                             
         TM    1(R2),X'20'         TEST PROTECTED                               
         BZ    *+14                NO                                           
         CLC   0(1,R2),GOLNAM1H    TEST LENGTH = SVMKTNAM LENGTH                
         BNE   RCL3                NO - DO NOT CLEAR                            
         ZIC   RE,0(R2)            GET CURRENT FIELD LEN                        
         SH    RE,=H'9'            SET FOR EX                                   
         EX    RE,RCLOC                                                         
         BZ    RCL3                                                             
         EX    RE,RCLXC                                                         
         FOUT  (R2)                                                             
         B     RCL3                                                             
*                                                                               
RCLOC    OC    8(0,R2),8(R2) *EXECUTED*                                         
RCLXC    XC    8(0,R2),8(R2) *EXECUTED*                                         
*                                                                               
RCL4     DS    0H                                                               
         MVI   ERRCD,MSSNGERR                                                   
         LA    R2,GOLMKT1H                                                      
         CLI   5(R2),0                                                          
         BE    GLERR                                                            
*                                                                               
         GOTO1 USER1               'EDTMKT'                                     
*                                                                               
         TM    4(R2),X'20'                                                      
         BO    RCL6                                                             
         FOUT  GOLNAM1H,SVMKTNAM,16                                             
*                                                                               
RCL6     MVI   ERRCD,MSSNGERR                                                   
         MVI   BDPT,0                                                           
         MVI   BSLN,0                                                           
         LA    R2,GOLDPT1H                                                      
         CLI   5(R2),0                                                          
         BNE   RCL7                                                             
         CLC   =C'RM',GOLACT1                                                   
         BNE   GLERR                                                            
         B     RCL8                                                             
*                                                                               
RCL7     GOTO1 USER2               'EDTDPTLN'                                   
*                                                                               
         CLI   BSLN,30                                                          
         BE    RCL8                                                             
         MVI   ERRCD,INVERR                                                     
         CLI   BSLN,0                                                           
         BNE   GLERR                                                            
         MVI   BSLN,30                                                          
         EJECT                                                                  
RCL8     DS    0H                                                               
         BAS   RE,BLDKEY                                                        
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLI   SVESTYP,5           TEST QUARTERLY DOLLARS                       
         BE    QCPRCL                                                           
*                                                                               
         LA    R2,GOLACT1H                                                      
         MVI   ERRCD,NODTAERR                                                   
         CLC   KEY(10),KEYSAVE     02/A-M/C/P/MKT/EST/DPT/SLN                   
         BE    RCL10                                                            
         CLC   =C'RM',GOLACT1                                                   
         BNE   GLERR                                                            
         CLC   KEY(8),KEYSAVE                                                   
         BNE   GLERR                                                            
*                                                                               
RCL10    GOTO1 GETREC                                                           
         LA    R2,GOLDPT1H                                                      
         MVC   8(1,R2),GKEYDPT                                                  
         MVC   9(3,R2),SPACES                                                   
         FOUT  (R2)                                                             
*                                                                               
         CLC   =C'R$',GOLACT1      EXPLICIT DOLLARS REQUEST                     
         BE    RCL20                                                            
         TM    SVESTYP,X'01'       TEST NORMAL INPUT TYPE                       
         BO    RCL20               DOLS                                         
         B     RCL40               PCTS                                         
         EJECT                                                                  
* DISPLAY DOLLARS                                                               
*                                                                               
RCL20    LA    R2,GOLDOL1H                                                      
         LA    R6,GDELEM                                                        
         LA    R8,BPERS            MONTH START/END DATES                        
         MVI   ELCODE,X'21'                                                     
*                                                                               
RCL22    BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   2(2,R6),0(R8)                                                    
         BL    RCL22                                                            
         CLC   2(2,R6),2(R8)                                                    
         BNH   *+6                                                              
         DC    H'0'                MISSING ELEMENTS                             
*                                                                               
         L     R0,8(R6)            GET DOLLARS                                  
         C     R0,BDOLS            TEST SAME AS PREVIOUS                        
         BE    RCL26               YES - SUPPRESS DISPLAY                       
         ST    R0,BDOLS                                                         
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(9,WORK2),2,ALIGN=LEFT                                      
         LA    R4,8(R2)                                                         
         CLC   GDBASMON,0(R8)                                                   
         BNE   RCL24                                                            
         MVI   0(R4),C'$'                                                       
         LA    R4,1(R4)                                                         
         XC    BDOLS,BDOLS         FORCE NEXT DISPLAY                           
RCL24    DS    0H                                                               
         MVC   0(7,R4),WORK2                                                    
         OC    8(11,R2),SPACES                                                  
         FOUT  (R2)                                                             
         OI    4(R2),X'20'                                                      
*                                                                               
RCL26    LA    R8,4(R8)                                                         
         CLI   0(R8),X'FF'                                                      
         BE    RCLX                                                             
*                                                                               
         LA    RF,7                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,*-6                                                           
         B     RCL22                                                            
         EJECT                                                                  
* DISPLAY BASE MONTH AND MONTHLY PCTGS                                          
*                                                                               
RCL40    DS    0H                                                               
         LA    R2,GOLDOL1H                                                      
         LA    R6,GDELEM                                                        
         LA    R3,GDMONPCT                                                      
         LA    R8,BPERS                                                         
         MVI   ELCODE,X'21'                                                     
*                                                                               
RCL42    DS    0H                                                               
         CLC   GDBASMON,0(R8)      IS THIS BASE MONTH                           
         BNE   RCL44                                                            
* BASE MONTH DISPLAYS DOLLARS                                                   
RCL43    BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   2(2,R6),0(R8)                                                    
         BNE   RCL43                                                            
         MVI   8(R2),C'$'                                                       
         L     R0,8(R6)                                                         
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(9,WORK2),2,ALIGN=LEFT                                      
         MVC   9(7,R2),WORK2                                                    
         XC    BDOLS,BDOLS         CLEAR FOR COMPARE                            
         B     RCL46                                                            
*                                                                               
* OTHER MONTHS DISPLAY PCTS                                                     
*                                                                               
RCL44    DS    0H                                                               
         ZIC   R0,0(R3)                                                         
         C     R0,BDOLS            TEST SAME AS PREVIOUS                        
         BE    RCL48               YES - SUPPRESS DISPLAY                       
         ST    R0,BDOLS                                                         
         EDIT  (R0),(3,WORK2),ALIGN=LEFT                                        
         MVC   8(3,R2),WORK2                                                    
*                                                                               
RCL46    OC    8(11,R2),SPACES                                                  
         FOUT  (R2)                                                             
*                                                                               
RCL48    LA    R3,1(R3)                                                         
         LA    R8,4(R8)                                                         
         CLI   0(R8),X'FF'                                                      
         BE    RCLX                                                             
*                                                                               
         LA    RF,7                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RF,*-6                                                           
         B     RCL42                                                            
*                                                                               
RCLX     LA    R2,GOLACT1H                                                      
         B     EXXMOD                                                           
         EJECT                                                                  
QCPRCL   DS    0H                                                               
         MVI   ERRCD,INVERR                                                     
         LA    R2,GOLPER1H                                                      
         CLI   5(R2),0                                                          
         BE    QCP200                                                           
         CLI   5(R2),2                                                          
         BNE   GLERR                                                            
         CLC   8(2,R2),=C'Q1'                                                   
         BL    GLERR                                                            
         CLC   8(2,R2),=C'Q4'                                                   
         BH    GLERR                                                            
QCP200   DS    0H                                                               
         FOUT  GOLACT0H,GOLACT1,3                                               
         FOUT  GOLMKT0H,GOLMKT1,4                                               
         FOUT  GOLDPT0H,GOLDPT1,4                                               
         FOUT  GOLPER0H,GOLPER1,13                                              
         MVC   GOLPER0H+5(1),GOLPER1H+5   SET LENGTH IN 0 HDR                   
*                                                                               
         MVI   ERRCD,NODTAERR                                                   
         CLC   KEY(10),KEYSAVE     02/A-M/C/P/MKT/EST/DPT/SLN                   
         BE    QCP201                                                           
         CLC   =C'RM',GOLACT1                                                   
         BNE   GLERR                                                            
         CLC   KEY(8),KEYSAVE                                                   
         BNE   GLERR                                                            
*                                                                               
QCP201   LA    R2,GOLACT1H                                                      
         XC    KEYSAVE,KEYSAVE     FORCE NOT EQ FIRST TIME                      
         EJECT                                                                  
QCP202   DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    KEYSAVE,KEYSAVE     FORCE KEY NEQ KEYSAVE                        
*                                                                               
         LA    R3,BPERS                                                         
         ZAP   HALF,=P'1'                                                       
         CLI   GOLPER0H+5,0                                                     
         BE    QCP204                                                           
         MVC   BYTE,GOLPER0H+9                                                  
         PACK  HALF,BYTE                                                        
         NI    BYTE,X'0F'                                                       
         ZIC   R0,BYTE                                                          
         BCTR  R0,0                                                             
         MH    R0,=H'12'                                                        
         AR    R3,R0                                                            
*                                                                               
QCP204   ST    R2,BLNADDR                                                       
         MVI   ELCODE,X'21'                                                     
         LA    R6,GDELEM                                                        
*                                                                               
QCP206   BAS   RE,NEXTEL                                                        
         BNE   QCP207E                                                          
         CLC   2(2,R6),0(R3)                                                    
         BL    QCP206                                                           
         CLC   2(2,R6),10(R3)                                                   
         BH    QCP207E                                                          
* BEFORE DISPLAY, MAKE SURE ALL CPP'S FOR QTR ARE SAME                          
QCP207A  LR    R1,R6               SAVE CURRENT ELEM ADDR                       
         BAS   RE,NEXTEL                                                        
         BNE   QCP207X                                                          
         CLC   2(2,R6),10(R3)      TEST STILL IN QUARTER                        
         BH    QCP207X                                                          
         CLC   8(4,R1),8(R6)       TEST SAME DOLLARS                            
         BE    QCP207A                                                          
QCP207E  MVI   ERRCD,QCPERR                                                     
         B     GLERR                                                            
*                                                                               
QCP207X  LR    R6,R1                                                            
         EJECT                                                                  
* ELEM IS IN QUARTER - DISPLAY IT                                               
         MVC   8(3,R2),=C'*  '                                                  
         FOUT  (R2)                                                             
*                                                                               
         ZIC   R0,0(R2)            SKIP MKT                                     
         AR    R2,R0                                                            
         OI    4(R2),X'20'         SET FLAG                                     
         IC    R0,0(R2)                                                         
         AR    R2,R0               SKIP MKT NAME                                
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLC   =C'TEST',GOLPLNR                                                 
         BNE   QCP208                                                           
*                                                                               
         SR    R2,R0               BACK UP                                      
         MVI   19(R2),C' '                                                      
         GOTO1 =V(HEXOUT),DMCB,KEY+14,16(R2),4,=C'TOG',RR=RB                    
         AR    R2,R0                                                            
*                                                                               
QCP208   CLC   KEY(11),KEYSAVE     SAME DPT/LEN                                 
         BE    QCP210                                                           
         MVC   KEYSAVE,KEY                                                      
         MVC   8(1,R2),GKEYDPT                                                  
         MVC   9(3,R2),SPACES                                                   
         FOUT  (R2)                                                             
QCP210   OI    4(R2),X'20'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         L     R0,8(R6)                                                         
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(9,WORK2),2,ALIGN=LEFT                                      
         MVC   8(7,R2),WORK2                                                    
         OC    8(11,R2),SPACES                                                  
         FOUT  (R2)                                                             
         OI    4(R2),X'20'                                                      
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               SKIP POINTS                                  
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVI   8(R2),C'Q'                                                       
         UNPK  9(1,R2),HALF                                                     
         OI    9(R2),X'F0'                                                      
         FOUT  (R2)                                                             
         OI    4(R2),X'20'                                                      
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST E-O-S                                   
         BE    EXXMOD                                                           
         EJECT                                                                  
         CLI   GOLPER0H+5,0        TEST SPECIFIC QTR REQUEST                    
         BNE   QCP220              YES - DONE                                   
         LA    R3,12(R3)           NEXT QUARTER                                 
         AP    HALF,=P'1'                                                       
         CP    HALF,=P'4'                                                       
         BNH   QCP204                                                           
*                                                                               
QCP220   CLC   =C'RM',GOLACT0                                                   
         BNE   EXXMOD                                                           
         XC    SVKEY,SVKEY         CLEAR FOR NEXT (I THINK)                     
         GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE                                                   
         BNE   EXXMOD                                                           
         B     QCP202                                                           
         EJECT                                                                  
BLDKEY   LA    R0,GOALREC                                                       
         ST    R0,AREC                                                          
*                                                                               
         XC    GKEY,GKEY                                                        
         MVI   GKEYTYPE,2                                                       
         MVC   GKEYAM,SVAGYMD                                                   
         MVC   GKEYCLT,SVCLT                                                    
         MVC   GKEYPRD,SVPRD                                                    
         MVC   GKEYMKT,BMKT                                                     
         MVC   GKEYEST,SVEST                                                    
         MVC   GKEYDPT,BDPT                                                     
         MVC   GKEYSLN,BSLN                                                     
         MVC   GKEYSEC,BSLN                                                     
         MVC   KEY,GKEY                                                         
         BR    RE                                                               
         SPACE 2                                                                
BLDREC   NTR1                                                                   
         LA    R0,GOALREC                                                       
         LHI   R1,REC2-REC                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   GKEY(13),KEYSAVE                                                 
         MVC   GLENGTH,=H'100'                                                  
         MVC   GAGYALPH,AGYALPHA                                                
         MVC   GDELEM(2),=X'204C'                                               
         MVC   GBUYNAME,GOLPLNR                                                 
         SR    R0,R0                                                            
         IC    R0,SVADVAGY                                                      
         SRL   R0,4                RIGHT ALIGN ADVTSR CODE                      
         STC   R0,GADVAGY                                                       
         XIT1                                                                   
*                                                                               
NEXTEL   SR    R0,R0                                                            
         CLI   0(R6),0             TEST E-O-R                                   
         BE    NEXTELX                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BNE   NEXTEL                                                           
         BR    RE                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
* THIS ROUTINE CHECKS THAT FIRST ELEMENT IS EST START DATE OR                   
* MONDAY AND CHANGES DATE IF NECESSARY.                                         
*                                                                               
CHKELDT  NTR1                                                                   
         MVI   BYTE,0              CLEAR COUNTER                                
         LA    R6,GDELEM                                                        
CHKEL2   ZIC   R0,1(R6)            COUNT NUMBER OF ELEMENTS                     
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    CHKEL10                                                          
         CLI   0(R6),X'21'                                                      
         BNE   CHKEL2                                                           
         IC    R0,BYTE                                                          
         AHI   R0,1                                                             
         STC   R0,BYTE                                                          
         B     CHKEL2                                                           
*                                                                               
CHKEL10  LA    R6,GDELEM                                                        
*                                                                               
CHKEL12  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    CHKELX                                                           
         CLI   0(R6),X'21'                                                      
         BNE   CHKEL12                                                          
* GET 6 BYTE YYMMDD                                                             
         GOTO1 VDATCON,DMCB,(2,2(R6)),WORK                                      
*                                                                               
         CLC   SVSTART,WORK        TEST EST START DATE                          
         BE    CHKELX                                                           
*                                                                               
         GOTO1 VGETDAY,DMCB,WORK,WORK+6                                         
*                                                                               
         CLI   0(R1),1             TEST MONDAY                                  
         BE    CHKELX                                                           
* BACK UP TO PREVIOUS MONDAY OR ESTART                                          
         ZIC   R0,0(R1)                                                         
         BCTR  R0,0                                                             
         LCR   R0,R0                                                            
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
*                                                                               
         CLC   SVSTART,WORK+6                                                   
         BL    *+10                                                             
         MVC   WORK+6(6),SVSTART                                                
*                                                                               
         GOTO1 VDATCON,DMCB,WORK+6,(2,2(R6))                                    
*                                                                               
*                                                                               
CHKELX   XIT1                                                                   
*                                                                               
GLERR    GOTO1 ERROR                                                            
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGOLWRK                                                       
 END                                                                            
