*          DATA SET SPREPXM02  AT LEVEL 081 AS OF 05/01/02                      
*PHASE SPXM02A                                                                  
         TITLE 'SPXM02 - ARMED FORCES SPOT INTERFACE TAPE'                      
SPXM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPXM02                                                         
         SPACE                                                                  
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING SPXMWRKD,RC                                                      
         SPACE                                                                  
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,ESTFRST                                                     
         BE    ESTF                                                             
         CLI   MODE,PROCBUY                                                     
         BE    PRBUY                                                            
         SPACE                                                                  
EXIT     DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
******************                                                              
*                *                                                              
*    QOPT = Y    *                                                              
*  PRINT REPORT  *                                                              
*                *                                                              
******************                                                              
         EJECT                                                                  
         SPACE                                                                  
* RUN FIRST *                                                                   
         SPACE                                                                  
RUNF     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* REQ FRST *                                                                    
         SPACE                                                                  
REQF     DS    0H                                                               
         SPACE                                                                  
         SR    R0,R0               SET BINPARS                                  
         LA    R1,BINTBL                                                        
         SR    R2,R2                                                            
         LA    R3,17               RECORD LENGTH                                
         LA    R4,15               KEY LENGTH                                   
         LA    R5,100              MAX NO OF RECS                               
         STM   R0,R5,BINPARS                                                    
         SPACE                                                                  
         ZAP   ANSTOTS,=P'0'       TOTS TO PACKED ZEROS                         
         ZAP   DOLTOTS,=P'0'                                                    
         SPACE                                                                  
         MVI   RQALLPOL,C'Y'       READ POOL                                    
         MVI   RQNOPSSV,C'Y'       DON'T READ PASSIVE POINTERS                  
         SPACE                                                                  
         CLC   QPRD,=C'ALL'                                                     
         BE    REQF5                                                            
         CLC   QPRD,=C'POL'                                                     
         BE    REQF5                                                            
         MVI   RQALLPOL,C'N'                                                    
         MVI   RQNOPSSV,C'N'                                                    
         SPACE                                                                  
REQF5    CLI   BLOPSW,C'Y'         TEST TAPE OPEN                               
         BE    REQ4                                                             
         MVI   BLOPSW,C'Y'                                                      
         OPEN  (SPXMTP,OUTPUT)                                                  
         SPACE                                                                  
REQ4     DS    0H                                                               
         B     EXIT                                                             
         SPACE                                                                  
* EST FIRST *                                                                   
ESTF     DS    0H                                                               
         SPACE                                                                  
         GOTO1 DATCON,DMCB,QSTART,(2,BQSTARTP)                                  
         GOTO1 DATCON,DMCB,QEND,(2,BQENDP)                                      
         B     EXIT                                                             
         SPACE 2                                                                
* REQUEST LAST *                                                                
REQL     DS    0H                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   EXIT                                                             
         GOTO1 REPORT              FOR BLANK LINE                               
         XC    REPREC,REPREC                                                    
         MVC   REPREC+15(14),=C'*** TOTALS ***'                                 
         MVC   RPTDPT(5),=C'SPOTS'                                              
         MVC   RPTDAYS+9(8),EDPTRN1                                             
         ED    RPTDAYS+9(8),ANSTOTS                                             
         MVC   P(96),REPREC                                                     
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         XC    REPREC,REPREC                                                    
         MVC   RPTDPT(4),=C'COST'                                               
         MVC   RPTDAYS(17),EDPTRN2                                              
         LA    R1,RPTDAYS+16                                                    
         EDMK  RPTDAYS(17),DOLTOTS                                              
         BCTR  R1,0                                                             
         MVI   0(R1),C'$'                                                       
         MVC   P(96),REPREC                                                     
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
EDPTRN1  DC    X'4020202020202120'                                              
EDPTRN2  DC    X'40202020202020202020202020204B2120'                            
         SPACE 2                                                                
* RUN LAST *                                                                    
         SPACE                                                                  
RUNL     DS    0H                                                               
         CLI   BLOPSW,C'Y'                                                      
         BNE   RUNL4                                                            
         CLOSE (SPXMTP)                                                         
         SPACE                                                                  
RUNL4    DS    0H                                                               
         B     EXIT                                                             
         SPACE                                                                  
SPXMTP   DCB   DDNAME=SPXMTP,                                          X        
               DSORG=PS,RECFM=FB,LRECL=00080,BLKSIZE=00800,MACRF=PM             
         EJECT                                                                  
************                                                                    
* PROC BUY *                                                                    
************                                                                    
PRBUY    DS    0H                                                               
         XC    BINPARS+8(4),BINPARS+8     SET BIN COUNT TO ZERO                 
         LA    RE,BINTBL                  CLEAR BIN TABLE                       
         LA    RF,1701                                                          
         XCEF                                                                   
         SPACE                                                                  
         XC    NTISAVE,NTISAVE                                                  
         MVI   NTISW,0                                                          
         SPACE                                                                  
         L     R3,ADBUY                                                         
         USING BUYREC,R3                                                        
         LA    R6,BDELEM                                                        
         SPACE                                                                  
* GET REGELEM ELEMENT *                                                         
PP10     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    WRTREC                                                           
         CLI   0(R6),X'0B'         IS IT POOL-BUY ELEM                          
         BL    PP12                                                             
         CLI   0(R6),X'0D'                                                      
         BH    PP10                                                             
         CLI   1(R6),10            TEST UNALL                                   
         BNH   PP10                                                             
         B     POOLBUY                                                          
         SPACE                                                                  
PP12     CLI   0(R6),X'06'         IS IT NON-POOL ELEM                          
         BE    NOTPOOL                                                          
         CLI   0(R6),X'07'                                                      
         BE    NOTPOOL                                                          
         B     PP10                                                             
         EJECT                                                                  
********************                                                            
* POOL BUY ELEMENT *                                                            
********************                                                            
         SPACE                                                                  
         USING REGELEM,R6                                                       
         SPACE                                                                  
POOLBUY  TM    6(R6),X'C4'         TEST HIATUS,MINUS,MINUSSED                   
         BNZ   PP10                                                             
         CLI   RLEN,X'0E'          TEST FOR LENGTH OVER 14                      
         BL    PP10                                                             
         CLC   RDATE,BQSTARTP      TEST DATE IN REQUEST PERIOD                  
         BL    PP10                                                             
         CLC   RDATE,BQENDP                                                     
         BH    PP10                                                             
         SPACE                                                                  
         CLC   QPRD,=C'ALL'                                                     
         BE    PP15                                                             
         CLC   QPRD,=C'POL'                                                     
         BE    PP15                                                             
         CLC   RPPRD,BPRD                                                       
         BNE   PP10                                                             
         SPACE                                                                  
* LOAD BINTBL *                                                                 
PP15     CLC   MED,=C'NT'          IS IT NETWORK                                
         BNE   PP17                                                             
         SPACE                                                                  
         CLI   NTISW,1             FIRST TIME = 0                               
         BE    PP17                                                             
         BAS   R1,NTIRTN           YES,FIRST TIME                               
         SPACE                                                                  
PP17     XC    WORK,WORK                                                        
         MVC   WORK(2),RDATE                                                    
         MVC   WORK+2(1),RPPRD                                                  
         SPACE                                                                  
         LA    R7,14(R6)           POINT R7 TO AFFID ELEM                       
         USING AFFELEM,R7                                                       
         SPACE                                                                  
         MVC   WORK+15(2),=PL2'1'       MOVE 1 TO CNT IN TBL                    
         CLI   0(R7),X'10'         IS THERE AN AFFID                            
         BE    PP20                                                             
         SPACE                                                                  
* NO AFFID ELEM *                                                               
         L     R4,ADBUY              COST                                       
         GOTO1 GETRATE,DMCB,(X'FF',SPOTS),(X'00',(R4)),(R6)                     
         MVC   WORK+7(4),NET                                                    
         SPACE                                                                  
         L     RE,NET              ADD TO TOTALS                                
         CVD   RE,DUB                                                           
         AP    DOLTOTS,DUB                                                      
         SPACE                                                                  
         CLC   MED,=C'NT'                                                       
         BNE   *+8                                                              
         BAS   RE,X13RTN                                                        
         SPACE                                                                  
         GOTO1 BINSRCH,BINPARS,(1,WORK)                                         
         CLI   0(R1),X'01'         =X'01' IF REC NOT FOUND,SO INSERTED          
         BE    PP10                REC INSERTED,SO GET NEXT ELEM                
         L     R2,0(R1)                                                         
         AP    15(2,R2),=P'1'      ADD TO ANNOUNCEMENT COUNTER                  
         B     PP10                GET NEXT POOL REC                            
         SPACE                                                                  
* AFFID ELEM *                                                                  
PP20     DS    0H                                                               
         L     R4,ADBUY              COST                                       
         GOTO1 GETRATE,DMCB,(X'FF',SPOTS),(X'00',(R4)),(R6)                     
         MVC   WORK+7(4),NET                                                    
         SPACE                                                                  
         L     RE,NET              ADD TO TOTALS                                
         CVD   RE,DUB                                                           
         AP    DOLTOTS,DUB                                                      
         SPACE                                                                  
         CLC   MED,=C'NT'                                                       
         BNE   *+8                                                              
         BAS   RE,X13RTN                                                        
         SPACE                                                                  
         MVC   WORK+3(2),ADATE                                                  
         MVC   WORK+5(2),ATIME                                                  
         DROP  R7                                                               
         GOTO1 BINSRCH,BINPARS,(1,WORK)                                         
         CLI   0(R1),X'01'                                                      
         BE    PP10                REC NOT FOUND,SO INSERTED                    
         L     R2,0(R1)            DUPLICATE RECS WITH = AFFIDS,                
         AP    15(2,R2),=P'1'      SO ADD 1 TO COUNTER                          
         B     PP10                                                             
         SPACE                                                                  
*                                                                               
X13RTN   DS    0H                  GET X'13' ELEM, EXTRACT NTI CODE             
         LR    R8,R6                                                            
         MVI   ELCODE,X'13'                                                     
X13A     CLI   0(R8),0                                                          
         BE    X13AA                                                            
         ZIC   R0,1(R8)                                                         
         LTR   R0,R0                                                            
         BNZ   X13B                                                             
X13AA    MVC   WORK+11(4),NTISAVE     USE X'92' NTI                             
         BR    RE                                                               
X13B     AR    R8,R0                                                            
         CLC   ELCODE,0(R8)                                                     
         BNE   X13A                                                             
         SR    R0,R0                                                            
         ICM   R0,3,2(R8)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+11(4),DUB                                                   
         BR    RE                                                               
         EJECT                                                                  
*******************                                                             
*NON-POOL ELEMENT *                                                             
*******************                                                             
NOTPOOL  DS    0H                                                               
         CLC   RDATE,BQSTARTP      TEST DATE IN REQUEST PERIOD                  
         BL    PP10                                                             
         CLC   RDATE,BQENDP                                                     
         BH    PP10                                                             
         SPACE                                                                  
         MVI   OTOSW,0                                                          
         TM    6(R6),X'40'         MINUSED                                      
         BNZ   PP10                YES, GET NEXT ELEM                           
         SPACE                                                                  
NP05     XC    WORK,WORK                                                        
         MVC   WORK(2),RDATE       DATE                                         
         MVC   WORK+2(1),BUYKPRD   PRODUCT                                      
         ZIC   R0,RNUM             NUMB OF SPOTS                                
         CVD   R0,DUB                                                           
         MVC   WORK+15(2),DUB+6                                                 
         SPACE                                                                  
         L     R4,ADBUY            COST OF 1 SPOT                               
         GOTO1 GETRATE,DMCB,(X'00',SPOTS),(X'00',(R4)),(R6)                     
         MVC   WORK+7(4),NET                                                    
         SPACE                                                                  
NP10     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NPWRITE                                                          
         CLI   0(R6),X'06'                                                      
         BE    NPWRITE                                                          
         CLI   0(R6),X'07'         IS IT OTO ELEMENT                            
         BE    NP20                                                             
         CLI   0(R6),X'10'         IS IT AN AFFID                               
         BNE   NP10                                                             
         SPACE                                                                  
* AFFID ELEMENT *                                                               
         LR    R7,R6                                                            
         USING AFFELEM,R7                                                       
         SPACE                                                                  
         SP    WORK+15(2),=P'1'            DECREASE  SPOTS BY 1                 
         MVC   SPOTSV,WORK+15              SAVE  NUMB OF SPOTS                  
         MVC   WORK+15(2),=PL2'1'                                               
         MVC   WORK+3(2),ADATE                                                  
         MVC   WORK+5(2),ATIME                                                  
         CP    SPOTSV,=PL2'1'              ARE SPOTS REDUCED TO ZERO            
         BL    NP10                                                             
         SPACE                                                                  
         DROP  R7                                                               
         SPACE                                                                  
         GOTO1 BINSRCH,BINPARS,(1,WORK)                                         
         CLI   0(R1),X'01'         IS THIS A DUPLICATE AFFID                    
         BE    NP15                                                             
         L     R2,0(R1)            YES. SO ADD 1 TO COUNTER                     
         AP    15(2,R2),=P'1'                                                   
NP15     ICM   R0,15,WORK+7                                                     
         CVD   R0,DUB                                                           
         AP    DOLTOTS,DUB         ADD TO TOTALS                                
         SPACE                                                                  
         MVC   WORK+15(2),SPOTSV   RESTORE NUMBER OF SPOTS                      
         XC    WORK+3(4),WORK+3    CLEAR ADATE,ATIME                            
         B     NP10                                                             
         SPACE                                                                  
* OTO ELEMENT *                                                                 
NP20     DS    0H                                                               
         CLC   RDATE,WORK          IS THERE A DATE BREAK                        
         BNE   NP50                                                             
         IC    R0,RNUM             NO.                                          
         CVD   R0,DUB                                                           
         TM    6(R6),X'80'         MINUS SPOT                                   
         BZ    NP30                                                             
         SP    WORK+15(2),DUB      YES. SUBTRACT SPOTS                          
         B     NP40                                                             
NP30     AP    WORK+15(2),DUB      NO. ADD SPOTS                                
NP40     B     NP10                                                             
NP50     MVI   OTOSW,C'Y'                                                       
         B     NPWRITE                                                          
         SPACE                                                                  
* PREPARE TO WRITE *                                                            
NPWRITE  DS    0H                                                               
         GOTO1 BINSRCH,BINPARS,(1,WORK)                                         
         CLI   0(R1),X'01'         DUPLICATE RECS                               
         BE    NPW05                                                            
         L     R2,0(R1)            YES, SO ADD TO SPOT COUNTER                  
         AP    15(2,R2),=P'1'                                                   
NPW05    XC    DUB,DUB                                                          
         MVC   DUB+6(2),WORK+15            NUMB OF SPOTS                        
         CVB   R2,DUB                                                           
         LTR   R2,R2               TEST IF SPOTS ARE ZER0                       
         BNP   NPW12                                                            
         ICM   R0,15,WORK+7                ADD TO TOTALS                        
         CVD   R0,DUB                                                           
NPW10    AP    DOLTOTS,DUB                                                      
         BCT   R2,NPW10                                                         
NPW12    CLI   0(R6),X'06'         IS THERE ANOTHER X'06'                       
         BE    NOTPOOL                                                          
         CLI   OTOSW,C'Y'          WAS IT AN OTO ELEM DATE BREAK                
         BE    NOTPOOL                                                          
         B     WRTREC                                                           
         EJECT                                                                  
****************                                                                
* WRITE RECORD *                                                                
****************                                                                
         SPACE                                                                  
WRTREC   DS    0H                                                               
         LA    R7,BINTBL                                                        
         CLC   =X'0000',0(R7)                                                   
         BE    EXIT                NO POOL ELEMS FOR THIS REC, EXIT             
         SPACE                                                                  
         XC    TAPREC,TAPREC                                                    
         XC    REPREC,REPREC                                                    
         SPACE                                                                  
         CLC   15(2,R7),=X'0000'   TEST NUMB OF SPOTS                           
         BE    WRT80B              IF ZERO, GET NXT BINTBL ENTRY                
         SPACE                                                                  
WRT05    ZIC   R0,BUYKBUY          BUY LINE NUMBER                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RPTLNM,DUB                                                       
         MVC   TPAGY,AGY           AGENCY                                       
         MVC   RPTAGY,AGY                                                       
         CLI   BDSTAT,X'01'                                                     
         BNE   WRT10               MEDIA                                        
         MVC   TPMED,=C'NT'                                                     
         MVC   RPTMED,=C'NT'                                                    
         B     WRT20                                                            
WRT10    MVC   TPMED+1(1),MED                                                   
         MVI   TPMED,C'S'                                                       
         MVC   RPTMED+1(1),MED                                                  
         MVI   RPTMED,C'S'                                                      
WRT20    MVC   TPCLT(3),CLT        CLIENT                                       
         MVC   RPTCLT(3),CLT                                                    
         ZIC   R0,BUYKEST          ESTIMATE                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TPEST,DUB                                                        
         MVC   RPTEST,TPEST                                                     
WRT25    CLC   MED,=C'XN'          IS IT NETWORK RADIO                          
         BNE   WRT25A                                                           
         CLC   AGY,=C'NW'          IS IT NW AYER                                
         BE    WRTNW               * GOTO NW AYER NETWORK RADIO                 
         SPACE                                                                  
WRT25A   MVC   TPSTA(5),STA        STATION CALL LETTERS                         
         CLI   STA+4,C'T'                                                       
         BE    WRT26                                                            
         CLI   STA+4,C'A'                                                       
         BE    WRT27                                                            
         CLI   STA+4,C'F'                                                       
         BE    WRT28                                                            
         B     WRT50               T/A/F/NOT FOUND                              
WRT26    MVI   TPSTA+5,C'V'                                                     
         B     WRT50                                                            
WRT27    MVI   TPSTA+5,C'M'                                                     
         B     WRT50                                                            
WRT28    MVI   TPSTA+5,C'M'                                                     
WRT50    MVC   RPTSTA,TPSTA                                                     
         B     WRT53                                                            
         SPACE                                                                  
WRTNW    MVC   TPPRG(6),STA        * ADDED FOR NW AYER NETWORK RADIO            
         MVC   RPTPRG(6),STA       * STA INTO NTWRK CODE                        
         MVI   ELCODE,X'01'        * AND PRG NAME INTO STA CALL LETTERS         
         LA    R8,BUYREC+24        *                                            
         CLC   0(1,R8),ELCODE      *                                            
         BE    WRTNW2              *                                            
         BAS   RE,NXTELEM          *                                            
         BNE   WRT53               *                                            
WRTNW2   DS    0H                  *                                            
         USING BDELEM,R8                                                        
         MVC   TPSTA(6),BDPROGRM   *                                            
         MVC   RPTSTA(6),BDPROGRM  *                                            
         DROP  R8                                                               
         SPACE                                                                  
WRT53    MVC   TPDPT(1),BDDAYPT     DAYPART                                     
         MVC   RPTDPT(1),BDDAYPT                                                
         ZIC   R0,BDSEC             SPOT LENGTH                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TPSLEN,DUB                                                       
         MVC   RPTSLEN,TPSLEN                                                   
         EJECT                                                                  
*                                                                               
* GOTO BINTBL AND GET VALUES *                                                  
         SPACE                                                                  
         L     R5,ADCLT            PRODUCT                                      
         USING CLTHDR,R5                                                        
         LA    R1,CLIST                                                         
WRT55    CLI   0(R1),C' '                                                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   2(1,R7),3(R1)                                                    
         BE    WRT57                                                            
         LA    R1,4(R1)                                                         
         B     WRT55                                                            
WRT57    MVC   TPPRD,0(R1)                                                      
         MVC   RPTPRD,TPPRD                                                     
         SPACE                                                                  
         CLC   =X'0000',3(R7)     WAS THERE AN AFFID FOR THIS ELEM              
         BE    WRT60                                                            
         SPACE                                                                  
* AFFID ELEM *                                                                  
         GOTO1 DATCON,DMCB,(2,3(R7)),TPDATE        YES. AFFID DATE              
         MVC   RPTDATE,TPDATE                                                   
         GOTO1 GETDAY,DMCB,TPDATE,WORK             AFFID DAY OF WEEK            
         ZIC   R2,0(R1)                                                         
         EDIT  (R2),(7,TPDAYS),ALIGN=LEFT                                       
         MVC   RPTDAYS,TPDAYS                                                   
         XC    WORK(4),WORK                                                     
         MVC   WORK(2),5(R7)                        AFFID START TIME            
         NI    WORK,X'0F'                           CLEAR STATUS BITS           
         BAS   RE,TIMERTN                                                       
         MVC   TPSTRTM,WORK                                                     
         MVC   RPTSTRTM,TPSTRTM                                                 
         B     WRT72                                                            
         SPACE                                                                  
* NO AFFID ELEM *                                                               
WRT60    GOTO1 DATCON,DMCB,(2,0(R7)),TPDATE                                     
         MVC   RPTDATE,TPDATE                                                   
         XC    WORK(4),WORK                                                     
         MVC   WORK(2),BDTIMST                                                  
         BAS   RE,TIMERTN                           START TIME                  
         MVC   TPSTRTM,WORK                                                     
         MVC   RPTSTRTM,TPSTRTM                                                 
         OC    BDTIMEND,BDTIMEND                                                
         BNZ   WRT65                                                            
         MVC   TPENDTM,TPSTRTM                                                  
         MVC   RPTENDTM,TPENDTM                                                 
         B     WRT70                                                            
WRT65    XC    WORK(4),WORK                                                     
         MVC   WORK(2),BDTIMEND                                                 
         BAS   RE,TIMERTN                           END TIME                    
         MVC   TPENDTM,WORK                                                     
         MVC   RPTENDTM,TPENDTM                                                 
         EJECT                                                                  
WRT70    DS    0H                  DAYS OF THE WEEK                             
         LA    R5,TPDAYS                                                        
         TM    BDDAY,X'40'         MONDAY                                       
         BZ    W70A                                                             
         MVI   0(R5),C'1'                                                       
         AH    R5,=H'1'                                                         
W70A     TM    BDDAY,X'20'         TUESDAY                                      
         BZ    W70B                                                             
         MVI   0(R5),C'2'                                                       
         AH    R5,=H'1'                                                         
W70B     TM    BDDAY,X'10'         WEDNESDAY                                    
         BZ    W70C                                                             
         MVI   0(R5),C'3'                                                       
         AH    R5,=H'1'                                                         
W70C     TM    BDDAY,X'08'         THURSDAY                                     
         BZ    W70D                                                             
         MVI   0(R5),C'4'                                                       
         AH    R5,=H'1'                                                         
W70D     TM    BDDAY,X'04'         FRIDAY                                       
         BZ    W70E                                                             
         MVI   0(R5),C'5'                                                       
         AH    R5,=H'1'                                                         
W70E     TM    BDDAY,X'02'         SATURDAY                                     
         BZ    W70F                                                             
         MVI   0(R5),C'6'                                                       
         AH    R5,=H'1'                                                         
W70F     TM    BDDAY,X'01'         SUNDAY                                       
         BZ    WRT72                                                            
         MVI   0(R5),C'7'                                                       
         SPACE                                                                  
WRT72    MVC   RPTDAYS,TPDAYS                                                   
         SPACE                                                                  
         ICM   RE,15,7(R7)         COST                                         
         CVD   RE,DUB                                                           
         LTR   RE,RE                                                            
         BM    *+8                                                              
         OI    DUB+7,X'0F'                                                      
         UNPK  TPCOST,DUB                                                       
         MVC   RPTCOST,TPCOST                                                   
         SPACE                                                                  
         CLC   MED,=C'NT'           NETWORK PROGRAM CODE                        
         BNE   WRT73                                                            
         CLC   11(4,R7),=C'0000'   IF NTI = 0000,                               
         BNE   WRT72A              THEN GET PROGRAM NAME                        
         MVI   ELCODE,X'92'                                                     
         LA    R8,BUYREC+24                                                     
         BAS   RE,NXTELEM                                                       
         BNE   WRT73                                                            
         USING NPGEL92,R8                                                       
         CLC   =C'BONUS',NPGNAME   * ADDED FOR NW AYER NETWORK                  
         BE    NWA1                *                                            
         CLC   =C'MAKEGD',NPGNAME  *                                            
         BE    NWA1                *                                            
         MVC   TPPRG(6),NPGNAME                                                 
         MVC   RPTPRG(6),NPGNAME                                                
         B     WRT73                                                            
NWA1     LA    R8,BUYREC+24        *                                            
         MVI   ELCODE,X'01'        *                                            
         CLC   0(1,R8),ELCODE      *                                            
         BE    NWA2                *                                            
         BAS   RE,NXTELEM          *                                            
         BNE   WRT73               *                                            
NWA2     DS    0H                                                               
         USING BDELEM,R8           *                                            
         MVC   TPPRG(6),BDPROGRM   *                                            
         MVC   RPTPRG(6),BDPROGRM  *                                            
         B     WRT73                                                            
         DROP  R8                                                               
         SPACE                                                                  
WRT72A   MVC   TPPRG(4),11(R7)     NTI CODE                                     
         MVC   RPTPRG(4),11(R7)                                                 
         SPACE                                                                  
WRT73    AP    ANSTOTS,15(2,R7)    NUMB OF ANNOUNCEMENTS                        
         UNPK  WORK(3),15(2,R7)                                                 
         OI    WORK+2,X'F0'                                                     
         MVC   TPANSMT,WORK                                                     
         MVC   RPTANSMT,TPANSMT                                                 
         SPACE                                                                  
         MVI   ELCODE,X'66'        LEAD GENERATION                              
         LA    R8,BUYREC+24                                                     
WRT75    BAS   RE,NXTELEM                                                       
         BNE   WRT80                                                            
         CLC   =C'LEAD=',3(R8)                                                  
         BNE   WRT75                                                            
         MVC   TPLDGEN,8(R8)                                                    
         MVC   RPTLDGEN,TPLDGEN                                                 
         SPACE                                                                  
WRT80    DS    0H                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   WRT80A                                                           
         MVC   P(97),REPREC                                                     
         GOTO1 REPORT                                                           
WRT80A   PUT   SPXMTP,TAPREC                                                    
         SPACE                                                                  
WRT80B   LA    R7,17(R7)            INCREMENT TABLE                             
         CLI   0(R7),0                                                          
         BE    EXIT                END OF BINTBL                                
         CLC   15(2,R7),=X'0000'   TEST NUMB OF SPOTS                           
         BE    WRT80B              NO SPOTS,GET NXT BINTBL ENTRY                
         XC    TAPREC,TAPREC                                                    
         XC    REPREC,REPREC                                                    
         B     WRT05                                                            
         EJECT                                                                  
*                                                                               
TIMERTN  DS    0H                  CONVERT MILITARY TIME TO STANDARD            
         SPACE                                                                  
         CLC   =C'VARY',BDTIMST        IS TIME = 'VARIOUS'                      
         BE    TIME40                                                           
         SPACE                                                                  
         SR    R0,R0                                                            
         ICM   R0,3,WORK                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB                                                      
         CLC   WORK(4),=C'1200'                                                 
         BNE   TIME03                                                           
         MVI   WORK+4,C'N'         EQUALS 1200                                  
         BR    RE                                                               
TIME03   CLC   WORK(4),=C'0060'    UNDER 60                                     
         BNL   TIME05                                                           
         MVC   WORK(2),=C'12'                                                   
         MVI   WORK+4,C'A'                                                      
         BR    RE                                                               
TIME05   CLC   WORK(4),=C'1200'                                                 
         BNL   TIME10                                                           
         MVI   WORK+4,C'A'         UNDER 1200                                   
         BR    RE                                                               
TIME10   CLC   WORK(4),=C'2400'    EQUALS 2400                                  
         BNE   TIME20                                                           
         MVC   WORK(5),=C'1200M'                                                
         BR    RE                                                               
TIME20   CLC   WORK(4),=C'1300'                                                 
         BNL   TIME30                                                           
         MVI   WORK+4,C'P'         UNDER 1300                                   
         BR    RE                                                               
TIME30   S     R0,=F'1200'         OVER 1300                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB                                                      
         MVI   WORK+4,C'P'                                                      
         BR    RE                                                               
TIME40   MVC   TPSTRTM,=C'1200N'                                                
         MVC   RPTSTRTM,=C'1200N'                                               
         MVC   TPENDTM,=C'1200M'                                                
         MVC   RPTENDTM,=C'1200M'                                               
         B     WRT70                                                            
         EJECT                                                                  
*                                                                               
         DS    F                                                                
NTIRTN   DS    0H                  GET NTI FROM X'92' ELEM                      
         ST    R1,NTIRTN-4                                                      
         MVI   ELCODE,X'92'                                                     
         LA    R8,BUYREC+24                                                     
         BAS   RE,NXTELEM                                                       
         BNE   NTI10               NO X'92'ELEM                                 
         USING NPGEL92,R8                                                       
         SR    R1,R1                                                            
         ICM   R1,3,NPGPPNO                                                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  NTISAVE,DUB                                                      
         DROP  R8                                                               
         MVI   NTISW,1                                                          
NTI10    L     R1,NTIRTN-4                                                      
         BR    R1                                                               
         SPACE                                                                  
*                                                                               
NXTELEM  DS    0H                                                               
         CLI   0(R8),0                                                          
         BE    NXTELX                                                           
         ZIC   R0,1(R8)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R8,R0                                                            
         CLC   ELCODE,0(R8)                                                     
         BNE   NXTELEM                                                          
         BR    RE                  EXIT WITH CC =                               
NXTELX   LTR   RE,RE                                                            
         BR    RE                  EXIT WITH CC NOT =                           
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SPXMWRKD DSECT                                                                  
*                                                                               
BINPARS  DS    6F                                                               
DOLTOTS  DS    D                                                                
ANSTOTS  DS    F                                                                
ELCODE   DS    CL1                                                              
BLOPSW   DS    CL1                                                              
OTOSW    DS    CL1                                                              
SPOTSV   DS    CL2                                                              
NTISAVE  DS    CL4                                                              
NTISW    DS    CL1                                                              
*                                                                               
TAPREC   DS    0CL80               *** TAPE RECORD ***                          
TPAGY    DS    CL2                 AGENCY                                       
TPMED    DS    CL2                 MEDIA                                        
TPCLT    DS    CL4                 CLIENT                                       
TPPRD    DS    CL3                 PRODUCT                                      
TPEST    DS    CL6                 ESTIMATE                                     
TPDATE   DS    CL6                 DATE(YYMMDD)                                 
         DS    CL2                 SPARE                                        
TPSTA    DS    CL6                 STATION CALL LETTERS (PLUS AM/FM)            
TPPRG    DS    CL6                 PROGRAM CODE(NETWORK)                        
TPDPT    DS    CL3                 DAYPART CODE                                 
TPSLEN   DS    CL3                 SPOT LENGTH                                  
TPSTRTM  DS    CL5                 START TIME                                   
TPENDTM  DS    CL5                 END TIME                                     
TPDAYS   DS    CL7                 DAYS OF WEEK(1=MONDAY,234=TUEWEDTH)          
TPCOST   DS    CL9                 COST                                         
TPANSMT  DS    CL3                 NUMBER OF ANNOUNCEMENTS                      
TPLDGEN  DS    CL1                 LEAD GENERATION CODE                         
         DS    CL6                 SPARE                                        
TPBKDT   DS    CL1                 BACK UP DATA                                 
*                                                                               
         EJECT                                                                  
*                                                                               
REPREC   DS    0CL97               *** REPORT RECORD ***                        
RPTLNM   DS    CL3                 BUY LINE NUMBER                              
         DS    CL1                                                              
RPTAGY   DS    CL2                 AGENCY                                       
         DS    CL1                                                              
RPTMED   DS    CL2                 MEDIA                                        
         DS    CL1                                                              
RPTCLT   DS    CL4                 CLIENT                                       
         DS    CL1                                                              
RPTPRD   DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
RPTEST   DS    CL6                 ESTIMATE                                     
         DS    CL1                                                              
RPTDATE  DS    CL6                 DATE(YYMMDD)                                 
         DS    CL2                 SPARE                                        
RPTSTA   DS    CL6                 STATION CALL LETTERS (PLUS AM/FM)            
         DS    CL1                                                              
RPTPRG   DS    CL6                 PROGRAM CODE(NETWORK)                        
         DS    CL1                                                              
RPTDPT   DS    CL3                 DAYPART CODE                                 
         DS    CL1                                                              
RPTSLEN  DS    CL3                 SPOT LENGTH                                  
         DS    CL1                                                              
RPTSTRTM DS    CL5                 START TIME                                   
         DS    CL1                                                              
RPTENDTM DS    CL5                 END TIME                                     
         DS    CL1                                                              
RPTDAYS  DS    CL7                 DAYS OF WEEK(1=MONDAY,234=TUEWEDTH)          
         DS    CL1                                                              
RPTCOST  DS    CL9                 COST                                         
         DS    CL1                                                              
RPTANSMT DS    CL3                 NUMBER OF ANNOUNCEMENTS                      
RPTLDGEN DS    CL1                 LEAD GENERATION CODE                         
         DS    CL6                 SPARE                                        
RPTBKDT  DS    CL1                 BACK UP DATA                                 
*                                                                               
         SPACE                                                                  
BINTBL   DS    0CL1701                                                          
*RDATE   DS    CL2(B)       *DSECT FOR BINTBL*                                  
*RPPRD   DS    CL1(B)                                                           
*ADATE   DS    CL2(B)                                                           
*ATIME   DS    CL2          (MILITARY TIME)                                     
*COST    DS    CL4                                                              
*NTI     DS    CL4         (FOR NETWORK ONLY)                                   
*ANSMNT  DS    PL2         NUMBER OF ANNOUNCEMENTS                              
*                                                                               
         CSECT                                                                  
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENPROG                                                      
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'081SPREPXM02 05/01/02'                                      
         END                                                                    
