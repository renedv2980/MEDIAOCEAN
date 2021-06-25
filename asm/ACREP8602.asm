*          DATA SET ACREP8602  AT LEVEL 012 AS OF 05/01/02                      
*PHASE AC8602A,+0                                                               
*INCLUDE SQUASHER                                                               
*INCLUDE UNDERLIN                                                               
*INCLUDE COVAIL                                                                 
*INCLUDE DLFLD                                                                  
         TITLE 'T/E ANALYSIS REPORT'                                            
AC8602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC86**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC86D,RC                                                         
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING AC8602+4096,R9                                                   
         EJECT                                                                  
*              RUNFRST -INITIALIZE BUFFALO                                      
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   TE00                                                             
         RELOC RELO                                                             
         L     R1,=V(SQUASHER)                                                  
         A     R1,RELO                                                          
         ST    R1,SQUASHER                                                      
         L     R1,=V(UNDERLIN)                                                  
         A     R1,RELO                                                          
         ST    R1,UNDERLIN                                                      
         L     R7,=A(BUFFALOC)                                                  
         A     R7,RELO                                                          
         ST    R7,ABUFF                                                         
*&&UK                                                                           
         L     R7,=V(DLFLD)                                                     
         A     R7,RELO                                                          
         ST    R7,DOWNLOAD                                                      
*&&                                                                             
*&&US                                                                           
         GOTO1 =V(COVAIL),DMCB,C'SETB',20000,300000,(R7)                        
         MVC   ABUFF,12(R1)                                                     
*&&                                                                             
         ZAP   MAXCNT,=P'600'                                                   
         B     TEXIT                                                            
         EJECT                                                                  
*              REQFRST -INITIALIZE BUFFALO                                      
*                  SET UP DATES                                                 
         SPACE 2                                                                
TE00     CLI   MODE,REQFRST                                                     
         BNE   TE20                                                             
         MVC   PAGE,=H'1'                                                       
         MVI   FCRDACC,C'Y'        RESET FOR READS                              
         MVI   FCRDHIST,C'Y'                                                    
         ZAP   MINIMP,=P'0'                                                     
         XC    MINIMB,MINIMB                                                    
*&&US                                                                           
         CLC   QSRTAREA(3),SPACES  ANY MINIMUM AMOUNT PROFILE                   
         BE    TE03                                                             
         XR    R1,R1                                                            
         CLI   QSRTAREA+1,C' '                                                  
         BE    TE01                                                             
         LA    R1,1                                                             
         CLI   QSRTAREA+2,C' '                                                  
         BE    TE01                                                             
         LA    R1,2                                                             
TE01     EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  MINIMP,QSRTAREA(0)                                               
         MP    MINIMP,=P'100'          REQUEST HELD IN UNITS OF 100             
         CVB   RF,MINIMP                                                        
         ST    RF,MINIMB                                                        
*&&                                                                             
TE03     MVI   RCSUBPRG,0                                                       
         CLI   QLEDGER,C'P'        STAFF ANALYSIS                               
         BE    TE1                                                              
         MVI   RCSUBPRG,1          MUST BE CLIENT                               
TE1      MVI   GROUP,C'N'                                                       
*&&UK                                                                           
         CLI   QOPT7,C'Y'          DOWNLOADING                                  
         BNE   TE2                                                              
         MVI   DLFRST,C'Y'         SET FOR HEADLINE PRINTING                    
         MVI   RCSUBPRG,2                                                       
         LA    R8,DLBUFF                                                        
         USING DLCBD,R8                                                         
         LA    RE,DLPLINE          PRINT-LINE                                   
         ST    RE,DLCBAPL                                                       
         LA    RE,DLPRINT                                                       
         ST    RE,DLCBAPR          PRINT ROUTINE                                
         MVI   DLCBACT,DLCBSOR     START NEW REPORT                             
         GOTO1 DOWNLOAD,(R8)                                                    
         MVC   DLPLINE,SPACES                                                   
*&&                                                                             
TE2      ZAP   SVDCNTR,=P'0'                                                    
         ZAP   DCNTR,=P'0'                                                      
         XC    DEPTOT,DEPTOT                                                    
         SPACE 1                                                                
         XC    PDATES(56),PDATES                                                
         MVC   PLIN(77),SPACES                                                  
         MVC   PLIN+77(77),SPACES                                               
         SPACE 1                                                                
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   PSTRT,WORK+6                                                     
         MVC   PERIOD,SPACES                                                    
         MVI   DMCB+4,6                                                         
         BASR  RE,RF                                                            
         MVC   PERIOD(6),WORK+6                                                 
         SPACE 1                                                                
         MVC   WORK(4),QEND                                                     
         MVI   DMCB+4,1                                                         
         BASR  RE,RF                                                            
         MVC   PEND,WORK+6                                                      
         MVI   DMCB+4,6                                                         
         BASR  RE,RF                                                            
         CLC   PSTRT,PEND                                                       
         BE    *+16                                                             
         MVC   PERIOD+10(6),WORK+6                                              
         MVC   PERIOD+7(2),=C'TO'                                               
*&&UK                                                                           
         MVI   PERIOD+3,C' '                                                    
         MVI   PERIOD+13,C' '                                                   
*&&                                                                             
         SPACE 1                                                                
         SR    R2,R2               GET NUMBER OF MONTHS                         
         SR    R3,R3                                                            
         IC    R2,PSTRT+1                                                       
         CH    R2,=H'10'                                                        
         BL    *+8                                                              
         SH    R2,=H'6'                                                         
         IC    R3,PEND+1                                                        
         CH    R3,=H'10'                                                        
         BL    *+8                                                              
         SH    R3,=H'6'                                                         
         SR    R3,R2                                                            
         A     R3,=F'1'                                                         
         CLC   PSTRT(1),PEND                                                    
         BE    *+8                                                              
         AH    R3,=H'12'                                                        
         SPACE 1                                                                
         STC   R3,ELCODE           LOOK FOR MINI-ELEMENT                        
         LA    R4,MONTB                                                         
TE12     BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                REQUEST OPTION NOT IN TABLE                  
         CLI   2(R4),C'Z'          IF Z IGNORE OPTION                           
         BE    *+14                                                             
         CLC   QOPT3,2(R4)                                                      
         BNE   TE12                                                             
         SPACE 1                                                                
         MVC   WORK(1),PSTRT       YEAR                                         
         MVI   WORK+1,X'0F'        PACKED                                       
         MVC   WORK+2(1),PSTRT+1   MONTH                                        
         MVI   WORK+3,X'0F'        PACKED                                       
         SPACE 1                                                                
         IC    R3,1(R4)            LENGTH                                       
         SH    R3,=H'3'            NUMBER OF BUCKETS                            
         CH    R3,=H'7'                                                         
         BL    *+6                                                              
         DC    H'0'                TABLE ERROR- MORE THAN SIX BUCKETS           
         SPACE 1                                                                
         ST    R3,CNT                                                           
         LA    R5,PDATES                                                        
         MVC   0(2,R5),PSTRT                                                    
         MVC   2(2,R5),PEND                                                     
         CH    R3,=H'1'            ONE ITEM                                     
         BE    TE17                                                             
         SPACE 1                                                                
TE13     IC    R2,3(R4)            NUMBER OF MONTHS IN THIS BUCKET              
         BCT   R2,*+8                                                           
         B     *+12                                                             
         BAS   RE,ADDMON           ADD ONE MONTH                                
         B     *-12                                                             
         MVC   2(1,R5),WORK        YEAR FOR END                                 
         MVC   3(1,R5),WORK+2      MONTH FOR END                                
         SPACE 1                                                                
         BAS   RE,ADDMON           NEW START MONTH                              
         LA    R5,4(R5)            PDATES                                       
         LA    R4,1(R4)            NEXT NUMBER OF MONTHS                        
         MVC   0(1,R5),WORK        START YEAR                                   
         MVC   1(1,R5),WORK+2      START MONTH                                  
         BCT   R3,TE13             NUMBER OF BUCKETS                            
         SPACE 1                                                                
TE17     LA    R4,PLIN             HEADLINE                                     
         LA    R3,PDATES                                                        
         BAS   RE,HEADMON                                                       
         CLI   QOPT2,C'Y'          INCLUDE LAST YEAR                            
         BNE   TE19                                                             
         SPACE 1                                                                
         LA    R4,PDATES           SET UP LAST YEARS                            
         LA    R3,PDATES+28                                                     
         L     R5,CNT                                                           
         XR    R6,R6                                                            
         SPACE 1                                                                
TE18     MVC   0(4,R3),0(R4)                                                    
         IC    R6,0(R3)                                                         
         BCTR  R6,0                                                             
         TM    0(R3),X'0F'         1980                                         
         BM    *+8                                                              
         SH    R6,=H'6'                                                         
         STC   R6,0(R3)                                                         
         IC    R6,2(R3)                                                         
         BCTR  R6,0                                                             
         TM    2(R3),X'0F'                                                      
         BM    *+8                                                              
         SH    R6,=H'6'                                                         
         STC   R6,2(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,TE18                                                          
         SPACE 1                                                                
         LA    R4,PLIN+77                                                       
         LA    R3,PDATES+28                                                     
         BAS   RE,HEADMON                                                       
TE19     B     TEXIT                                                            
         SPACE 2                                                                
ADDMON   CP    WORK+2(2),=P'120'   DECEMBER                                     
         BNE   *+16                                                             
         AP    WORK(2),=P'10'      ADD 1 TO YEAR                                
         ZAP   WORK+2(2),=P'0'                                                  
         AP    WORK+2(2),=P'10'                                                 
         BR    RE                                                               
         EJECT                                                                  
*              BUILD HEADLINES  FOR MONTHS FROM PDATES                          
         SPACE 2                                                                
HEADMON  NTR1                                                                   
         L     R5,CNT                                                           
         SPACE 1                                                                
HEDM1    MVC   WORK(2),0(R3)                                                    
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,WORK+3)                                  
         MVC   5(3,R4),WORK+3      MMM                                          
         MVC   8(2,R4),WORK+7      YY                                           
         CLC   0(2,R3),2(R3)       SAME END                                     
         BE    HEDLOP                                                           
         SPACE 1                                                                
         MVC   WORK(2),2(R3)       SET UP FOR SPAN MMM-MMM/YY                   
         MVC   1(3,R4),WORK+3                                                   
         MVI   4(R4),C'-'                                                       
         GOTO1 (RF)                                                             
         MVC   5(3,R4),WORK+3                                                   
         MVC   8(2,R4),WORK+7                                                   
         SPACE 1                                                                
HEDLOP   LA    R4,11(R4)                                                        
         LA    R3,4(R3)                                                         
         BCT   R5,HEDM1                                                         
         SPACE 1                                                                
         CLI   CNT+3,1                                                          
         BE    TEXIT                                                            
         MVC   0(11,R4),=C'     TOTAL '                                         
         B     TEXIT                                                            
         EJECT                                                                  
*              LEDGFRST  - GET WORK CODES                                       
         SPACE 2                                                                
TE20     CLI   MODE,LEDGFRST                                                    
         BNE   TE30A                                                            
         CLI   QLEDGER,C'9'                                                     
         BNE   *+8                                                              
         BAS   RE,GET2P                                                         
         L     R4,ADLDGHIR                                                      
         LA    R4,ACHRLEVD-ACHEIRD(R4)                                          
         LA    R5,LEVNTAB+(3*L'LEVNTAB)                                         
         LA    R2,4                                                             
         LA    R3,L'LEVNTAB                                                     
         XC    BYTE,BYTE                                                        
         USING LEVNAMD,R5                                                       
TE20A    MVC   LEVNDESC,SPACES                                                  
         XC    LEVNCONT,LEVNCONT                                                
         CLI   0(R4),0                                                          
         BE    TE20B                                                            
         OC    BYTE,BYTE                                                        
         BNZ   *+8                                                              
         STC   R2,BYTE                                                          
         MVC   LEVNDESC,1(R4)                                                   
TE20B    SH    R4,=H'16'                                                        
         SR    R5,R3                                                            
         BCT   R2,TE20A                                                         
         SPACE 1                                                                
         IC    R2,BYTE             BYTE NOW HAS NO OF LEVELS THIS LEDG          
         TM    QOPT4,X'0F'         QOPT4 = REQUESTED LEVEL OF DETAIL            
         BZ    TE20C                                                            
         OI    BYTE,X'F0'          C.V.D. FOR COMPARISON                        
         CLC   QOPT4,BYTE                                                       
         BH    TE20C               DEFAULT IS TO SHOW ALL LEVELS                
         SPACE 1                                                                
         MVC   BYTE,QOPT4                                                       
         SPACE 1                                                                
TE20C    ZIC   R3,BYTE                                                          
         SLL   R3,28                                                            
         SLDL  R2,4                                                             
         STC   R2,ELCODE                                                        
         LA    R4,RQLEVTAB                                                      
TE20D    BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RQLEVD,R4                                                        
         CLI   RQOPT,C'Z'                                                       
         BE    TE20E               IF Z IGNORE ANY POSSIBLE OPTIONS             
         SPACE 1                                                                
TE20E    ST    R4,ARQLEV                                                        
         MVC   HILEV,RQHILEV                                                    
         BAS   RE,SETBUFF                                                       
         SPACE 1                                                                
TE21     EQU   *                                                                
         L     R3,=A(WRKTB)                                                     
         MVI   ELCODE,X'12'                                                     
         L     R4,ADLEDGER                                                      
         BAS   RE,GETEL                                                         
         BE    TE22                                                             
         MVI   FCRDACC,C'N'        NO WORKCODES YOUR DONE                       
         MVI   FCRDHIST,C'N'                                                    
         MVC   P+1(33),=CL33'** ERROR ** NO WORKCODES FOUND   '                 
         GOTO1 ACREPORT                                                         
         B     TEXIT                                                            
         SPACE 1                                                                
TE22     MVC   0(17,R3),2(R4)                                                   
         LA    R3,17(R3)                                                        
         XC    0(17,R3),0(R3)                                                   
         BAS   RE,NEXTEL                                                        
         BNE   TEXIT                                                            
         B     TE22                                                             
         SPACE 2                                                                
GET2P    NTR1                                                                   
         MVI   DEPTLOC,1                                                        
         MVC   SVKEY,KEY                                                        
         MVC   KEY+1(31),SPACES                                                 
         MVC   KEY+1(2),=C'2P'                                                  
         L     R4,=A(MYIO)                                                      
         A     R4,RELO                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',KEY,(R4)                     
         CLC   KEY,0(R4)                                                        
         BNE   GET2PX                                                           
         AH    R4,DATADISP                                                      
GET2P1   CLI   0(R4),0                                                          
         BE    GET2PX                                                           
         CLI   0(R4),X'16'                                                      
         BE    GET2P3                                                           
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     GET2P1                                                           
         SPACE 2                                                                
         USING ACHEIRD,R4                                                       
GET2P3   CLI   ACHRLEVA,1                                                       
         BNE   GET2PX                                                           
         MVI   DEPTLOC,2                                                        
         SPACE 2                                                                
GET2PX   MVC   KEY,SPACES                                                       
         MVC   KEY,SVKEY                                                        
         L     R4,=A(MYIO)                                                      
         A     R4,RELO                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',KEY,(R4)                     
         CLC   KEY,0(R4)                                                        
         BE    *+6                                                              
         DC    H'0'                CANT READ LEDGER RECORD                      
         B     TEXIT                                                            
         EJECT                                                                  
*              PROCLEVA ETC.                                                    
         SPACE 2                                                                
TE30A    CLI   MODE,PROCLEVA                                                    
         BNE   TE30B                                                            
         SPACE 1                                                                
         L     R4,ADLVANAM                                                      
         L     R1,ADHEIRA                                                       
         MVI   BYTE,C'A'                                                        
         MVI   LEVAACT,C'N'                                                     
         B     TENAMOUT                                                         
         SPACE 2                                                                
TE30B    CLI   MODE,PROCLEVB                                                    
         BNE   TE30C                                                            
         SPACE 1                                                                
         L     R4,ADLVBNAM                                                      
         L     R1,ADHEIRB                                                       
         MVI   BYTE,C'B'                                                        
         MVI   LEVBACT,C'N'                                                     
         B     TENAMOUT                                                         
         SPACE 2                                                                
TE30C    CLI   MODE,PROCLEVC                                                    
         BNE   TE30D                                                            
         SPACE 1                                                                
         L     R4,ADLVCNAM                                                      
         L     R1,ADHEIRC                                                       
         MVI   BYTE,C'C'                                                        
         MVI   LEVCACT,C'N'                                                     
         B     TENAMOUT                                                         
         SPACE 2                                                                
TE30D    CLI   MODE,PROCLEVD                                                    
         BNE   TE30                                                             
         SPACE 1                                                                
         L     R4,ADLVDNAM                                                      
         L     R1,ADHEIRD                                                       
         MVI   BYTE,C'D'                                                        
         B     TENAMOUT                                                         
         SPACE 2                                                                
         USING ACNAMED,R4                                                       
TENAMOUT DS    0H                                                               
         LA    R5,LEVNTAB                                                       
         USING LEVNAMD,R5                                                       
         SPACE 1                                                                
TEN10    CLC   LEVNLET,BYTE                                                     
         BE    TEN20                                                            
         LA    R5,L'LEVNTAB(R5)                                                 
         B     TEN10                                                            
         SPACE 1                                                                
TEN20    MVC   LEVACCT,SPACES                                                   
         MVC   LEVACCT(12),3(R1)                                                
         ZIC   R2,ACNMLEN                                                       
         SH    R2,=H'3'                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   LEVACCT+13(0),ACNMNAME                                           
         SPACE 1                                                                
         CLI   LEVNLET,C'D'                                                     
         BE    TENXIT                                                           
         LA    R5,L'LEVNTAB(R5)                                                 
         XC    LEVNCONT,LEVNCONT                                                
TENXIT   EQU   *                                                                
         B     TEXIT                                                            
         EJECT                                                                  
*              PROCACC                                                          
         SPACE 2                                                                
TE30     CLI   MODE,PROCACC                                                     
         BNE   TE40                                                             
         MVI   ACCTACT,C'N'                                                     
         ZAP   CNTSUM,=P'0'                                                     
         MVI   FORCEHED,C'Y'                                                    
         BAS   R7,CLRBUF                                                        
         XC    PERSTOT,PERSTOT                                                  
         SPACE 1                                                                
         L     R4,ADACCSTA                                                      
         USING ACSTATD,R4                                                       
         CLI   ACSTCOST,C'('                                                    
         BNE   TE32                                                             
         MVI   GROUP,C'Y'                                                       
         B     TEXIT                                                            
         SPACE 1                                                                
TE32     CLI   ACSTCOST,C')'                                                    
         BNE   TEXIT                                                            
         CLI   GROUP,C'Y'                                                       
         BNE   TEXIT                                                            
         MVI   THISLEV,2                                                        
         MVI   BYTE,C'A'           MUST BE LEVEL A,NO PT O/W.                   
         BAS   RE,LEVEL                                                         
         MVI   GROUP,C'N'                                                       
         B     TEXIT                                                            
         EJECT                                                                  
*              SBACFRST                                                         
         SPACE 2                                                                
TE40     CLI   MODE,SBACFRST                                                    
         BNE   TE50                                                             
         MVI   WANT,C'N'                                                        
         SPACE 1                                                                
         L     R4,ADSUBAC          LOOK FOR * TE -                              
         USING TRSUBHD,R4                                                       
         XR    R3,R3                                                            
         CLI   TRSBACNT,C'*'                                                    
         BNE   TEXIT                                                            
         SPACE 1                                                                
         CLC   QSELECT,SPACES                                                   
         BE    TEB1                                                             
         LA    R4,QSELECT                                                       
         LA    R5,6                                                             
TEA      CLI   0(R4),X'40'                                                      
         BE    TE40A0                                                           
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,TEA                                                           
TE40A0   BCTR  R3,0                                                             
         L     R4,ADSUBAC                                                       
         EXCLC R3,QSELECT,TRSBACNT+1                                            
         BE    TEB1                                                             
         TM    QSELECT,X'40'                                                    
         BO    TEXIT                                                            
         NI    TRSBACNT+1,X'BF'                                                 
         EXCLC R3,QSELECT,TRSBACNT+1                                            
         BE    TEXIT                                                            
         OI    TRSBACNT+1,X'40'                                                 
         SPACE 1                                                                
TEB1     LA    R3,TRSBACNT+14                                                   
         LA    R5,14                                                            
TEB1A    CLI   0(R3),C' '          SCAN BACKWARDS FOR 1ST SIGNF CHAR            
         BE    TEB1B                                                            
         CLI   0(R3),C'-'                                                       
         BNE   TEB2                                                             
         B     TEB2B                                                            
TEB1B    BCTR  R3,0                                                             
         BCT   R5,TEB1A                                                         
         B     TEXIT                                                            
TEB2     CLI   0(R3),C'-'          SCAN BACKWARDS FOR RIGHTMOST '-'             
         BE    TEB3                                                             
TEB2B    BCTR  R3,0                                                             
         BCT   R5,TEB2                                                          
         B     TEXIT                                                            
         SPACE 1                                                                
TEB3     LA    R0,TRSBACNT+1                                                    
         SR    R3,R0               R3 = LENGTH OF TE CODE IN CONTRA             
TE40A    MVI   WANT,C'Y'                                                        
         MVC   TE,SPACES                                                        
         MVC   SBACNUM,SPACES                                                   
         MVC   SBACNAM,SPACES                                                   
         L     R4,ADSUBAC                                                       
         SH    R3,=H'1'                                                         
         BM    TE40B                                                            
         EXMVC R3,TE,TRSBACNT+1                                                 
*                                                                               
TE40B    CLI   PROGPROF+1,0        START POSITION IN TE CODE                    
         BE    TE42                                                             
         ZIC   RE,PROGPROF+1                                                    
         SH    RE,=H'2'                                                         
         LA    RF,TRSBACNT+1(RE)                                                
         MVC   TE,SPACES                                                        
         LR    R1,R3                                                            
         SR    R1,RE                                                            
         BM    TE42                                                             
         EXMVC R1,TE,0(RF)                                                      
*                                                                               
TE42     LA    R4,5(R3,R4)                                                      
         LA    R3,3(R3)                                                         
         LH    R5,=H'14'                                                        
         SR    R5,R3                                                            
         BM    TE43                                                             
         EXMVC R5,SBACNUM,0(R4)                                                 
*                                                                               
TE43     L     R4,ADSUBAC                                                       
         ZIC   R3,TRSBLEN                                                       
         SH    R3,=H'18'                                                        
         BM    TEXIT                                                            
         EXMVC R3,SBACNAM,TRSBNAME                                              
         B     TEXIT                                                            
         EJECT                                                                  
*              PROCHIST                                                         
         SPACE 2                                                                
TE50     CLI   MODE,PROCHIST                                                    
         BNE   TE60                                                             
         CLI   WANT,C'Y'                                                        
         BNE   TEXIT                                                            
         CLI   BUCKTYPE,C' '       REGULAR BUCKETS ONLY                         
         BNE   TEXIT                                                            
         L     R4,ADTRANS                                                       
         CLI   0(R4),X'45'                                                      
         BNE   TEXIT                                                            
         USING TRHISTD,R4                                                       
         LA    R6,TRHSCR           CREDITS                                      
         LA    R7,TRHSDR           DEBITS                                       
         CLI   QLEDGER,C'9'        CLIENT ANALYSIS                              
         BE    TE55                CREDITS MINUS DEBITS                         
         SPACE 1                                                                
         LA    R6,TRHSDR           MUST BE STAFF                                
         LA    R7,TRHSCR           DEBITS MINUS CREDITS - GIVES UNBILL          
         CLI   QOPT1,C'B'          REQUEST BOTH  BILLABLE AND UNBILLABL         
         BNE   *+14                                                             
         ZAP   TRHSCR,=P'0'                                                     
         B     TE55                                                             
         CLI   QOPT1,C'C'          BILLABLE ONLY                                
         BNE   TE55                                                             
         ZAP   TRHSDR,TRHSCR                                                    
         ZAP   TRHSCR,=P'0'                                                     
         SPACE 1                                                                
TE55     ZAP   AMT,0(6,R6)                                                      
         SP    AMT,0(6,R7)                                                      
         AP    AMT,=P'50'          ROUNDING                                     
         CP    AMT,=P'0'                                                        
         BH    *+10                                                             
         SP    AMT,=P'100'                                                      
         DP    AMT,=P'100'                                                      
         ZAP   DUB,AMT(6)                                                       
         CVB   R1,DUB                                                           
         ST    R1,FULL                                                          
         SPACE 1                                                                
         L     R5,CNT              ADD AMOUNT TO BUCKETS                        
         LA    R3,PDATES                                                        
         LA    R6,BUFBK                                                         
         L     R2,CNT              NUMBER OF MONTHS                             
         SLL   R2,2                                                             
         AR    R2,R6               R2 TO PRIOR BUCKETS                          
TE57     CLC   TRHSYEAR(2),0(R3)                                                
         BL    TE58                                                             
         CLC   TRHSYEAR(2),2(R3)                                                
         BH    TE58                                                             
         ICM   R1,15,0(R6)                                                      
         A     R1,FULL                                                          
         STCM  R1,15,0(R6)                                                      
         ICM   R1,15,PERSTOT                                                    
         A     R1,FULL                                                          
         STCM  R1,15,PERSTOT                                                    
         SPACE 1                                                                
TE58     CLI   QOPT2,C'Y'          LAST YEAR                                    
         BNE   TE59                                                             
         CLC   TRHSYEAR(2),28(R3)                                               
         BL    TE59                                                             
         CLC   TRHSYEAR(2),30(R3)                                               
         BH    TE59                                                             
         ICM   R1,15,0(R2)                                                      
         A     R1,FULL                                                          
         STCM  R1,15,0(R2)                                                      
         SPACE 1                                                                
TE59     LA    R3,4(R3)                                                         
         LA    R6,4(R6)                                                         
         LA    R2,4(R2)                                                         
         BCT   R5,TE57                                                          
         B     TEXIT                                                            
         EJECT                                                                  
*              SBACLAST                                                         
         SPACE 2                                                                
TE60     CLI   MODE,SBACLAST                                                    
         BNE   TE70                                                             
         CLI   WANT,C'Y'                                                        
         BNE   TEXIT                                                            
         SPACE 1                                                                
         MVC   BUFKY,SPACES                                                     
         MVC   BUFKY(3),TE                                                      
         MVC   BUFKY+3(10),SBACNUM                                              
         MVC   BUFKY+13(36),SBACNAM                                             
         OC    BUFBK,BUFBK                                                      
         BZ    TEXIT               NOTHING GOOD TO PRINT                        
         MVI   ACCTACT,C'Y'                                                     
         MVI   LEVCACT,C'Y'                                                     
         MVI   LEVBACT,C'Y'                                                     
         MVI   LEVAACT,C'Y'                                                     
         SPACE 1                                                                
TE61     CLI   QOPT5,C'Y'          SUMMARY ONLY                                 
         BE    TE62                                                             
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFKY                                 
TE62     MVC   SV3,BUFKY                                                        
         MVC   BUFKY(3),=3X'FF'                                                 
         SPACE 1                                                                
         CP    CNTSUM,MAXCNT                                                    
         BH    TE65                MAX 500 FOR SUMMARY                          
         CLI   QOPT5,C'N'                                                       
         BE    TE65                NO SUMMARY                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFKY                                 
         CLI   QOPT5,C' '                                                       
         BNE   *+10                IF NOT BOTH DON'T LIMIT IT TO 500            
         AP    CNTSUM,=P'1'                                                     
         CP    CNTSUM,MAXCNT                                                    
         BL    TE65                                                             
         LA    R3,1                CLEAR SUMMARY TABLE TO MAKE ROOM             
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'FF',ABUFF),(X'80',(R3))                
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFKY                                 
         SPACE 1                                                                
TE65     MVC   BUFKY(3),SV3                                                     
         XC    BUFKY+3(46),BUFKY+3 TOTALS BY T/E                                
*&&UK                                                                           
         CLI   PROGPROF+2,0        NO REQUEST SUMMARY IF MINIMUM                
         BNE   TE66                PROFILE IS IN EFFECT                         
*&&                                                                             
*&&US                                                                           
         CP    MINIMP,=P'0'        US OPTION FOR MINIMUM                        
         BNE   TE66                REQUEST OPTION IN EFFECT                     
*&&                                                                             
         L     R3,=A(REQSUM)       FOR REQUEST TOTALS                           
         BAS   RE,BINADD           ADD TO REQUEST TABLE                         
TE66     BAS   R7,CLRBUF                                                        
         B     TEXIT                                                            
         EJECT                                                                  
*              ACCLAST                                                          
         SPACE 2                                                                
TE70     CLI   MODE,ACCLAST                                                     
         BNE   TE80                                                             
         CLI   ACCTACT,C'Y'                                                     
         BNE   TEXIT                                                            
         L     R4,ADACCSTA                                                      
         USING ACSTATD,R4                                                       
         CLI   ACSTCOST,C')'                                                    
         BE    TEXIT                                                            
*&&UK                                                                           
         CLI   PROGPROF+2,0        ANY MINIMUM AMOUNT PROFILE                   
         BE    TE71                                                             
         ZIC   RF,PROGPROF+2                                                    
         MH    RF,=H'100'          PROFILE HELD IN UNITS OF 100                 
         ICM   R1,15,PERSTOT                                                    
         CR    R1,RF                                                            
         BNL   TE71                                                             
         ZIC   R2,THISLEV                                                       
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ABUFF,(X'80',(R2))                        
         B     TEXIT                                                            
*&&                                                                             
*&&US                                                                           
         CP    MINIMP,=P'0'        ANY MINIMUM AMOUNT PROFILE                   
         BE    TE71                                                             
         L     RF,MINIMB                                                        
         XR    R1,R1                                                            
         ICM   R1,15,PERSTOT                                                    
         CR    R1,RF                                                            
         BNL   TE71                                                             
         ZIC   R2,THISLEV                                                       
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ABUFF,(X'80',(R2))                        
         B     TEXIT                                                            
*&&                                                                             
         SPACE 1                                                                
TE71     L     R4,ARQLEV                                                        
         LA    R4,RQLEVEL-RQLEVD(R4)                                            
         USING LEVELD,R4                                                        
TE72     CLI   LEVNO,C'1'                                                       
         BE    TE74                                                             
         LA    R4,L'RQLEVEL(R4)                                                 
         B     TE72                                                             
         SPACE 1                                                                
TE74     MVI   THISLEV,1                                                        
         MVC   BYTE,LEVLET                                                      
         ZIC   R2,THISLEV          BUILD PARAMETER LIST FOR ADD                 
         ZIC   R3,HILEV                                                         
         SR    R3,R2                                                            
         BNP   TE76                                                             
         LA    R1,PARM+12                                                       
         LA    R0,2                                                             
TE75     ST    R0,0(R1)                                                         
         AH    R0,=H'1'                                                         
         LA    R1,4(R1)                                                         
         BCT   R3,TE75                                                          
         SH    R1,=H'4'                                                         
         OI    0(R1),X'80'         END OF LIST                                  
         GOTO1 BUFFALO,PARM,=C'ADD',ABUFF,(R2)                                  
TE76     MVC   THISLEV,LEVBUF                                                   
         NI    THISLEV,X'0F'                                                    
         BAS   RE,LEVEL                                                         
         B     TEXIT                                                            
         EJECT                                                                  
*              LEVALAST ETC.                                                    
         SPACE 2                                                                
TE80     CLI   MODE,LEVALAST                                                    
         BNE   TE81                                                             
         CLI   LEVAACT,C'Y'                                                     
         BNE   TEXIT                                                            
         MVI   BYTE,C'A'                                                        
         B     TE85                                                             
         SPACE 1                                                                
TE81     CLI   MODE,LEVBLAST                                                    
         BNE   TE82                                                             
         CLI   LEVBACT,C'Y'                                                     
         BNE   TEXIT                                                            
         MVI   BYTE,C'B'                                                        
         B     TE85                                                             
         SPACE 1                                                                
TE82     CLI   MODE,LEVCLAST                                                    
         BNE   TE90                                                             
         CLI   LEVCACT,C'Y'                                                     
         BNE   TEXIT                                                            
         MVI   BYTE,C'C'                                                        
         SPACE 1                                                                
TE85     L     R4,ARQLEV                                                        
         LA    R4,RQLEVEL-RQLEVD(R4)                                            
         USING LEVELD,R4                                                        
TE86     CLC   BYTE,LEVLET                                                      
         BE    TE87                                                             
         LA    R4,L'RQLEVEL(R4)                                                 
         B     TE86                                                             
         SPACE 1                                                                
TE87     MVC   THISLEV,LEVBUF                                                   
         NI    THISLEV,X'0F'                                                    
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,LEVEL                                                         
         B     TEXIT                                                            
         EJECT                                                                  
         SPACE 2                                                                
TE90     CLI   MODE,REQLAST                                                     
         BNE   TEXIT                                                            
*&&UK                                                                           
         CLI   PROGPROF+2,0        NO REQUEST SUMMARY IF MINIMUM                
         BNE   TEXIT               PROFILE IS IN EFFECT                         
         CLI   RCSUBPRG,2          OR IF DOWNLOADING                            
         BNE   TE90A                                                            
         LA    R8,DLBUFF                                                        
         MVI   DLCBACT,DLCBEOR     FINISH OFF REPORT                            
         GOTO1 DOWNLOAD,(R8)                                                    
         B     TEXIT                                                            
*&&                                                                             
*&&US                                                                           
         CP    MINIMP,=P'0'        NO REQUEST SUMMARY IF MINIMUM                
         BNE   TEXIT               PROFILE IS IN EFFECT                         
*&&                                                                             
TE90A    MVC   BUFKY,SPACES                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVC   MYHEAD1,SPACES                                                   
         MVC   MYHEAD2,SPACES                                                   
         MVC   MYHEAD3,SPACES                                                   
         MVC   MYHEAD4,SPACES                                                   
         MVC   MYHEAD1(14),=C'REQUEST TOTALS'                                   
         MVC   P+1(15),=C'REQUEST SUMMARY'                                      
         MVI   PSECOND+1,C'-'                                                   
         MVC   PSECOND+2(14),PSECOND+1                                          
         MVI   SPACING,2                                                        
         BAS   RE,PLINE                                                         
         SPACE 1                                                                
         L     R3,=A(REQSUM)                                                    
         BAS   RE,BINGET                                                        
         OC    DMCB(4),DMCB                                                     
         BZ    TEXIT                                                            
         MVC   TOTS,BUFKY                                                       
TE91     MVC   TENAME,SPACES                                                    
         MVC   P+1(3),BUFKY                                                     
         L     R4,=A(WRKTB)                                                     
         LA    R5,500                                                           
         LA    RF,BUFKY+1                                                       
         CLI   PROGPROF+3,C'Y'                                                  
         BNE   *+8                                                              
         LA    RF,BUFKY            USE FIRST 2 FOR NAME                         
         CLC   0(2,RF),0(R4)                                                    
         BE    *+16                                                             
         LA    R4,17(R4)                                                        
         BCT   R5,*-14                                                          
         B     *+10                                                             
         SPACE 1                                                                
         MVC   TENAME,2(R4)                                                     
         MVC   P+5(15),TENAME                                                   
         LA    R5,BUFBK                                                         
         MVI   SPACING,2                                                        
         BAS   RE,PRNBK                                                         
         SPACE 1                                                                
         L     R3,=A(REQSUM)                                                    
         BAS   RE,BINGET                                                        
         OC    DMCB(4),DMCB                                                     
         BZ    TE92                                                             
         LA    R3,BUFBK                                                         
         LA    R4,TOTS+49                                                       
         ZIC   R5,BKCNT            BUCKETS                                      
TE91A    ICM   R1,15,0(R4)                                                      
         ICM   R2,15,0(R3)                                                      
         AR    R1,R2                                                            
         STCM  R1,15,0(R4)                                                      
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,TE91A                                                         
         B     TE91                                                             
         SPACE 1                                                                
TE92     MVC   P+1(13),=C'REQUEST TOTAL'                                        
         LA    R5,TOTS+49                                                       
         BAS   RE,PRNBK                                                         
         SPACE 3                                                                
TEXIT    XIT1                                                                   
         EJECT                                                                  
LEVEL    NTR1                                                                   
         L     R4,ARQLEV                                                        
         LA    R4,RQLEVEL-RQLEVD(R4)                                            
         USING LEVELD,R4                                                        
         LA    R7,LEVNTAB                                                       
         SPACE 1                                                                
LE10     CLC   LEVLET,BYTE                                                      
         BE    LE20                                                             
         LA    R4,L'RQLEVEL(R4)                                                 
         LA    R7,L'LEVNTAB(R7)                                                 
         B     LE10                                                             
         SPACE 1                                                                
LE20     CLI   LEVWANT,C'Y'                                                     
         BNE   LEXIT                                                            
         ZIC   R3,LEVNCONT-LEVNAMD(R7)  UPDATE COUNT EVEN IF NOT                
         LA    R3,1(R3)                  PRINTING LEVEL                         
         STC   R3,LEVNCONT-LEVNAMD(R7)                                          
         CLI   LASTLEV,C'Y'        IF NOT LAST LEV,SOMETHING IS WANTED          
         BE    LE40                FURTHER DOWN.                                
         LR    R5,R7                                                            
         USING LEVNAMD,R5                                                       
LE30     LA    R4,L'RQLEVEL(R4)    BUMP THRO ELEMENTS TO FIND NEXT ONE          
         LA    R5,L'LEVNTAB(R5)    WANTED                                       
         CLI   LEVWANT,C'Y'                                                     
         BNE   LE30                                                             
         SPACE 1                                                                
         CLI   LEVNCONT,1          HAVE WE PRINTED MORE THAN ONE                
         BNH   LEXIT               IF NOT,GO AWAY                               
         SPACE 1                                                                
LE40     ST    R7,FULL                                                          
         MVC   MYHEAD1,SPACES                                                   
         MVC   MYHEAD2,SPACES                                                   
         MVC   MYHEAD3,SPACES                                                   
         MVC   MYHEAD4,SPACES                                                   
         MVC   DLHEADS,SPACES                                                   
         LA    R2,DLHEADS                                                       
         LA    R3,MYHEAD1                                                       
         LA    R5,LEVNTAB          PREPARE FOR BXLE- R7 IS ALREADY SET          
         LA    R6,L'LEVNTAB                                                     
         SPACE 1                                                                
LE50     CR    R5,R7                                                            
         BNE   LE60                                                             
         MVC   ACCT,LEVACCT+13     SAVE THIS LEVEL'S INFO                       
         MVC   SMHEAD(L'LEVNDESC),LEVNDESC                                      
         MVC   SMHEAD+L'LEVNDESC(8),=C' SUMMARY'                                
         GOTO1 SQUASHER,DMCB,SMHEAD,30                                          
         SPACE 1                                                                
LE60     CLI   RCSUBPRG,2                                                       
         BNE   LE65                                                             
         MVC   0(L'LEVNDESC,R2),LEVNDESC                                        
         LA    R2,L'LEVNDESC(R2)                                                
         MVC   1(L'LEVACCT,R3),LEVACCT                                          
         LA    R3,1(R3)            NO SQUASHING IF DOWNLOADING                  
         B     LE70                                                             
LE65     MVC   1(L'LEVNDESC,R3),LEVNDESC                                        
         MVC   2+L'LEVNDESC(L'LEVACCT,R3),LEVACCT                               
LE68     LA    R3,1(R3)                                                         
         GOTO1 SQUASHER,DMCB,(R3),69                                            
LE70     LA    R3,L'MYHEAD1-1(R3)                                               
         BXLE  R5,R6,LE50                                                       
         SPACE 1                                                                
LEALL    EQU   *                                                                
         CLI   QOPT5,C'Y'                                                       
         BE    LEA8A               SUMMARY ONLY                                 
         ZIC   R2,THISLEV          R2 GETS ACCUMULATOR ROW NO                   
         SPACE 1                                                                
         MVC   BUFKY,SPACES                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,BUFKY,(R2)                           
         TM    DMCB+8,X'80'                                                     
         BO    LEXIT                                                            
         L     R5,FULL                                                          
         SPACE 1                                                                
LEA1     MVC   TOTS,BUFKY                                                       
         MVC   DEPTOT,BUFBK                                                     
         MVC   TENAME,SPACES                                                    
         SPACE 1                                                                
         BAS   RE,PLINE                                                         
         MVC   P+1(3),BUFKY        WORK CODE                                    
         L     R4,=A(WRKTB)                                                     
         LA    R5,500                                                           
         LA    RF,BUFKY+1                                                       
         CLI   PROGPROF+3,C'Y'                                                  
         BNE   *+8                                                              
         LA    RF,BUFKY            USE FIRST 2 FOR NAME                         
         CLC   0(2,RF),0(R4)                                                    
         BE    *+16                                                             
         LA    R4,17(R4)                                                        
         BCT   R5,*-14                                                          
         B     LEA2                                                             
         MVC   TENAME,2(R4)                                                     
         SPACE 1                                                                
LEA2     MVC   P+5(15),TENAME                                                   
         GOTO1 SQUASHER,DMCB,P+1,20                                             
         L     R3,DMCB+4                                                        
         SH    R3,=H'2'                                                         
         MVI   PSECOND+1,C'-'                                                   
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   PSECOND+2(0),PSECOND+1                                           
         MVI   SPACING,2                                                        
         CLI   LINE,X'36'                                                       
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PLINE                                                         
         SPACE 1                                                                
LEA3     MVC   SVDEPT,BUFKY+3                                                   
         MVC   SVTOTS,BUFKY                                                     
         MVC   P+2(10),BUFKY+3                                                  
         LA    R5,P+11                                                          
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         LA    R5,2(R5)                                                         
         GOTO1 CHOPPER,DMCB,(36,BUFKY+13),(21,0(R5)),(C'P',2)                   
         LA    R5,BUFBK                                                         
         BAS   RE,PRNBK                                                         
         SPACE 1                                                                
LEA4     ZIC   R2,THISLEV                                                       
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,BUFKY,(R2)                            
         TM    DMCB+8,X'80'                                                     
         BZ    *+10                                                             
         MVC   BUFKY(3),=3X'FF'                                                 
         BAS   RE,DEPTBRK                                                       
         CLC   BUFKY(3),=3X'FF'                                                 
         BE    LEA7                                                             
         CLC   TOTS(3),BUFKY                                                    
         BNE   LEA6                                                             
         LA    R3,BUFBK                                                         
         LA    R4,TOTS+49                                                       
         LA    R6,DEPTOT                                                        
         ZIC   R5,BKCNT            NUMBER OF BUCKETS                            
LEA4A    ICM   R2,15,0(R3)                                                      
         ICM   R1,15,0(R4)                                                      
         AR    R1,R2                                                            
         STCM  R1,15,0(R4)                                                      
         ICM   R1,15,0(R6)                                                      
         AR    R1,R2                                                            
         STCM  R1,15,0(R6)                                                      
         LA    R3,4(R3)                                                         
         LA    R6,4(R6)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,LEA4A                                                         
         B     LEA3                                                             
         SPACE 1                                                                
LEA6     BAS   RE,PLINE                                                         
         MVC   P+1(5),=C'TOTAL'                                                 
         MVC   P+7(15),TENAME                                                   
         MVI   SPACING,2                                                        
         LA    R5,TOTS+49                                                       
         CLI   RCSUBPRG,2                                                       
         BE    *+8                                                              
         BAS   RE,PRNBK                                                         
         B     LEA1                                                             
         SPACE 1                                                                
LEA7     CP    DCNTR,=P'2'                                                      
         BL    LEA7AB                                                           
         MVC   SVTOTS(3),=3X'FF'                                                
         BAS   RE,DEPTBRK                                                       
LEA7AB   BAS   RE,PLINE                                                         
         MVC   P+1(5),=C'TOTAL'                                                 
         MVC   P+7(15),TENAME                                                   
         MVI   SPACING,2                                                        
         LA    R5,TOTS+49                                                       
         CLI   RCSUBPRG,2                                                       
         BE    *+8                                                              
         BAS   RE,PRNBK                                                         
         SPACE 2                                                                
LEA8     BAS   RE,PLINE                                                         
LEA8A    CP    CNTSUM,MAXCNT                                                    
         BNL   LEXIT               SUMMARY TOO BIG                              
         CLI   QOPT5,C'N'                                                       
         BE    LEXIT               NO SUMMARY                                   
         CLI   RCSUBPRG,2                                                       
         BE    LEXIT                                                            
         ZAP   CNTSUM,=P'0'                                                     
         MVC   P+1(L'SMHEAD),SMHEAD                                             
         GOTO1 UNDERLIN,DMCB,(20,P+1),(0,P+133)                                 
         MVI   SPACING,2                                                        
         CLI   LINE,X'36'                                                       
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PLINE                                                         
         BAS   RE,SUMHI                                                         
         OC    DMCB(4),DMCB                                                     
         BZ    LEXIT                                                            
         SPACE 1                                                                
         MVC   TOTS,BUFKY                                                       
         MVC   DEPTOT,BUFBK                                                     
         MVC   SVDEPT,BUFKY+3                                                   
         MVC   SVTOTS,BUFKY                                                     
         ZAP   DCNTR,=P'0'                                                      
LEA9     MVC   P+2(10),BUFKY+3                                                  
         LA    R5,P+11                                                          
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         LA    R5,2(R5)                                                         
         GOTO1 CHOPPER,DMCB,(36,BUFKY+13),(21,0(R5)),(C'P',2)                   
         LA    R5,BUFBK                                                         
         BAS   RE,PRNBK                                                         
         SPACE 1                                                                
         BAS   RE,SUMSEQ                                                        
         MVC   SVDMCB,DMCB                                                      
         OC    DMCB(4),DMCB                                                     
         BNZ   *+8                                                              
         MVI   BUFKY+3,X'FF'                                                    
         BAS   RE,DEPTBRK                                                       
         MVC   DMCB(4),SVDMCB                                                   
         OC    DMCB(4),DMCB                                                     
         BZ    LEA9A                                                            
         LA    R5,BUFBK                                                         
         LA    R6,TOTS+49                                                       
         LA    R7,DEPTOT                                                        
         ZIC   R3,BKCNT            NUMBER OF BUCKETS                            
LEA9AA   ICM   R2,15,0(R5)                                                      
         ICM   R1,15,0(R6)                                                      
         AR    R1,R2                                                            
         STCM  R1,15,0(R6)                                                      
         ICM   R1,15,0(R7)                                                      
         AR    R1,R2                                                            
         STCM  R1,15,0(R7)                                                      
         LA    R5,4(R5)                                                         
         LA    R7,4(R7)                                                         
         LA    R6,4(R6)                                                         
         BCT   R3,LEA9AA                                                        
         B     LEA9                                                             
         SPACE 1                                                                
LEA9A    CP    DCNTR,=P'2'                                                      
         BL    LEA9AB                                                           
         MVC   SVTOTS(3),=3X'FF'                                                
         BAS   RE,DEPTBRK                                                       
LEA9AB   BAS   RE,PLINE                                                         
         MVC   P+1(5),=C'TOTAL'                                                 
         GOTO1 CHOPPER,DMCB,(36,ACCT),(21,P+7),(C'P',2)                         
         LA    R5,TOTS+49                                                       
         BAS   RE,PRNBK                                                         
         ZAP   DCNTR,=P'0'                                                      
         B     LEXIT                                                            
         SPACE 1                                                                
LEXIT    ZIC   R2,THISLEV                                                       
LEXIT1   GOTO1 BUFFALO,DMCB,=C'CLEAR',ABUFF,(X'80',(R2))                        
         B     TEXIT                                                            
         SPACE 2                                                                
DEPTBRK  NTR1                                                                   
         CLC   QUNIT(2),=C'29'                                                  
         BNE   TEXIT                                                            
         CLI   PROGPROF,C'Y'                                                    
         BNE   TEXIT                                                            
         AP    DCNTR,=P'1'                                                      
         CLC   SVTOTS(3),BUFKY                                                  
         BNE   DEPTPRN                                                          
         ZIC   R2,DEPTLOC                                                       
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   SVDEPT(0),BUFKY+3                                                
         BE    TEXIT                                                            
DEPTPRN  CP    DCNTR,=P'1'                                                      
         BE    DEPTCLR                                                          
         MVC   P+3(15),=C'TOTAL DEPT.    '                                      
         MVI   SPACING,2                                                        
         ZIC   R2,DEPTLOC                                                       
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P+15(0),SVDEPT                                                   
         LA    R5,DEPTOT                                                        
         BAS   RE,PRNBK                                                         
DEPTCLR  CP    DCNTR,=P'1'                                                      
         BNE   DCLR1                                                            
         BAS   RE,PLINE                                                         
DCLR1    XC    DEPTOT,DEPTOT                                                    
         MVC   SVDEPT,BUFKY+3                                                   
         MVC   SVTOTS,BUFKY                                                     
         ZAP   SVDCNTR,DCNTR                                                    
         ZAP   DCNTR,=P'0'                                                      
         B     TEXIT                                                            
         EJECT                                                                  
PRNBK    NTR1                                                                   
         LA    R3,P+30        R5 POINTS TO ACCUMULATORS TO BE PRINTED           
         L     R4,CNT                                                           
         ST    R5,FULL                                                          
         MVI   PRINTSW,C'N'                                                     
*&&UK                                                                           
         CLI   RCSUBPRG,2          DOWN-LOADING                                 
         BNE   *+16                                                             
         LA    R8,DLBUFF                                                        
         BAS   RE,DLEDIT                                                        
         B     TEXIT                                                            
*&&                                                                             
         BAS   R7,EDIT                                                          
         CLI   QOPT2,C'Y'          LAST YEAR                                    
         BNE   PRNBK2                                                           
         LA    R3,PSECOND                                                       
         CLC   PSECOND,SPACES                                                   
         BE    *+8                                                              
         LA    R3,132(R3)                                                       
         MVC   3(12,R3),=C'*PRIOR YEAR*'                                        
         LA    R3,30(R3)                                                        
         L     R4,CNT                                                           
         L     R5,FULL                                                          
         L     R2,CNT              NUMBER OF BUCKETS                            
         SLL   R2,2                X 4                                          
         AR    R5,R2               R5 TO PRIOR YEAR                             
         BAS   R7,EDIT                                                          
PRNBK2   CLI   PRINTSW,C'N'                                                     
         BE    *+8                                                              
         BAS   RE,PLINE                                                         
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         B     TEXIT                                                            
         SPACE 1                                                                
EDIT     XC    TOTAL,TOTAL                                                      
EDIT1    OC    0(4,R5),0(R5)                                                    
         BZ    EDIT2                                                            
         MVI   PRINTSW,C'Y'                                                     
         EDIT  (B4,0(R5)),(11,0(R3)),COMMAS=YES,MINUS=YES                       
         ICM   R1,15,0(R5)                                                      
         L     R2,TOTAL                                                         
         AR    R2,R1                                                            
         ST    R2,TOTAL                                                         
EDIT2    LA    R3,11(R3)                                                        
         LA    R5,4(R5)                                                         
         BCT   R4,EDIT1                                                         
         CLI   CNT+3,1                                                          
         BER   R7                  NO TOTAL IF ONE COLUMN                       
         EDIT  (B4,TOTAL),(11,0(R3)),COMMAS=YES,MINUS=YES                       
         BR    R7                                                               
         EJECT                                                                  
*&&UK                                                                           
DLEDIT   NTR1                                                                   
         CLI   DLFRST,C'Y'                                                      
         BNE   DLED02                                                           
         BAS   RE,DLHEDS                                                        
         MVI   DLFRST,C'N'                                                      
DLED02   OC    0(4,R5),0(R5)                                                    
         BZ    *+8                                                              
         MVI   PRINTSW,C'Y'                                                     
         LA    R5,4(R5)                                                         
         BCT   R4,DLED02                                                        
         CLI   PRINTSW,C'Y'        IF ALL COLS ZERO                             
         BNE   TEXIT               DO NOT DOWNLOAD                              
         L     R4,CNT              RESET COUNT                                  
         L     R5,FULL             AND A(ACCUMS)                                
         LA    R8,DLBUFF                                                        
         LA    R3,MYHEAD1                                                       
         LA    R2,4                                                             
DLED05   CLC   0(L'MYHEAD1,R3),SPACES                                           
         BE    DLED08                                                           
         MVC   DLCBFLD(12),1(R3)   PUT IN THE CODE                              
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,(R8)                                                    
         MVC   DLCBFLD,14(R3)      THEN THE NAME                                
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,(R8)                                                    
DLED08   LA    R3,L'MYHEAD1(R3)                                                 
         BCT   R2,DLED05                                                        
         MVC   DLCBFLD(3),BUFKY        TE CODE                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         BASR  RE,RF                                                            
         MVC   DLCBFLD(15),TENAME      AND NAME                                 
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         BASR  RE,RF                                                            
         MVC   DLCBFLD(10),BUFKY+3     CODE                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         BASR  RE,RF                                                            
         MVC   DLCBFLD(36),BUFKY+12    NAME                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         BASR  RE,RF                                                            
         XC    TOTAL,TOTAL                                                      
DLED10   MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBNUM                                                  
         OC    0(4,R5),0(R5)                                                    
         EDIT  (B4,0(R5)),(11,DLCBFLD),FLOAT=-,ALIGN=LEFT                       
         GOTO1 DOWNLOAD,(R8)                                                    
         ICM   R1,15,0(R5)                                                      
         L     R2,TOTAL                                                         
         AR    R2,R1                                                            
         ST    R2,TOTAL                                                         
         LA    R5,4(R5)                                                         
         BCT   R4,DLED10                                                        
         CLI   CNT+3,1             IF ONLY ONE COL THEN NO TOTAL                
         BE    DLED15                                                           
         EDIT  (B4,TOTAL),(11,DLCBFLD),FLOAT=-,ALIGN=LEFT                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBNUM                                                  
         GOTO1 DOWNLOAD,(R8)                                                    
DLED15   MVI   DLCBACT,DLCBEOL                                                  
         GOTO1 DOWNLOAD,(R8)                                                    
         B     TEXIT                                                            
*&&                                                                             
         EJECT                                                                  
PLINE    NTR1                                                                   
         CLI   RCSUBPRG,2                                                       
         BE    TEXIT                                                            
         LA    R1,4                                                             
         LA    R2,HEAD4                                                         
         LA    R3,MYHEAD1                                                       
PLI02    MVC   0(L'MYHEAD1,R2),0(R3)                                            
         LA    R2,L'HEAD4(R2)                                                   
         LA    R3,L'MYHEAD1(R3)                                                 
         BCT   R1,PLI02                                                         
         SPACE 1                                                                
         MVC   HEAD4+80(16),PERIOD                                              
         CLI   QOPT1,C'B'                                                       
         BNE   *+10                                                             
         MVC   HEAD6+80(23),=C'BILLABLE AND UNBILLABLE'                         
         CLI   QOPT1,C'C'                                                       
         BNE   *+10                                                             
         MVC   HEAD6+80(8),=C'BILLABLE'                                         
         CLI   QOPT1,C' '                                                       
         BNE   *+10                                                             
         MVC   HEAD6+80(10),=C'UNBILLABLE'                                      
*&&US                                                                           
         CP    MINIMP,=P'0'                                                     
         BE    PLI04                                                            
         MVC   HEAD7+80(14),=C'MINIMUM AMOUNT'                                  
         LA    R3,HEAD7+95                                                      
         EDIT  (P8,MINIMP),(6,0(R3)),COMMAS=YES,ALIGN=LEFT                      
*&&                                                                             
PLI04    MVC   MID1+30(77),PLIN                                                 
         MVC   MID2+30(77),PLIN+77  SECOND DATE LINE                            
         GOTO1 ACREPORT                                                         
         B     TEXIT                                                            
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 3                                                                
CLRBUF   MVC   BUFKY,SPACES                                                     
         XC    BUFBK,BUFBK                                                      
         BR    R7                                                               
         EJECT                                                                  
BINADD   NTR1                                                                   
         A     R3,RELO                                                          
         L     R5,4(R3)            NUMBER IN TABLE                              
         L     R6,8(R3)            MAX                                          
         LA    R7,12(R3)           TABLE                                        
         GOTO1 BINSRCH,DMCB,(1,BUFKY),(R7),(R5),97,(0,13),(R6)                  
         SPACE 1                                                                
         MVC   4(4,R3),DMCB+8      UPDATE NUMBER                                
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         SPACE 1                                                                
         CLI   DMCB,1              NOT FOUND ADDED                              
         BE    TEXIT                                                            
         L     R5,DMCB             RECORD FOUND                                 
         LA    R5,49(R5)           ACCUMS                                       
         LA    R7,BUFKY+49                                                      
         ZIC   R6,BKCNT                                                         
BINADD1  ICM   R1,15,0(R7)                                                      
         ICM   R2,15,0(R5)                                                      
         AR    R2,R1                                                            
         STCM  R2,15,0(R5)                                                      
         LA    R5,4(R5)                                                         
         LA    R7,4(R7)                                                         
         BCT   R6,BINADD1                                                       
         B     TEXIT                                                            
         SPACE 3                                                                
BINGET   NTR1                                                                   
         XC    DMCB(4),DMCB                                                     
         A     R3,RELO                                                          
         L     R4,0(R3)                                                         
         L     R5,4(R3)                                                         
         CR    R4,R5                                                            
         BNE   BINGT1                                                           
         XC    0(8,R3),0(R3)                                                    
         B     TEXIT                                                            
         SPACE 1                                                                
BINGT1   LA    R4,1(R4)                                                         
         ST    R4,0(R3)                                                         
         LA    R3,12(R3)                                                        
         CH    R4,=H'1'                                                         
         BE    BINGT2                                                           
         BCTR  R4,0                                                             
         MH    R4,=H'97'                                                        
         AR    R3,R4                                                            
BINGT2   MVC   BUFKY(97),0(R3)                                                  
         MVI   DMCB+3,X'FF'                                                     
         B     TEXIT                                                            
*                                                                               
*&&UK                                                                           
DLPRINT  NTR1                                                                   
         MVC   P,DLPLINE                                                        
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   SPACING,1                                                        
         GOTO1 ACREPORT                                                         
         MVI   LINE,1                                                           
         MVC   DLPLINE,SPACES                                                   
         B     TEXIT                                                            
*&&                                                                             
         EJECT                                                                  
SUMHI    NTR1                                                                   
         ZIC   R2,THISLEV                                                       
         MVC   BUFKY,SPACES                                                     
         MVC   BUFKY(3),=3X'FF'                                                 
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,BUFKY,(R2)                           
         TM    DMCB+8,X'80'                                                     
         BO    SUMSXT                                                           
         CLC   BUFKY(3),=3X'FF'                                                 
         BE    TEXIT                                                            
         B     SUMSXT                                                           
         SPACE 1                                                                
SUMSEQ   NTR1                                                                   
         ZIC   R2,THISLEV                                                       
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,BUFKY,(R2)                            
         TM    DMCB+8,X'80'                                                     
         BO    SUMSXT                                                           
         CLC   BUFKY(3),=3X'FF'                                                 
         BE    TEXIT                                                            
SUMSXT   XC    DMCB(4),DMCB                                                     
         B     TEXIT                                                            
         EJECT                                                                  
*              SET BUFFALOC                                                     
SETBUFF  NTR1                                                                   
         L     R3,CNT                                                           
         CLI   QOPT2,C'Y'          PRIOR YEAR                                   
         BNE   *+8                                                              
         SLL   R3,1                                                             
         STC   R3,BKCNT                                                         
         USING BUFFALOD,R4                                                      
         L     R4,ABUFF                                                         
         XC    BUFFDOPT(BUFFFLIP-BUFFDOPT),BUFFDOPT                             
         MVI   BUFFFLIP,C'A'                                                    
         XC    BUFFCB(BUFFDADS-BUFFCB),BUFFCB                                   
         ZIC   R3,HILEV                                                         
         ST    R3,BUFFROWS                                                      
         ZIC   R2,BKCNT                                                         
         ST    R2,BUFFCOLS                                                      
         MR    R2,R2               COLS X ROWS                                  
         MH    R3,=H'4'            X 4                                          
         ST    R3,BUFFLDTA         DATA LENGTH                                  
         AH    R3,=H'49'           KEY AND COMMENT                              
         ST    R3,BUFFLALL         RECORD LENGTH                                
         LR    R5,R3                                                            
         SR    R2,R2                                                            
         L     R3,=F'96400'        MAX BUFFER SIZE                              
         DR    R2,R5               DIVIDE BY RECORD LENGTH                      
         ST    R3,BUFFCRMX         MAX IN CORE                                  
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         B     TEXIT                                                            
         EJECT                                                                  
*&&UK                                                                           
*        SET UP HEADLINE FOR DOWN-LOADED FORMAT                                 
DLHEDS   NTR1                                                                   
         LA    R8,DLBUFF                                                        
         LA    R3,DLHEADS          LEVEL DESCRIPTIONS                           
         LA    R2,4                                                             
         LA    R0,2                                                             
DLHED10  CLC   0(L'LEVNDESC,R3),SPACES                                          
         BE    DLHED20                                                          
         MVC   DLCBFLD(L'LEVNDESC),0(R3)                                        
         MVC   DLCBFLD+L'LEVNDESC+1(4),=C'CODE'                                 
         CH    R0,=H'1'                                                         
         BH    *+10                                                             
         MVC   DLCBFLD+L'LEVNDESC+1(4),=C'NAME'                                 
         GOTO1 SQUASHER,DMCB,DLCBFLD,20                                         
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 DOWNLOAD,(R8)                                                    
         BCT   R0,DLHED10                                                       
DLHED20  LA    R3,L'LEVNDESC(R3)                                                
         LA    R0,2                                                             
         BCT   R2,DLHED10                                                       
         LA    R0,2                                                             
DLHED30  MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLD(17),=C'EXPENSE TYPE CODE'                                
         CH    R0,=H'1'                                                         
         BH    *+10                                                             
         MVC   DLCBFLD+13(4),=C'NAME'                                           
         BASR  RE,RF                                                            
         BCT   R0,DLHED30                                                       
         LA    R0,2                                                             
DLHED40  MVC   DLCBFLD(11),=C'CLIENT CODE'                                      
         CH    R0,=H'1'                                                         
         BH    *+10                                                             
         MVC   DLCBFLD+7(4),=C'NAME'                                            
         CLI   QLEDGER,C'P'                                                     
         BE    DLHED50                                                          
         MVC   DLCBFLD(11),=C'STAFF CODE '                                      
         CH    R0,=H'1'                                                         
         BH    *+10                                                             
         MVC   DLCBFLD+6(4),=C'NAME'                                            
DLHED50  MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         BASR  RE,RF                                                            
         BCT   R0,DLHED40                                                       
*                                                                               
         LA    R3,PLIN             MONTH HEADINGS                               
         LA    R2,7                                                             
TE36     CLC   0(11,R3),SPACES                                                  
         BE    TE37                                                             
         MVC   DLCBFLD(11),0(R3)                                                
         GOTO1 SQUASHER,DMCB,DLCBFLD,11                                         
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 DOWNLOAD,(R8)                                                    
TE37     LA    R3,11(R3)                                                        
         BCT   R2,TE36                                                          
         MVI   DLCBACT,DLCBEOL     END OF HEADLINE                              
         BASR  RE,RF                                                            
         B     TEXIT                                                            
*&&                                                                             
         EJECT                                                                  
*              TABLE TO BUILD BUCKET DATES                                      
*              BYTE 1              NUMBER OF MONTHS REQUESTED                   
*              BYTE 2              LENGTH OF ELEMENT                            
*              BYTE 3              OPTION FOR QOPT3                             
*              BYTE 4-N            NUMBER OF MONTHS IN EACH BUCKET              
*                                                                               
MONTB    DC    X'FF02'             SO I CAN START WITH NEXTEL                   
A1       DC    X'04',AL1(A2-A1),C'A',4X'01'                                     
A2       DC    X'05',AL1(A3-A2),C'A',5X'01'                                     
A3       DC    X'06',AL1(A4-A3),C'A',6X'01'                                     
A4       EQU   *                                                                
M1       DC    X'01',AL1(M2-M1),C'Z',X'01'                                      
M2       DC    X'02',AL1(M3-M2),C'Z',2X'01'                                     
M3       DC    X'03',AL1(M4-M3),C'Z',3X'01'                                     
M4       DC    X'04',AL1(M5-M4),C'Z',X'03',X'01'                                
M5       DC    X'05',AL1(M6-M5),C'Z',X'03',2X'01'                               
M6       DC    X'06',AL1(M7-M6),C'Z',X'03',3X'01'                               
M7       DC    X'07',AL1(M8-M7),C'Z',2X'03',X'01'                               
M8       DC    X'08',AL1(M9-M8),C'Z',2X'03',2X'01'                              
M9       DC    X'09',AL1(MA-M9),C'Z',2X'03',3X'01'                              
MA       DC    X'0A',AL1(MB-MA),C'Z',3X'03',X'01'                               
MB       DC    X'0B',AL1(MC-MB),C'Z',3X'03',2X'01'                              
MC       DC    X'0C',AL1(MD-MC),C'Z',3X'03',3X'01'                              
MD       DC    X'0000'             END OF TABLE                                 
         SPACE 1                                                                
*              TABLES FOR LEVEL HANDLING & THEIR DSECTS                         
         SPACE 1                                                                
RQLEVTAB DC    X'FF',X'02'                                                      
RQL1     DC    X'11',AL1(RQL2-RQL1),C'Z',X'01',C'AYY11'                         
RQL2     DC    X'21',AL1(RQL3-RQL2),C'Z',X'02',C'AYY22BNN11'                    
RQL3     DC    X'22',AL1(RQL4-RQL3),C'Z',X'02',C'AYN22BYY11'                    
RQL4     DC    X'31',AL1(RQL5-RQL4),C'Z',X'03',C'AYY33BNN22CNN11'               
RQL5     DC    X'32',AL1(RQL6-RQL5),C'Z',X'03',C'AYN33BYY22CNN11'               
RQL6     DC    X'33',AL1(RQL7-RQL6),C'Z',X'03',C'AYN33BYN22CYY11'               
RQL7     DC    X'41',AL1(RQL8-RQL7),C'Z',X'04',C'AYY44BNN33CNN22DNN11'          
RQL8     DC    X'42',AL1(RQL9-RQL8),C'Z',X'04',C'AYN44BYY33CNN22DNN11'          
RQL9     DC    X'43',AL1(RQLA-RQL9),C'Z',X'04',C'AYN44BYN33CYY22DNN11'          
RQLA     DC    X'44',AL1(RQLB-RQLA),C'Z',X'04',C'AYN44BYN33CYN22DYY11'          
RQLB     DC    X'0000'                                                          
         EJECT                                                                  
LEVNTAB  DS    0CL66               TABLE OF LEVEL NAMES & DESCRIPTIONS          
         DC    C'A'                                                             
         DS    XL1,CL15,CL49                                                    
         DC    C'B'                                                             
         DS    XL1,CL15,CL49                                                    
         DC    C'C'                                                             
         DS    XL1,CL15,CL49                                                    
         DC    C'D'                                                             
         DS    XL1,CL15,CL49                                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
WRKTB    DS    500CL17                                                          
*                                                                               
REQSUM   DS    0F                                                               
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'500'                                                           
         DS    500CL97                                                          
         SPACE 2                                                                
MYIO     DS    0F                                                               
         DS    1008C                                                            
         EJECT                                                                  
AC86D    DSECT                                                                  
RELO     DS    F                                                                
SQUASHER DS    V                                                                
UNDERLIN DS    V                                                                
DOWNLOAD DS    V                                                                
PDATES   DS    14CL4          14 SETS START - END DATES YYMM                    
PLIN     DS    14CL11         HEADLINES FOR COLUMNS                             
BUFKY    DS    CL49                3 TE, 10 SBACNUM, 36 SBACNAM                 
BUFBK    DS    CL48           BUCKETS 4 X 12                                    
         SPACE 1                                                                
ABUFF    DS    A              ADDRESS OF BUFFALO                                
ARQLEV   DS    A                                                                
ELCODE   DS    CL1            ELEMENT CODE                                      
CNT      DS    F              NUMBER OF MONTHS REQUESTED FOR 1 YEAR             
BKCNT    DS    CL1            NUMBER OF BUCKETS USED                            
         SPACE 1                                                                
MYHEAD1  DS    CL70                                                             
MYHEAD2  DS    CL70                                                             
MYHEAD3  DS    CL70                                                             
MYHEAD4  DS    CL70                                                             
SMHEAD   DS    CL30                                                             
ACCT     DS    CL36                                                             
HILEV    DS    CL1                                                              
THISLEV  DS    CL1                                                              
AMT      DS    PL8            WORK                                              
TOTAL    DS    F                                                                
SBACNUM  DS    CL10                                                             
SBACNAM  DS    CL36                                                             
PRINTSW  DS    CL1                                                              
TOTS     DS    CL97                SAME AS BUFFALO RECORD                       
SV3      DS    CL3                                                              
         SPACE 1                                                                
TE       DS    CL3                                                              
TENAME   DS    CL15                                                             
PSTRT    DS    CL2                                                              
PEND     DS    CL2                                                              
WANT     DS    CL1                                                              
GROUP    DS    CL1                                                              
SVKEY    DS    CL32                                                             
DEPTLOC  DS    CL1                                                              
CNTSUM   DS    PL3                                                              
MAXCNT   DS    PL3                                                              
ACCTACT  DS    CL1                                                              
LEVAACT  DS    CL1                                                              
LEVBACT  DS    CL1                                                              
LEVCACT  DS    CL1                                                              
MINIMP   DS    D                                                                
MINIMB   DS    F                                                                
         SPACE 1                                                                
PARM     DS    10F                                                              
         SPACE 1                                                                
PERIOD   DS    CL16                                                             
         SPACE 2                                                                
SVDEPT   DS    CL3                                                              
SVTOTS   DS    CL3                                                              
DEPTOT   DS    CL48                                                             
PERSTOT  DS    CL4                                                              
DCNTR    DS    PL4                                                              
SVDCNTR  DS    PL4                                                              
SVDMCB   DS    CL4                                                              
DLHEADS  DS    CL60                LEVEL DESCRIPTIONS                           
DLBUFF   DS    CL80                BUFFER FOR DOWNLOADING                       
DLPLINE  DS    CL132               PRINT LINE FOR DOWNLOADING                   
DLFRST   DS    C                   HEADLINE PRINT FLAG                          
         EJECT                                                                  
RQLEVD DSECT                                                                    
RQTYPE   DS    CL1                 E.G. '31'= 3 LEVELS,1 REQUESTED              
RQELEN   DS    CL1                 LENGTH OF THIS ELEMENT                       
RQOPT    DS    CL1                 POSSIBLE OPTION FILTER                       
RQHILEV  DS    CL1                 HILEV FOR THIS REQUEST                       
RQLEVEL  DS    0CL5                STRING OF LEVELD-TYPE ELEMENTS               
         SPACE 2                                                                
LEVELD   DSECT                                                                  
LEVLET   DS    CL1                 LETTER FOR THIS LEVEL                        
LEVWANT  DS    CL1                 WANTED=Y                                     
LASTLEV  DS    CL1                 LAST LEVEL WANTED=Y                          
LEVNO    DS    CL1                 NO OF THIS LEVEL                             
LEVBUF   DS    CL1                 BUFFALO LEVEL                                
         SPACE 2                                                                
LEVNAMD  DSECT                                                                  
LEVNLET  DS    CL1                 LEVEL LETTER                                 
LEVNCONT DS    XL1                 NO OF ACCOUNTS SO FAR THIS LEVEL             
LEVNDESC DS    CL15                DESCRIPTION OF THIS LEVEL                    
LEVACCT  DS    CL49                NAME & NO OF THIS LEVEL ( NO B NAME)         
         EJECT                                                                  
         PRINT GEN                                                              
*&&UK                                                                           
         BUFF  LINES=400,ROWS=4,COLUMNS=12,FLAVOR=BINARY,              X        
               KEYLIST=(13,A),COMMENT=36                                        
*&&                                                                             
*&&US                                                                           
         BUFF  LINES=001,ROWS=4,COLUMNS=12,FLAVOR=BINARY,              X        
               KEYLIST=(13,A),COMMENT=36                                        
*&&                                                                             
         SPACE 2                                                                
       ++INCLUDE DDBUFFALOD                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACREP8602 05/01/02'                                      
         END                                                                    
