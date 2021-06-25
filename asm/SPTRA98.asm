*          DATA SET SPTRA98    AT LEVEL 050 AS OF 06/11/15                      
*PHASE T21698A                                                                  
         TITLE 'T21698 - CMML INST AUTO-REQUEST GENERATOR'                      
***********************************************************************         
*                                                                     *         
*  LEV 32-36 MAR02/87 ADD EXPAND COPY CODE TO EST FOR PATTERN RECAP   *         
*  LEV 37    OCT01/87 DON'T PUT E(STIMATE) INDICATOR INTO PATTERN     *         
*                     REQUESTOR FIELD                                 *         
*  LEV 38-39 OCT06/87 GEN T/A FOR PAT RECAPS AND SHIPPING ORDERS      *         
*  LEV 40    NOV24/87 CHANGE FOR NEW SHIP GEN SCREEN                  *         
*  LEV 41-42 DEC03/87 CHANGE FOR NEW PATTERN RECAP INACTIVE FOR DFS   *         
*  LEV 43    JUL06/89 ADD SHIP REQ PRD2                               *         
*  LEV 44    OCT04/89 FIX SHIP REQ BUG                                *         
*  LEV 45    NOV13/89 FIX DDMASTR BUG                                 *         
*  LEV 46 SMUR AUG14/97 CHANGE SHIP REQ DUE TO NEW FAX FIELD          *         
*  LEV 47 BGRI MAR11/02 ADD SPOT SEED REQUEST                         *         
*  LEV 48 BGRI MAR18/02 FIX SPOT SEED REQUEST                         *         
*                                                                     *         
***********************************************************************         
T21698   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21698                                                         
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         B     OPEN                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
OPEN     DS    0H                                                               
         LA    R2,TR04FIL                                                       
         OPEN  ((R2),INPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   QMED,0                                                           
         XC    QCLT,QCLT                                                        
*                                                                               
         LA    RE,ENDFILE          RELOCATE EOD ADDRESS                         
         STCM  RE,7,TR04FIL+33                                                  
         SPACE 1                                                                
* ALWAYS NEED TO OPEN AND CLOSE REQUEST FILES *                                 
         SPACE 1                                                                
         LA    R2,PTNQFIL                                                       
         OPEN  ((R2),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,SHPQFIL                                                       
         OPEN  ((R2),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,INSQFIL                                                       
         OPEN  ((R2),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,SPSDFIL                                                       
         OPEN  ((R2),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         LA    R7,ELEM                                                          
         USING XRECD,R7                                                         
*                                                                               
         B     GETFILE                                                          
         SPACE 2                                                                
GETFILE  LA    R2,TR04FIL                                                       
         GET   (R2),(R7)                                                        
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',(R7)                                         
*                                                                               
         B     GETFILE                                                          
         SPACE 2                                                                
ENDFILE  DS    0H                                                               
         LA    R2,TR04FIL                                                       
         CLOSE ((2),)                                                           
         FREEPOOL (R2)                                                          
*                                                                               
         LA    R7,ELEM             POINT TO DUMMY AREA                          
         XC    ELEM,ELEM           CLEAR SO NO MATCH                            
         EJECT                                                                  
         USING XRECD,R7                                                         
*                                                                               
GETSORT  MVC   ELEM(L'XREC),0(R7)          SAVE PREVIOUS RECORD                 
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R7,15,4(R1)         TEST EOF                                     
         BZ    ENDSORT                                                          
*                                                                               
         CLC   ELEM(L'XREC),0(R7)  TEST DUPLICATE REQUEST                       
         BE    GETSORT             YES - IGNORE                                 
         SPACE 1                                                                
         CLI   XTYPE,C'P'          TEST PATTERN RECAP REQ                       
         BE    PTN                                                              
         CLI   XTYPE,C'S'          TEST SHIPPING ORDER REQ                      
         BE    SHP                                                              
         CLI   XTYPE,C'I'          TEST INSTRUCTION RECAP REQ                   
         BE    INS                                                              
         CLI   XTYPE,C'D'          TEST SPOT SEED REQ                           
         BE    SED                                                              
         DC    H'0'                                                             
         SPACE 2                                                                
ENDSORT  DS    0H                                                               
         MVC   PREQ(2),=C'/*'                                                   
         MVC   PREQ+2(78),SPACES                                                
         LA    R0,PREQ                                                          
         LA    R1,PTNQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         LA    R2,PTNQFIL                                                       
         CLOSE ((2))                                                            
         FREEPOOL (R2)                                                          
*                                                                               
         LA    R0,PREQ                                                          
         LA    R1,SHPQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         LA    R2,SHPQFIL                                                       
         CLOSE ((2))                                                            
         FREEPOOL (R2)                                                          
*                                                                               
         LA    R0,PREQ                                                          
         LA    R1,INSQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         LA    R2,INSQFIL                                                       
         CLOSE ((2))                                                            
         FREEPOOL (R2)                                                          
*                                                                               
         LA    R0,PREQ                                                          
         LA    R1,SPSDFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         LA    R2,SPSDFIL                                                       
         CLOSE ((2))                                                            
         FREEPOOL (R2)                                                          
*                                                                               
ENDSORT4 B     EXIT                                                             
         EJECT                                                                  
* PATTERN RECAP REQUESTS *                                                      
         SPACE                                                                  
PTN      AP    REQNUM,=P'1'                                                     
         OI    REQNUM+3,X'0F'                                                   
*                                                                               
         MVC   PREQ,SPACES                                                      
         MVC   PREQ(2),=C'TC'                                                   
         MVC   PREQ+2(2),AGENCY                                                 
         UNPK  PREQ+5(6),REQNUM                                                 
*                                                                               
         LA    R4,PREQ+12                                                       
         MVC   0(18,R4),=C'0203PAT 0305RECAP '                                  
         LA    R4,18(R4)                                                        
         LA    R5,PREQ1TAB                                                      
         BAS   RE,SETREQ                                                        
*                                                                               
         LA    R0,PREQ                                                          
         LA    R1,PTNQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         MVC   PREQ+12(68),SPACES                                               
*                                                                               
         LA    R4,PREQ+12                                                       
         LA    R5,PREQ2TAB                                                      
         BAS   RE,SETREQ                                                        
*                                                                               
         CLI   XSVPR011,C'E'       COPY CODE = EST                              
         BNE   PTN20                                                            
*                                                                               
         CLI   XCOPY,0             NO EST ANYHOW                                
         BE    PTN20                                                            
*                                                                               
         CLI   XCOPY,C' '          IF WAS EST 64, BUILD ALL                     
         BNE   PTN10                                                            
*                                                                               
         L     R1,NEXTCOL          PUT OUT COPY CODE AS EST                     
         MVC   0(4,R1),=C'1303'                                                 
         ST    R1,LASTCOL                                                       
*                                                                               
PTN10    L     R4,LASTCOL          PUT OUT COPY CODE AS EST                     
         ZIC   R0,XCOPY                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R4),DUB                                                      
         MVI   3(R4),C'3'                                                       
*                                                                               
PTN20    LA    R0,PREQ                                                          
         LA    R1,PTNQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         CLC   XREF,=C'INACTIVE'   THIS FOR DFS                                 
         BNE   PTN30                                                            
         LA    R4,PREQ+12                                                       
         B     PTN40                                                            
*                                                                               
PTN30    MVC   PREQ+12(68),SPACES                                               
         LA    R4,PREQ+12                                                       
         LA    R5,PREQ3TAB                                                      
         BAS   RE,SETREQ                                                        
*                                                                               
         L     R4,NEXTCOL                                                       
PTN40    MVC   0(4,R4),=C'1517'                                                 
         GOTO1 DATCON,DMCB,XFLTST,(5,4(R4))                                     
         MVI   12(R4),C'-'                                                      
         GOTO1 (RF),(R1),XFLTEND,(5,13(R4))                                     
*                                                                               
         MVC   22(8,R4),=C'1603T/A*'                                            
*                                                                               
         CLC   XREF,=C'INACTIVE'   THIS FOR DFS                                 
         BNE   PTN50                                                            
         MVC   22(17,R4),=C'1612T/A,INACTIVE*'                                  
*                                                                               
PTN50    LA    R0,PREQ                                                          
         LA    R1,PTNQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         B     GETSORT                                                          
*                                                                               
PREQ1TAB DC    AL1(5,7),AL4(XWHEN-XRECD)                                        
         DC    X'FF'                                                            
*                                                                               
PREQ2TAB DC    AL1(9,1),AL4(XMED-XRECD)                                         
         DC    AL1(10,3),AL4(XCLT-XRECD)                                        
         DC    AL1(11,7),AL4(XPRD-XRECD)                                        
         DC    AL1(12,7),AL4(XPRD2-XRECD)                                       
         DC    AL1(13,1),AL4(XCOPY-XRECD) IF ENTRY ADDED AFTER THIS,            
         DC    X'FF'                      MUST CHANGE CODE BEFORE PTN20         
*                                                                               
PREQ3TAB DC    AL1(14,5),AL4(XREF-XRECD)                                        
         DC    X'FF'                                                            
         DS    0H                                                               
         EJECT                                                                  
* SHIPPING ORDER REQUESTS *                                                     
         SPACE                                                                  
SHP      AP    REQNUM,=P'1'                                                     
         OI    REQNUM+3,X'0F'                                                   
*                                                                               
         MVC   SREQ,SPACES                                                      
         MVC   SREQ(2),=C'TO'                                                   
         MVC   SREQ+2(2),AGENCY                                                 
         UNPK  SREQ+5(6),REQNUM                                                 
*                                                                               
         LA    R4,SREQ+12                                                       
         MVC   0(17,R4),=C'0204SHIP 0303GEN '                                   
         LA    R4,17(R4)                                                        
         LA    R5,SREQ1TAB                                                      
         BAS   RE,SETREQ                                                        
*                                                                               
         LA    R0,SREQ                                                          
         LA    R1,SHPQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         MVC   SREQ+12(68),SPACES                                               
*                                                                               
         LA    R4,SREQ+12                                                       
         LA    R5,SREQ2TAB                                                      
         BAS   RE,SETREQ                                                        
*                                                                               
         L     R4,NEXTCOL                                                       
         CLI   XRERUN,C' '         TEST RERUN DATE PRESENT                      
         BNH   SHP10                                                            
         MVC   0(22,R4),=C'1518RERUN=JAN01/84,T/A'                              
         GOTO1 DATCON,DMCB,XRERUN,(5,10(R4))                                    
         LA    R4,23(,R4)          BUMP TO INCLUDE SPACE                        
         B     SHP14                                                            
*                                                                               
SHP10    MVC   0(7,R4),=C'1503T/A'                                              
         LA    R4,8(,R4)           BUMP TO INCLUDE SPACE                        
*                                                                               
SHP14    BCTR  R4,0                                                             
         MVI   0(R4),C'*'          SET EOD FLAG                                 
*                                                                               
         LA    R0,SREQ                                                          
         LA    R1,SHPQFIL                                                       
         PUT   (1),(0)                                                          
         B     GETSORT                                                          
         SPACE 2                                                                
SREQ1TAB DC    AL1(5,8),AL4(XWHEN-XRECD)                                        
         DC    X'FF'                                                            
*                                                                               
SREQ2TAB DC    AL1(9,1),AL4(XMED-XRECD)                                         
         DC    AL1(10,3),AL4(XCLT-XRECD)                                        
         DC    AL1(11,3),AL4(XPRD-XRECD)                                        
         DC    AL1(12,3),AL4(XPRD2-XRECD)                                       
         DC    AL1(13,12),AL4(XQUESTOR-XRECD)                                   
         DC    X'FF'                                                            
         DS    0H                                                               
         EJECT                                                                  
* INSTRUCTION RECAP REQUESTS *                                                  
         SPACE                                                                  
INS      AP    REQNUM,=P'1'                                                     
         OI    REQNUM+3,X'0F'                                                   
*                                                                               
INS4     MVC   IREQ,SPACES                                                      
         MVC   IREQ(2),AGENCY                                                   
         MVC   IREQ+2(2),=C'TC'                                                 
         UNPK  IREQ+5(6),REQNUM                                                 
*                                                                               
         LA    R4,IREQ+12                                                       
         MVC   0(18,R4),=C'0203INS 0305RECAP '                                  
         LA    R4,18(R4)                                                        
         LA    R5,IREQ1TAB                                                      
         BAS   RE,SETREQ                                                        
*                                                                               
         LA    R0,IREQ                                                          
         LA    R1,INSQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         MVC   IREQ+12(68),SPACES                                               
*                                                                               
         LA    R4,IREQ+12                                                       
         LA    R5,IREQ2TAB                                                      
         BAS   RE,SETREQ                                                        
*                                                                               
         LA    R0,IREQ                                                          
         LA    R1,INSQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         MVC   IREQ+12(68),SPACES                                               
         LA    R4,IREQ+12                                                       
*                                                                               
         MVC   0(4,R4),=C'1517'                                                 
         GOTO1 DATCON,DMCB,XFLTST,(5,4(R4))                                     
         MVI   12(R4),C'-'                                                      
         GOTO1 (RF),(R1),XFLTEND,(5,13(R4))                                     
*                                                                               
         MVI   21(R4),C'*'                                                      
*                                                                               
         LA    R0,IREQ                                                          
         LA    R1,INSQFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         B     GETSORT                                                          
*                                                                               
IREQ1TAB DC    AL1(5,8),AL4(XWHEN-XRECD)                                        
         DC    X'FF'                                                            
*                                                                               
IREQ2TAB DC    AL1(9,1),AL4(XMED-XRECD)                                         
         DC    AL1(10,3),AL4(XCLT-XRECD)                                        
         DC    AL1(11,3),AL4(XPRD-XRECD)                                        
         DC    AL1(12,6),AL4(XSTA-XRECD)                                        
         DC    X'FF'                                                            
         EJECT                                                                  
* SPOT SEED REQUESTS *                                                          
         SPACE                                                                  
SED      AP    REQNUM,=P'1'                                                     
         OI    REQNUM+3,X'0F'                                                   
*                                                                               
         MVC   DREQ,SPACES                                                      
         MVC   DREQ(2),AGENCY                                                   
         MVC   DREQ+2(2),=C'TS'                                                 
         UNPK  DREQ+5(6),REQNUM                                                 
*                                                                               
         LA    R4,DREQ+12                                                       
         MVC   0(18,R4),=C'0204SPOT 0304SEED '                                  
         LA    R4,18(R4)                                                        
         LA    R5,DREQ1TAB                                                      
         BAS   RE,SETREQ                                                        
*                                                                               
         LA    R0,DREQ                                                          
         LA    R1,SPSDFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
         MVC   DREQ+12(68),SPACES                                               
*                                                                               
         LA    R4,DREQ+12                                                       
         LA    R5,DREQ2TAB                                                      
         BAS   RE,SETREQ                                                        
*                                                                               
         L     R4,NEXTCOL          SET NEXT FIELD START ADDR                    
*                                                                               
         MVC   0(4,R4),=C'1107'                                                 
         MVC   4(3,R4),XPRD                                                     
         MVI   7(R4),C'-'                                                       
         MVC   8(3,R4),XPRD+3                                                   
         LA    R4,12(,R4)                                                       
*                                                                               
         CLI   XCOPY,0             IF COPY CODE ZERO, NO ESTIMATE               
         BE    SED20                                                            
*                                                                               
         MVC   0(4,R4),=C'1403'                                                 
*                                                                               
         ZIC   R0,XCOPY                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R4),DUB                                                      
         LA    R4,8(,R4)                                                        
*                                                                               
SED20    DS    0H                                                               
         MVC   0(4,R4),=C'1517'                                                 
         GOTO1 DATCON,DMCB,XFLTST,(5,4(R4))                                     
         MVI   12(R4),C'-'                                                      
         GOTO1 (RF),(R1),XFLTEND,(5,13(R4))                                     
         LA    R4,21(,R4)                                                       
*                                                                               
         CLI   XTEST,C'Y'          THIS A TEST RUN                              
         BNE   SED30                NO                                          
*                                                                               
         MVC   1(8,R4),=C'1604TEST'                                             
         LA    R4,9(,R4)                                                        
*                                                                               
SED30    DS    0H                                                               
         MVI   0(R4),C'*'                                                       
*                                                                               
         LA    R0,DREQ                                                          
         LA    R1,SPSDFIL                                                       
         PUT   (1),(0)                                                          
*                                                                               
*                                                                               
         B     GETSORT                                                          
*                                                                               
DREQ1TAB DC    AL1(5,7),AL4(XWHEN-XRECD)                                        
         DC    X'FF'                                                            
*                                                                               
DREQ2TAB DC    AL1(9,1),AL4(XMED-XRECD)                                         
         DC    AL1(10,3),AL4(XCLT-XRECD)                                        
         DC    X'FF'                                                            
         EJECT                                                                  
*******************************************                                     
* SUBROUTINE TO MOVE DATA TO REQUEST CARD *                                     
* R4 POINTS TO NEXT OUTPUT ADDRESS        *                                     
* R5 POINTS TO DATA TABLE                 *                                     
*******************************************                                     
         SPACE 1                                                                
SETREQ   NTR1                                                                   
*                                                                               
SETREQ0  ZIC   R0,1(R5)            GET MAXIMUM DATA LENGTH                      
         ICM   R1,15,2(R5)         GET DATA DSPL IN XREC                        
         LA    R1,XREC(R1)                                                      
         STM   R0,R1,DUB           SAVE MAX LEN/ADDRESS                         
*                                                                               
         AR    R1,R0               POINT PAST END                               
*                                                                               
SETREQ2  BCTR  R1,0                                                             
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         CLI   0(R1),C' '                                                       
         BNE   SETREQ4                                                          
         BCT   R0,SETREQ2                                                       
         B     SETREQ10                                                         
*                                                                               
SETREQ4  LR    RF,R0                                                            
         BCTR  RF,0                                                             
         L     R1,DUB+4            GET DATA ADDRESS                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R4),0(R1) *EXECUTED*                                         
         SPACE                                                                  
* SET ACTUAL DATA LENGTH                                                        
         SPACE                                                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(2,R4),DUB                                                      
         SPACE                                                                  
* SET FIELD NUMBER                                                              
         SPACE                                                                  
         ZIC   R1,0(R5)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
*                                                                               
         ST    R4,LASTCOL          SET LAST FIELD START ADDR                    
         AR    R4,R0               ADD LENGTH OF EXECUTED MOVE                  
         LA    R4,5(R4)            BUMP FOR FIELD NUM/FIELD LEN/SPACE           
*                                                                               
SETREQ10 LA    R5,6(R5)            NEXT TABLE ENTRY                             
         CLI   0(R5),X'FF'                                                      
         BNE   SETREQ0                                                          
         ST    R4,NEXTCOL          SET NEXT FIELD START ADDR                    
         B     EXIT                                                             
         EJECT                                                                  
*********************************************                                   
* SUBROUTINE TO GENERATE CONTROL STATEMENTS *                                   
* NOT PRESENTLY GENERATED BY END OF DAY     *                                   
* THESE ARE READ IN ADDITION TO STATEMENTS  *                                   
* ON NORMAL SYSIN FILE                      *                                   
*********************************************                                   
         SPACE                                                                  
SETCNTL  NTR1                                                                   
         L     R3,ATWA                                                          
         L     R1,60(R3)           GET A(MASTC) FROM TWA                        
         USING MASTD,R1                                                         
         L     R1,MCVLOGOC                                                      
         USING LOGOD,R1                                                         
         MVC   PREQ,SPACES                                                      
         MVC   PREQ(5),=C'LOGO='                                                
         MVC   PREQ+5(7),LOGO1                                                  
         MVC   PREQ+13(7),LOGO2                                                 
         MVC   PREQ+21(8),LOGOJOB                                               
*                                                                               
         MVC   PREQ+33(8),=C'ORIGIN='                                           
         LH    R0,10(R3)           GET ORIGIN ID NUMBER                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PREQ+41(5),DUB                                                   
         B     EXIT                                                             
*                                                                               
*                                                                               
REQNUM   DC    PL4'0'                                                           
SORTCARD DC    CL80'SORT FIELDS=(1,64,A),FORMAT=BI'                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=64'                                    
SREQ     DC    CL80' '                                                          
PREQ     DC    CL80' '                                                          
IREQ     DC    CL80' '                                                          
DREQ     DC    CL80' '                                                          
NEXTCOL  DS    A                                                                
LASTCOL  DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
TR04FIL  DCB   DDNAME=TR04FIL,DSORG=PS,RECFM=FB,LRECL=64,              X        
               BLKSIZE=3200,MACRF=GM,EODAD=ENDFILE                              
*                                                                               
PTNQFIL  DCB   DDNAME=PTNQFIL,DSORG=PS,RECFM=FB,LRECL=80,              X        
               BLKSIZE=2000,MACRF=PM                                            
*                                                                               
SHPQFIL  DCB   DDNAME=SHPQFIL,DSORG=PS,RECFM=FB,LRECL=80,              X        
               BLKSIZE=2000,MACRF=PM                                            
*                                                                               
INSQFIL  DCB   DDNAME=INSQFIL,DSORG=PS,RECFM=FB,LRECL=80,              X        
               BLKSIZE=2000,MACRF=PM                                            
*                                                                               
SPSDFIL  DCB   DDNAME=SPSDFIL,DSORG=PS,RECFM=FB,LRECL=80,              X        
               BLKSIZE=2000,MACRF=PM                                            
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAINSTN                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050SPTRA98   06/11/15'                                      
         END                                                                    
