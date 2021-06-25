*          DATA SET SPMIS03    AT LEVEL 014 AS OF 03/04/18                      
*PHASE T20B03B                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'SPMIS03 - MKT LIST FEATURE '                                    
***********************************************************************         
* DATE    LEV WHO  DESCRIPTION                                        *         
* ------- --- ---- -------------------------------------------------- *         
* 18SEP95 11  SPRI LIST IDR'S                                         *         
* 09APR01     MHER PURPOSE CODES                                      *         
***********************************************************************         
T20B03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20B03,RR=R5                                                   
         L     RC,0(R1)                                                         
         LA    R8,2048(RC)                                                      
         LA    R8,2048(R8)                                                      
         USING GENOLD,RC,R8        WORK                                         
         USING T20BFFD,RA          TWA                                          
         ST    R5,RELO                                                          
         L     R9,ABUCKETS                                                      
         USING BUCKETSD,R9                                                      
         SPACE                                                                  
         FOUT  MISMKTNH,SPACES,23    CLEAR HEADERS                              
         FOUT  MISSTAH,SPACES,8                                                 
         FOUT  MISDPTH,SPACES,4                                                 
         FOUT  MISLENH,SPACES,5                                                 
         FOUT  MISEQUH,SPACES,13                                                
         CLC   MISEST(3),=C'ALL'                                                
         BE    MIS04                                                            
         FOUT  MISPERH,SPACES,17                                                
MIS04    CLC   MISFOR,=C'NAMES'       NAMES OPTION                              
         BE    MIS05                                                            
         FOUT  MISFORH,SPACES,5                                                 
         CLC   =C'LIST',MISMKT     CHECK IF MARKET LIST                         
         BNE   MIS10               NO, DON'T COMPARE KEYS                       
         B     MIS05                                                            
RELO     DC    A(0)                                                             
         SPACE                                                                  
MIS05    CLC   M03KEY,SAVBKEY      FIRST TIME                                   
         BNE   MIS10               YES                                          
         CLC   M03EST,SAVBKEY+9                                                 
         BNE   MIS10               YES                                          
         CLC   M03FOR,MISFOR                                                    
         BNE   LD12                FORMAT FIELD CHANGED(NAMES/BLANK)            
         SPACE                                                                  
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    DSP05                                                            
         MVC   HALF,MISPAGE                                                     
         OC    HALF,SPACES                                                      
         CLC   HALF,MISMSG+11      IS LAST PG DISPLAYED                         
         BNE   DSP05                                                            
         B     LD12                                                             
         SPACE                                                                  
MIS10    DS    0H                  SET BINSRCH PARS                             
         SR    R0,R0               ADDRS OF INSERT                              
         LA    R1,MKTBL            ADDRS OF TBL                                 
         SR    R2,R2               NUM OF RECS SO FAR                           
         LA    R3,3                L'REC                                        
         LA    R4,2                BYTE 0=KEY DISP,1-3=L'KEY                    
         CLC   =C'LIST',MISMKT     CHECK IF MARKET LIST                         
         BE    *+10                                                             
         LA    R3,L'SVID+1         NO, IDR LIST                                 
         LR    R4,R3               BYTE 0=KEY DISP,1-3=L'KEY                    
         LA    R5,2000             MAX NUM OF RECS                              
         STM   R0,R5,BINPARS                                                    
         SPACE                                                                  
         MVI   NMSW,0              CLEAR THIS FUCKING SWITCH                    
         MVI   PGNUM,0             SET PAGE NUM TO 0                            
         LA    RE,MKTBL            CLEAR TABLE                                  
         LHI   RF,6000                                                          
         XCEF                                                                   
         CLC   =C'LIST',MISMKT     CHECK IF MARKET LIST                         
         BNE   DSP05               NO, MUST BE IDR LIST                         
         SPACE                                                                  
         EJECT                                                                  
*************************                                                       
*    READ BUY RECORDS  *                                                        
*************************                                                       
         SPACE                                                                  
RDBYS    DS    0H                                                               
RB00     XC    KEY,KEY                                                          
         MVC   KEY(4),SAVBKEY                                                   
         MVI   KEY+5,1                  START WITH MKT 1                        
         MVC   KEY+9(1),SAVBKEY+9                                               
         SPACE                                                                  
RBHIGH   GOTO1 HIGH                                                             
         B     RB10                                                             
         SPACE                                                                  
RBSEQ    GOTO1 SEQ                                                              
         SPACE                                                                  
RB10     CLC   KEY(4),KEYSAVE           A-M/CLT/PRD                             
         BNE   RDGLS                    GOTO READ GOAL REC                      
         CLI   SAVBKEY+9,0              TEST EST REQUESTED                      
         BE    RB15                     NO                                      
         CLC   KEY+9(1),SAVBKEY+9       IS EST EQUAL                            
         BE    RB50                                                             
         BH    RB20                                                             
         MVC   KEY+9(1),SAVBKEY+9        EST IS LOW. SET NEEDED EST             
         XC    KEY+10(3),KEY+10                                                 
         B     RBHIGH                                                           
*                                                                               
RB15     ZIC   RE,KEY+9                                                         
         LA    RE,SAVESLST(RE)                                                  
         CLI   0(RE),0             TEST EST ACTIVE                              
         BNZ   RB50                YES - PROCESS                                
         MVC   KEY+10(3),=4X'FF'   FORCE NEXT EST                               
         B     RBHIGH                                                           
*                                                                               
RB20     MVC   KEY+9(4),=4X'FF'          EST IS HIGH. FORCE NXT STA             
         B     RBHIGH                                                           
         SPACE                                                                  
* TEST FOR AND IGNORE SPILL POINTERS *                                          
RB50     DS    0H                                                               
         CLI   KEY+3,X'FF'                                                      
         BNE   RB55                                                             
         TM    KEY+10,X'80'                                                     
         BO    RBSEQ               NOT OK. GOTO SEQ                             
         B     RB60                OK.     GOTO DELETE CHECK                    
RB55     CLI   KEY+10,X'FF'                                                     
         BE    RB60                OK.     GOTO DELETE CHECK                    
         TM    KEY+10,X'80'                                                     
         BZ    RB60                OK.     GOTO DELETE CHECK                    
         CLI   KEY+11,X'00'                                                     
         BE    RBSEQ               NOT OK. GOTO SEQ                             
         SPACE                                                                  
RB60     CLI   SAVBKEY+3,X'FF'     REQUESTED PRD = POL                          
         BE    RB70                                                             
         EJECT                                                                  
* PRD NOT POL *                                                                 
         SPACE                                                                  
         MVI   DMINBTS,X'08'                                                    
         GOTO1 GETREC                                                           
         MVI   DMINBTS,0                                                        
         TM    DMCB+8,X'02'         IS IT DELETED                               
         BO    RBSEQ               YES, GOTO SEQ                                
* FIND A SPOT ALLOCATED TO THIS BRAND                                           
         L     R1,AIO                                                           
         CLI   3(R1),X'FF'         TEST POL BUY                                 
         BNE   RB70                                                             
         LA    R1,24(R1)           POINT TO FIRST ELEMENT                       
         SR    R0,R0                                                            
         MVI   BYTE,C'N'           RESET SPOT FOUND FLAG                        
RB62     ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         CLI   0(R1),0             TEST E-O-R                                   
         BE    RB66                                                             
         CLI   0(R1),X'0B'                                                      
         BL    RB62                                                             
         CLI   0(R1),X'0D'                                                      
         BH    RB62                                                             
         MVI   BYTE,C'Y'           SET FLAG WE FOUND A SPOT                     
         ZIC   RE,1(R1)                                                         
         AHI   RE,-10                                                           
         BNP   RB62                                                             
         SRL   RE,2                SET FOR NUMBER OF ALLOCATIONS                
         LA    RF,10(R1)                                                        
RB64     CLC   SAVBKEY+3(1),0(RF)  MATCH ON PRD                                 
         BE    RB70                                                             
         LA    RF,4(RF)                                                         
         BCT   RE,RB64                                                          
         B     RB62                                                             
*                                                                               
RB66     CLI   BYTE,C'Y'           DID WE FIND ANY BUY ELEMS                    
         BE    RBSEQ               YES - FORGET ABOUT THIS BUY                  
         CLC   SAVBKEY+3(1),BDMASPRD  TEST MASPRD FOR OUR PRD                   
         BE    RB70                                                             
         CLC   SAVBKEY+3(1),BDMASPRD+1                                          
         BE    RB70                                                             
         B     RBSEQ                                                            
         SPACE                                                                  
RB70     XC    WORK,WORK                                                        
         MVC   WORK(2),KEY+4       MKT NUM TO WORK                              
         OI    WORK+2,X'F0'        SET BUY BIT                                  
         BAS   RE,LDTBL                                                         
         MVC   KEY+6(7),=7X'FF'    FORCE NXT MKT                                
         B     RBHIGH                                                           
         EJECT                                                                  
**********************                                                          
* READ GOAL RECORDS  *                                                          
**********************                                                          
         SPACE                                                                  
RDGLS    DS    0H                                                               
         CLI   SAVGKEY+4,X'FF'     TEST POL REQUEST                             
         BE    RG60                                                             
         SPACE                                                                  
RG10     DS    0H                  PRD NOT = POL                                
         XC    KEY,KEY                                                          
         MVC   KEY(5),SAVGKEY      02/A-M/CLT/PRD                               
         MVC   KEY+7(1),SAVGKEY+7                                               
         SPACE                                                                  
RG20     GOTO1 HIGH                                                             
         B     RG25                                                             
*                                                                               
RG22     GOTO1 SEQ                                                              
*                                                                               
RG25     CLC   KEY(5),KEYSAVE          02/A-M/CLT/PRD                           
         BNE   LD10                    GOTO LOAD RTN                            
         CLI   SAVGKEY+7,0             TEST EST REQUESTED                       
         BE    RG30                    NO                                       
         CLC   KEY+7(1),SAVGKEY+7      COMPARE EST                              
         BE    RG50                                                             
         BH    RG55                                                             
         MVC   KEY+7(1),SAVGKEY+7      EST IS LOW. SET NEEDED EST               
         XC    KEY+8(5),KEY+8                                                   
         B     RG20                                                             
*                                                                               
RG30     ZIC   RE,KEY+7                                                         
         LA    RE,SAVESLST(RE)                                                  
         CLI   0(RE),0             TEST EST ACTIVE                              
         BNZ   RG50                YES - PROCESS                                
         MVC   KEY+8(5),=6X'FF'    FORCE NEXT EST                               
         B     RG20                                                             
*                                                                               
RG50     BAS   RE,TESTGLS          TEST FOR ACTIVE GOAL ELEM                    
         BNZ   RG22                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),KEY+5       MKT NUM TO WORK                              
         OI    WORK+2,X'0F'        SET GOAL BITS                                
         BAS   RE,LDTBL                                                         
RG55     MVC   KEY+7(6),=6X'FF'    FORCE NXT MKT                                
         B     RG20                                                             
         EJECT                                                                  
*                                                                               
RG60     DS    0H                  PRD = POL                                    
         XC    KEY,KEY                                                          
         MVC   KEY(4),SAVGKEY                                                   
         MVC   KEY+7(1),SAVGKEY+7                                               
         MVI   KEY+4,X'01'         START PRD=X'01'                              
         SPACE                                                                  
RG80     GOTO1 HIGH                                                             
         B     RG90                                                             
*                                                                               
RG85     GOTO1 SEQ                                                              
*                                                                               
RG90     CLC   KEY(4),KEYSAVE           02/A-M/CLT                              
         BNE   LD10                     GOTO LOAD RTN                           
         CLI   KEY+4,X'FF'                                                      
         BE    LD10                                                             
         CLI   KEY+7,0                  TEST EST REQUESTED                      
         BE    RG95                     NO                                      
         CLC   KEY+7(1),SAVGKEY+7       COMPARE EST                             
         BE    RG110                                                            
         BH    RG100                                                            
         MVC   KEY+7(1),SAVGKEY+7    EST IS LOW. SET NEEDED EST                 
         XC    KEY+8(5),KEY+8                                                   
         B     RG80                                                             
*                                                                               
RG95     ZIC   RE,KEY+7                                                         
         LA    RE,SAVESLST(RE)                                                  
         CLI   0(RE),0             TEST EST ACTIVE                              
         BNZ   RG110               YES - PROCESS                                
         MVC   KEY+8(5),=6X'FF'    FORCE NEXT EST                               
         B     RG80                                                             
*                                                                               
RG100    MVC   KEY+7(6),=6X'FF'      EST IS HIGH. FORCE NXT MKT                 
         B     RG80                                                             
         SPACE                                                                  
RG110    BAS   RE,TESTGLS          TEST FOR ACTIVE GOALS                        
         BNZ   RG85                NO                                           
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),KEY+5       MKT NUM TO WORK                              
         OI    WORK+2,X'0F'        SET GOAL BITS                                
         BAS   RE,LDTBL                                                         
         MVC   KEY+7(6),=6X'FF'    FORCE NXT MKT                                
         B     RG80                                                             
         EJECT                                                                  
* READ GOAL RECORD AND SEARCH FOR AN ACTIVE ELEMENT                             
* EXIT WITH CC EQ IF FOUND, NEQ IF NOT                                          
         SPACE 1                                                                
TESTGLS  NTR1                                                                   
         GOTO1 GETREC                                                           
*                                                                               
         SR    R0,R0                                                            
         LA    R2,GDELEM                                                        
TESTGL2  CLI   0(R2),0                                                          
         BE    NEQXIT                                                           
         CLI   0(R2),X'21'         TEST GOAL ELEM                               
         BE    EQXIT                                                            
         ICM   R0,1,1(R2)                                                       
         BZ    NEQXIT                                                           
         AR    R2,R0                                                            
         B     TESTGL2                                                          
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
********************                                                            
*    LOAD TABLE    *                                                            
********************                                                            
         SPACE                                                                  
LDTBL    NTR1                                                                   
         LA    R5,WORK                                                          
         GOTO1 =V(BINSRCH),BINPARS,(1,(R5)),RR=RELO                             
         L     R7,BRECAD                                                        
         LTR   R7,R7                                                            
         BZ    LD10                                                             
         CLC   =C'LIST',MISMKT     CHECK IF MARKET LIST                         
         BNE   *+10                                                             
         OC    0(3,R7),WORK          WORK HAS MKT NUM + B/G                     
         B     EXIT                                                             
         SPACE                                                                  
LD10     ICM   R1,15,BRECN                                                      
         BZ    NOMKTRTN                                                         
         B     LD14                                                             
*                                                                               
LD12     MVI   PGNUM,0             FMT FIELD CHANGED(NAMES/BLANK)               
         L     R1,BRECM                                                         
         MVI   NMSW,1                                                           
*                                                                               
LD14     SR    R0,R0                                                            
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    LD25                                                             
         CLC   MISFOR,=C'NAMES'                                                 
         BNE   LD30                                                             
LD25     D     R0,=F'30'           DIVIDE REC NUM BY SCREEN MAX                 
         B     LD35                                                             
LD30     D     R0,=F'105'          DIVIDE REC NUM BY SCREEN MAX                 
LD35     LTR   R0,R0               ANY REMAINDER                                
         BZ    LD40                                                             
         LA    R1,1(R1)            ADD 1 PG FOR REMAINDER                       
LD40     STC   R1,MAXPG                                                         
         CLI   NMSW,1                                                           
         BNE   DSPRTN                                                           
*        XC    NMSW,NMSW           CLEAR IT                                     
         B     DSP05                                                            
         EJECT                                                                  
************************                                                        
* MOVE TABLE TO SCREEN *                                                        
*  1 PAGE AT A TIME    *                                                        
************************                                                        
         SPACE                                                                  
DSPRTN   DS    0H                                                               
         MVC   BRECM,BRECN         MOVE NUM OF REC TO MAX                       
         XC    BRECN,BRECN         CLEAR REC NUM                                
         SPACE                                                                  
DSP05    TM    PCDRIVEN,PCGRIDQ   *USING GRIDS?                                 
         BO    DSP15                                                            
         LA    R2,MISL8H          *CLEAR SCREEN                                 
DSP10    OC    8(79,R2),8(R2)                                                   
         BZ    DSP13                                                            
         XC    8(79,R2),8(R2)                                                   
         FOUT  (R2)                                                             
DSP13    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BH    DSP10                                                            
         SPACE                                                                  
DSP15    CLC   =C'LIST',MISMKT     CHECK IF MARKET LIST                         
         BNE   IDR                                                              
         LA    R4,MKTBL            *SET UP SCREEN                               
*                                                                               
         TM    PCDRIVEN,PCGRIDQ   *USING GRIDS?                                 
         BZ    DSP20                                                            
         ICM   R1,15,BRECN                                                      
         BZ    DSP18                                                            
         BCTR  R1,0                                                             
         ST    R1,BRECN                                                         
         MHI   R1,3                                                             
         LA    R4,0(R1,R4)                                                      
DSP18    XC    WORK3,WORK3                                                      
         LA    R3,WORK3                                                         
         B     DSP45                                                            
*                                                                               
DSP20    LA    R2,MISL10H                                                       
         LR    R3,R2                                                            
         MVI   COLNUM,X'01'       SET SCREEN COLUMN TO 1                        
         SPACE 2                                                                
*                               *GET DISPLACEMENT INTO MKTBL                    
         SPACE                                                                  
         LA    R1,MISPAGEH         MISPAGE NUMERIC                              
         CLI   5(R1),0                                                          
         BNE   DSP23                                                            
         MVI   PGNUM,0                                                          
         B     DSP38                                                            
DSP23    TM    4(R1),X'08'                                                      
         BO    DSP25                                                            
         XC    MISMSG,MISMSG                                                    
         FOUT  MISMSGH,=C'** ERROR ** PAGE NOT NUMERIC',29                      
         LA    R2,MISPAGEH                                                      
         OI    6(R2),X'40'                                                      
         FOUT  (R2)                                                             
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
DSP25    CLI   NMSW,1                                                           
         BNE   DSP28                                                            
         MVI   NMSW,0              CLEAR THIS FUCKING SWITCH                    
         TM    MISPAGEH+4,X'20'    JUST ENTERED?                                
         BNZ   DSP30               NO                                           
         OI    MISPAGEH+4,X'20'    VALIDATE IT NOW                              
*                                                                               
DSP28    ZIC   RE,5(R1)           YES. MISPAGE NUMERIC                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R1)                                                      
         CVB   R0,DUB                                                           
         LTR   R0,R0               MISPAGE=0                                    
         BNZ   DSP32               NO.                                          
DSP30    MVI   PGNUM,0             YES.                                         
         B     DSP38                                                            
DSP32    STC   R0,BYTE                                                          
         CLC   BYTE,PGNUM          MISPAGE=PGNUM                                
         BNE   DSP34               NO                                           
         AHI   R0,1                YES.ADD 1 TO REQUESTED PG                    
         STC   R0,BYTE                                                          
         SPACE                                                                  
DSP34    CLC   BYTE,MAXPG          REQ PG GRTR THAN MAX PG                      
         BNH   DSP36                                                            
         ZIC   R0,MAXPG                                                         
DSP36    AHI   R0,-1               R0 HAS REQUESTED PG NUM                      
         STC   R0,PGNUM                                                         
         SPACE                                                                  
DSP38    CLC   MISFOR,=C'NAMES'                                                 
         BNE   DSP40                                                            
         ZIC   R0,PGNUM                                                         
         MHI   R0,90               NUM OF PGS X BYTES PER PAGE                  
         AR    R4,R0               ADD DISP TO MKTBL ADDRS                      
         ZIC   R0,PGNUM            GET NUM OF RECS DISPLAYED                    
         MHI   R0,30                                                            
         ST    R0,BRECN                                                         
         B     DSP45                                                            
         SPACE                                                                  
DSP40    ZIC   R0,PGNUM            NO NAMES OPTION                              
         MHI   R0,315                                                           
         AR    R4,R0               GET DISP OF MKTBL                            
         ZIC   R0,PGNUM            GET NUM OF RECS DISPLAYED                    
         MHI   R0,105                                                           
         ST    R0,BRECN                                                         
         SPACE                                                                  
         USING MKTLSTD,R3                                                       
DSP45    TM    2(R4),X'F0'        *BUYS AND/OR GOALS                            
         BZ    DSP50                                                            
         MVI   MLBUY,C'B'                                                       
DSP50    TM    2(R4),X'0F'                                                      
         BZ    DSP52                                                            
         MVI   MLGOAL,C'G'                                                      
         SPACE                                                                  
DSP52    SR    R5,R5              *UNPK MKT NUM                                 
         ICM   R5,3,0(R4)                                                       
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MLMKT,DUB                                                        
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    DSP55                                                            
         CLC   MISFOR,=C'NAMES'    NAMES OPTION                                 
         BNE   DSP60                                                            
         SPACE                                                                  
DSP55    XC    KEY,KEY            *GET MKT NAME                                 
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),MISMED                                                  
         MVC   KEY+2(4),MLMKT      11(R3) HAS MKT NUM                           
         MVC   KEY+6(2),AGYALPHA                                                
         MVC   COMMAND(6),=C'DMRDHI'                                            
         GOTO1 STA                                                              
         LA    R5,IOAREA                                                        
         USING MKTREC,R5                                                        
         MVC   MLMKTNM(13),=C'** UNKNOWN **'                                    
         CLC   KEY(8),IOAREA                                                    
         BNE   DSP60                                                            
         MVI   MLDASH,C'-'                                                      
         MVC   MLMKTNM,MKTNAME    MKTNAME TO SCREEN                             
         DROP  R5,R3                                                            
         EJECT                                                                  
DSP60    LA    R4,3(R4)           *INCREMENT TBL                                
         L     R5,BRECN                                                         
         AHI   R5,1                                                             
         ST    R5,BRECN                                                         
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    DSP63                                                            
         GOTO1 VFORMGRD,DMCB,GCLDET                                             
         BNE   FTRTN                                                            
DSP63    CLC   BRECN,BRECM         REC NUM = REC TOTAL                          
         BE    FTRTN               YES.END OF TBL. GOTO FOUT RTN                
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    DSP65                                                            
         XC    WORK3,WORK3                                                      
         B     DSP45                                                            
*                                                                               
         SPACE                                                                  
DSP65    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         AR    R3,R0                                                            
         CLI   0(R2),9                                                          
         BH    DSP45                                                            
         SPACE                                                                  
         CLC   MISFOR,=C'NAMES'    NAMES REQUESTED                              
         BNE   NONMDSP             NO.                                          
         SPACE                                                                  
         CLI   COLNUM,X'01'          1ST SCREEN COLUMN                          
         BE    DSP70                                                            
         B     FTRTN               END OF 2D COLUMN.GOTO FOUT RTN               
DSP70    MVI   COLNUM,X'02'                                                     
         LA    R2,MISL10H                                                       
         LR    R3,R2                                                            
         LA    R3,40(R3)                                                        
         B     DSP45                                                            
         SPACE                                                                  
NONMDSP  DS    0H                  NO NAME OPTION                               
         CLI   COLNUM,X'01'                                                     
         BE    NO10                                                             
         CLI   COLNUM,X'02'                                                     
         BE    NO20                                                             
         CLI   COLNUM,X'03'                                                     
         BE    NO30                                                             
         CLI   COLNUM,X'04'                                                     
         BE    NO40                                                             
         CLI   COLNUM,X'05'                                                     
         BE    NO50                                                             
         CLI   COLNUM,X'06'                                                     
         BE    NO60                                                             
         B     FTRTN               END OF 7TH COL.GOTO FOUT RTN                 
         SPACE                                                                  
NO10     MVI   COLNUM,X'02'                                                     
         LA    R2,MISL10H                                                       
         LR    R3,R2                                                            
         LA    R3,11(R3)                                                        
         B     DSP45                                                            
NO20     MVI   COLNUM,X'03'                                                     
         LA    R2,MISL10H                                                       
         LR    R3,R2                                                            
         LA    R3,21(R3)                                                        
         B     DSP45                                                            
NO30     MVI   COLNUM,X'04'                                                     
         LA    R2,MISL10H                                                       
         LR    R3,R2                                                            
         LA    R3,31(R3)                                                        
         B     DSP45                                                            
NO40     MVI   COLNUM,X'05'                                                     
         LA    R2,MISL10H                                                       
         LR    R3,R2                                                            
         LA    R3,41(R3)                                                        
         B     DSP45                                                            
NO50     MVI   COLNUM,X'06'                                                     
         LA    R2,MISL10H                                                       
         LR    R3,R2                                                            
         LA    R3,51(R3)                                                        
         B     DSP45                                                            
NO60     MVI   COLNUM,X'07'                                                     
         LA    R2,MISL10H                                                       
         LR    R3,R2                                                            
         LA    R3,61(R3)                                                        
         B     DSP45                                                            
         EJECT                                                                  
FTRTN    DS    0H                *FOUT ROUTINE                                  
         ZIC   R1,PGNUM            KEEP PAGE COUNT                              
         LA    R1,1(R1)                                                         
         STC   R1,PGNUM                                                         
         SPACE                                                                  
         TM    PCDRIVEN,PCGRIDQ                                                 
         BO    FT50                                                             
         LA    R2,MISL10H          FOUT SCREEN                                  
FT00     FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BH    FT00                                                             
         SPACE                                                                  
*                                                                               
         EDIT  (1,PGNUM),(2,MISPAGE),ALIGN=LEFT                                 
         STC   R0,MISPAGEH+5                                                    
         LA    R2,MISPAGEH                                                      
         OI    1(R2),X'01'         FORCE MODIFIED BIT                           
         OI    6(R2),X'40'         POSITION CURSOR                              
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALIDATE IT                                  
         SPACE                                                                  
FT50     MVC   M03KEY,SAVBKEY      STORE A-M/CLT/PRD                            
         MVC   M03EST,SAVBKEY+9    STORE EST                                    
         MVC   M03FOR,MISFOR       STORE FORMAT                                 
         SPACE 1                                                                
* WRITE TWA SAVE AREAS *                                                        
         SPACE 1                                                                
         LA    R2,TWAPAGES         NUMBER OF PAGES                              
         L     R3,AMISTWA2         INPUT AREA                                   
         L     RF,VTWA                                                          
         MVC   DMCB+10(2),2(RF)    2 BYTE TERM NO.                              
         LA    R4,1                                                             
*                                                                               
PUTTWAS  STC   R4,DMCB+8           PAGE NO                                      
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R3)                        
         LA    R4,1(R4)            NEXT PAGE                                    
         AH    R3,TWASIZE          NEXT INPUT AREA                              
         BCT   R2,PUTTWAS                                                       
*                                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    PTTW10                                                           
         CLC   BRECN,BRECM         REC NUM = REC TOTAL                          
         BNE   EXIT                                                             
         TM    PCDRIVE2,PC2SCRFL   ERROR DUE TO FULL SCREEN?                    
         BO    EXIT                                                             
         GOTO1 VFORMGRD,DMCB,GFEOR                                              
         B     EXIT                                                             
PTTW10   XC    MISMSG,MISMSG                                                    
         MVC   MISMSG(4),=C'PAGE'                                               
         LA    R5,MISMSG+5                                                      
         EDIT  (1,PGNUM),(2,(R5)),ZERO=BLANK,ALIGN=LEFT                         
         MVC   MISMSG+8(2),=C'OF'                                               
         LA    R5,MISMSG+11                                                     
         EDIT  (1,MAXPG),(2,(R5)),ZERO=BLANK,ALIGN=LEFT                         
         MVC   MISMSG+14(9),=C'DISPLAYED'                                       
         SPACE                                                                  
         MVC   MISMSG+24(16),=C'(TOTAL MARKETS = '                              
         LA    R5,MISMSG+41                                                     
         EDIT  (4,BRECM),(4,(R5)),ALIGN=LEFT,ZERO=BLANK                         
         MVC   MISMSG+44(1),=C')'                                               
         MVC   MISMSG+46(3),=C'...'                                             
         SPACE                                                                  
         LA    R2,MISMSGH                                                       
         FOUT  (R2)                                                             
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
NOMKTRTN DS    0H                  NO ACTIVE MKTS                               
         LA    R2,MISL8H                                                        
NM10     XC    8(79,R2),8(R2)                                                   
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BH    NM10                                                             
         XC    MISMSG,MISMSG                                                    
         CLC   MISMKT,=C'LIST'                                                  
         BNE   NM20                                                             
         FOUT  MISMSGH,=C'** ERROR ** NO ACTIVE MARKETS **',32                  
         B     NM30                                                             
NM20     FOUT  MISMSGH,=C'** ERROR ** NO PURPOSE CODES **',31                   
NM30     LA    R2,MISMEDH                                                       
         OI    6(R2),X'40'                                                      
         FOUT  (R2)                                                             
ERREXIT  MVI   ERRAREA,X'FF'       FORCE ERROR IN CONTROLLER!!                  
         B     EXIT                                                             
         SPACE                                                                  
         EJECT                                                                  
************************                                                        
* DISPLAY IDR'S        *                                                        
************************                                                        
IDR      DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(8),SAVGKEY      GOAL KEY                                     
         GOTO1 HIGH                                                             
         B     IDR20                                                            
IDR10    GOTO1 SEQ                                                              
*                                                                               
IDR20    CLC   KEY(8),SAVGKEY                                                   
         BNE   IDR50                                                            
         TM    KEY+11,X'40'        GOAL WITH AN IDR                             
         BO    IDR30                                                            
         MVI   WORK,X'FF'          NO, SO ADD AN ENTRY WITH FF'S                
         MVC   WORK+1(L'SVID-1),WORK                                            
         B     IDR40                                                            
IDR30    GOTO1 GETREC                                                           
         MVC   WORK(L'GDIDR),GDIDR                                              
IDR40    MVI   WORK+L'SVID,C'G'                                                 
         BAS   RE,LDTBL            PUT IN TABLE                                 
         B     IDR10                                                            
*                                                                               
IDR50    XC    KEY,KEY                                                          
         MVC   KEY(6),SAVBKEY      A/M/CLT/PRD/MKT                              
         MVC   KEY+9(1),SAVBKEY+9  ESTIMATE                                     
IDR60    GOTO1 HIGH                                                             
         B     IDR80                                                            
IDR70    GOTO1 SEQ                                                              
*                                                                               
IDR80    CLC   KEY(6),SAVBKEY                                                   
         BNE   IDR110                                                           
         CLC   KEY+9(1),SAVBKEY+9    COMPARE ESTIMATE                           
         BE    IDR90                                                            
         MVC   KEY+9(4),=X'FFFFFFFF' IF HIGHER, GO TO NEXT STATION              
         BH    IDR60                                                            
         MVC   KEY+9(1),SAVBKEY+9    IF LOWER READ FOR THE ESTIMATE             
         XC    KEY+10(3),KEY+10      AND CLEAR REST OF KEY                      
         B     IDR60                                                            
*                                                                               
IDR90    DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R3,BDELEM                                                        
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
*                                                                               
         LA    R1,=6X'FF'                                                       
         BAS   RE,NEXTEL                                                        
         BNE   IDR100              NO ID ELEMENT                                
         LA    R1,9(R3)            POINT TO IDR                                 
         CLI   SVB0PROF+9,C'Y'     TEST PURPOSE CODES                           
         BNE   *+8                                                              
         LA    R1,3(R3)            POINT TO PURPOSE CODE                        
*                                                                               
IDR100   MVC   WORK(6),0(R1)                                                    
         MVI   WORK+6,C'B'                                                      
         BAS   RE,LDTBL            PUT IN TABLE                                 
         B     IDR70                                                            
*                                                                               
IDR110   LA    R2,MISL10H          FIRST LINE                                   
         LA    R3,MISL23H          LAST LINE                                    
         LA    R4,MKTBL            ADDRESS OF TABLE                             
         OC    0(L'SVID,R4),0(R4)  NOTHING TO DISPLAY?                          
         BZ    NOMKTRTN            YES, EXIT                                    
*                                                                               
IDR120   CR    R2,R3               END OF SCREEN?                               
         BH    IDR140                                                           
IDR125   OC    0(L'SVID,R4),0(R4)  END OF TABLE?                                
         BZ    IDR140                                                           
*                                                                               
         USING MKTLSTD,R2                                                       
         TM    PCDRIVEN,PCGRIDQ    USING GRIDS?                                 
         BZ    *+14                                                             
         XC    WORK3,WORK3                                                      
         LA    R2,WORK3                                                         
*                                                                               
         CLI   0(R4),X'FF'                                                      
         BNE   *+14                                                             
         MVC   MLPUR,=C'*NONE*'                                                 
         B     *+10                                                             
         MVC   MLPUR,0(R4)         MOVE IDR TO SCREEN                           
         CLI   L'SVID(R4),C'B'     CAME FROM A BUY?                             
         BNE   IDR130              NO, PUT IN G FOR GOAL                        
         MVI   MLBUY,C'B'          YES, PUT B FOR BUY                           
         CLC   L'SVID+1(L'SVID,R4),0(R4) NEXT ENTRY A GOAL W/ SAME IDR?         
         BNE   *+12                                                             
         LA    R4,L'SVID+1(R4)     INCREMENT TBL AGAIN                          
IDR130   MVI   MLGOAL,C'G'                                                      
         DROP  R2                                                               
*                                                                               
         LA    R4,L'SVID+1(R4)     INCREMENT TBL                                
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    IDR135                                                           
         GOTO1 VFORMGRD,DMCB,GCLDET                                             
         BNE   IDR140                                                           
         B     IDR125                                                           
IDR135   OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R1,0(R3)            NEXT FIELD                                   
         AR    R2,R1                                                            
         B     IDR120                                                           
*                                                                               
IDR140   TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    IDR150                                                           
         GOTO1 VFORMGRD,DMCB,GFEOR                                              
         B     EXIT                                                             
*                                                                               
IDR150   LA    R2,MISFORH          CURSOR                                       
         MVC   MISMSG,MIS                                                       
         FOUT  MISMSGH                                                          
         OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
NEXTEL   SR    R0,R0                                                            
         CLI   0(R3),0                                                          
         BE    NEXTELX                                                          
         ICM   R0,1,1(R3)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
         CLC   0(1,R3),ELCDLO                                                   
         BL    NEXTEL                                                           
         CLC   0(1,R3),ELCDHI                                                   
         BH    NEXTEL                                                           
         CR    RE,RE               SET CC EQUAL                                 
         BR    RE                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  SET CC NOT EQ                                
*                                                                               
SPACES   DC    60C' '                                                           
MIS      DC    28C' '                                                           
         DC    CL32'MEDIA INFORMATION SYSTEM'                                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPMISWORK                                                      
*                                                                               
BUCKETSD DSECT                     *** BINSRCH PARAMETERS ***                   
BINPARS  DS    0F                                                               
BRECAD   DS    F                   A(INSERT REC) OR A(0) WHEN TBL FULL          
BTBLAD   DS    F                   A(TBL)                                       
BRECN    DS    F                   TOTAL NUM OF REC IN TBL                      
BRECL    DS    F                   L'REC                                        
BRECKEY  DS    F                   0 DISP OF KEY,1-3 L'KEY                      
BRECM    DS    F                   MAX NUM OF REC IN TBL                        
*                                                                               
M03KEY   DS    CL4                 A-M/CLT/PRD OF 03 PHASE                      
M03EST   DS    CL1                 EST OF 03 PHASE                              
M03FOR   DS    CL5                 FORMAT OF 03 PHASE                           
COLNUM   DS    CL1                                                              
FRSTSW   DS    CL1                                                              
MAXPG    DS    CL1                 MAX NUM OF SCREEN PAGES                      
PGNUM    DS    CL1                                                              
NMSW     DS    CL1                                                              
*                                                                               
MKTBL    DS    0C                                                               
*                                                                               
         EJECT                                                                  
GENOLD   DSECT                                                                  
*                                                                               
         ORG   IOAREA                                                           
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE SPGENMKT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPMIS03   03/04/18'                                      
         END                                                                    
