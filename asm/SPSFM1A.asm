*          DATA SET SPSFM1A    AT LEVEL 023 AS OF 08/11/11                      
*PHASE T2171AA                                                                  
         TITLE 'T2171A  COMPETITION STATION LIST RECORDS'                       
T2171A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2171A                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       XC    SVKEY,SVKEY                                                      
         LA    R3,SVKEY                                                         
         USING CLSRECD,R3                                                       
         MVI   CLSKTYP,CLSKTYPQ                                                 
         MVI   CLSKSUB,CLSKSUBQ                                                 
*                                                                               
         LA    R2,STLMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   CLSKAGMD,BAGYMD                                                  
*                                                                               
         XC    STLMKN,STLMKN       MARKET                                       
         OI    STLMKNH+6,X'80'                                                  
         LA    R2,STLMKTH                                                       
         CLI   ACTEQU,ACTLIST                                                   
         BE    *+12                                                             
         CLI   ACTEQU,ACTREP                                                    
         BNE   VK10                                                             
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         B     VK20                                                             
*                                                                               
VK10     GOTO1 ANY                                                              
*                                                                               
VK20     MVI   ERROR,INVMKT                                                     
         GOTO1 VALIMKT                                                          
         MVC   CLSKMKT,BMKT                                                     
         MVC   STLMKN,MKTNM                                                     
*                                                                               
VK30     LA    R2,STLSCHH          SCHEME                                       
         XC    SVSCHEME,SVSCHEME                                                
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         MVC   SVSCHEME,8(R2)                                                   
         MVI   ERROR,INVSCH                                                     
         LA    RF,SRBLKLN          CLEAR RANSID BLOCK                           
         XCEF  SRBLK,(RF)                                                       
         MVC   SRASIR,AIO3                                                      
         MVC   SRACOM,ACOMFACS                                                  
         MVC   SRACLPAC,CLPACK                                                  
         MVC   SRAMSUNP,MSUNPK                                                  
         MVC   SRADYUNP,UNDAY                                                   
         MVC   SRAUNTIM,UNTIME                                                  
         MVC   SRSELSCH,SVSCHEME                                                
         MVC   SRSELAM,BAGYMD                                                   
         MVC   SRSELAGY,14(RA)                                                  
         MVC   SRSELMED,QMED                                                    
         MVC   SRSELCTY,SVAPROF+7                                               
         GOTO1 RANSID,DMCB,SRBLK   CALL RANSID TO VAILDATE SCHEME               
         CLI   SRERROR,SRNOSCH                                                  
         BE    TRAPERR                                                          
         MVC   CLSKSCHM,SRACTSCH                                                
         MVC   SVBSCHEM,SRACTSCH   SAVE PACKED SCHEME CODE                      
*                                                                               
VK40     MVC   KEY,SVKEY           SAVE THE KEY                                 
         B     VKX                                                              
*                                                                               
VKX      B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       MVI   ELCODE,CLSELCDQ     REMOVE ELEMENT                               
         GOTO1 REMELEM                                                          
         MVI   ELCODE,CSPELCDQ     REMOVE SPILL ELEMENT                         
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM           BUILD NEW ELEMNT                             
         LA    R5,ELEM                                                          
         USING CLSEL,R5                                                         
         MVI   CLSELCD,CLSELCDQ    STATION LIST ELEMENT                         
         SR    R0,R0                                                            
         LA    R2,STLSL1H                                                       
         LA    R4,CLSSTA                                                        
         LA    R6,STLSPLH                                                       
*                                                                               
VR10     CLI   5(R2),0                                                          
         BE    VR20                                                             
         BAS   RE,VALSTA           VALIDATE STATION                             
         BNE   TRAPERR                                                          
         MVC   0(5,R4),QSTA                                                     
         LA    R4,5(R4)                                                         
*                                                                               
VR20     IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CR    R2,R6                                                            
         BL    VR10                                                             
*                                                                               
         LA    RE,CLSEL                                                         
         SR    R4,RE                                                            
         CH    R4,=Y(CLSSTA-CLSEL)                                              
         BH    VR30                                                             
         LA    R2,STLSL1H                                                       
         GOTO1 ANY                                                              
*                                                                               
VR30     STC   R4,CLSELLN                                                       
         GOTO1 ADDELEM                                                          
*                                                                               
         XC    ELEM,ELEM           BUILD SPILL ELEMENT                          
         LA    R5,ELEM                                                          
         USING CSPELD,R5                                                        
         MVI   CSPELCD,CSPELCDQ                                                 
         SR    R0,R0                                                            
         LA    R2,STLSP1H                                                       
         LA    R4,CSPSTA                                                        
*                                                                               
VR32     CLI   5(R2),0                                                          
         BE    VR34                                                             
         BAS   RE,VALSTA                                                        
         BNE   TRAPERR                                                          
         MVC   0(5,R4),QSTA                                                     
         LA    R4,5(R4)                                                         
*                                                                               
VR34     IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   VR32                                                             
*                                                                               
         LA    RE,CSPEL                                                         
         SR    R4,RE                                                            
         CH    R4,=Y(CSPSTA-CSPEL)                                              
         BNH   VRX                                                              
         STC   R4,CSPELLN                                                       
         GOTO1 ADDELEM                                                          
*                                                                               
VRX      B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       LA    R2,STLSL1H          CLEAR UNPROTECTED FIELDS                     
         LA    RF,STLSPLH                                                       
         SR    RE,RE                                                            
*                                                                               
DR2      IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,*+4                                                           
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         CR    R2,RF                                                            
         BL    DR2                                                              
         LA    R2,STLSP1H                                                       
         BAS   RE,CLRSCRN                                                       
*                                                                               
         L     R6,AIO                                                           
         USING CLSRECD,R6                                                       
         LA    R7,CLSSTA                                                        
         LA    R4,L'CLSSTA                                                      
         ZIC   R5,CLSELLN                                                       
         LA    R5,CLSEL(R5)                                                     
         BCTR  R5,0                                                             
         LA    R2,STLSL1H                                                       
         SR    R0,R0                                                            
*                                                                               
DR10     BAS   RE,DISSTA                                                        
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BXLE  R7,R4,DR10                                                       
*                                                                               
         LA    R7,CLSEL            FORMAT SPILL STATIONS AND MARKETS            
         SR    R0,R0                                                            
*                                                                               
DR12     CLI   0(R7),0                                                          
         BE    DRX                                                              
         CLI   0(R7),CSPELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     DR12                                                             
         USING CSPELD,R7                                                        
         LA    R4,L'CSPSTA                                                      
         ZIC   R5,CSPELLN                                                       
         AR    R5,R7                                                            
         BCTR  R5,0                                                             
         LA    R2,STLSP1H                                                       
         LA    R7,CSPSTA                                                        
         SR    R0,R0                                                            
*                                                                               
DR14     BAS   RE,DISSTA                                                        
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BXLE  R7,R4,DR14                                                       
*                                                                               
DRX      B     EXIT                                                             
         SPACE 2                                                                
DISSTA   DS    0H                                                               
         LA    R3,8(R2)                                                         
         MVC   0(4,R3),0(R7)                                                    
         CLI   3(R3),C' '                                                       
         BH    *+6                                                              
         BCTR  R3,0                                                             
         MVI   4(R3),C'-'                                                       
         CLI   4(R7),C' '                                                       
         BNH   *+12                                                             
         CLI   4(R7),C'T'                                                       
         BNE   *+12                                                             
         MVI   5(R3),C'T'                                                       
         B     DISSTA10                                                         
         MVC   5(1,R3),4(R7)                                                    
         CLI   4(R7),C'L'                                                       
         BE    *+8                                                              
         MVI   6(R3),C'M'                                                       
DISSTA10 OI    6(R2),X'80'                                                      
         BR    RE                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING CLSRECD,R6                                                       
         SR    RE,RE               MARKET                                       
         ICM   RE,3,CLSKMKT                                                     
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  STLMKT,DUB                                                       
         OI    STLMKTH+6,X'80'                                                  
*                                                                               
         MVC   QMKT,STLMKT         MARKET NAME                                  
         MVC   SVKEY,KEY                                                        
         BAS   RE,GETMKT                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SVKEY                                                        
         MVC   STLMKN,MKTNM                                                     
         OI    STLMKNH+6,X'80'                                                  
*                                                                               
         MVC   STLSCH,=C'ALL'      SCHEME                                       
         OI    STLSCHH+6,X'80'                                                  
         OC    CLSKSCHM,CLSKSCHM                                                
         BZ    DKX                                                              
         GOTO1 CLUNPK,DMCB,CLSKSCHM,FULL                                        
         MVC   STLSCH,FULL                                                      
         B     DKX                                                              
*                                                                               
DKX      B     EXIT                                                             
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       LA    R2,SELSELH          CLEAR THE SCREEN                             
         BAS   RE,CLRSCRN                                                       
         MVC   AIO,AIO2                                                         
         LA    R6,KEY                                                           
         USING CLSRECD,R6                                                       
         OC    KEY(13),KEY         TEST FIRST TIME                              
         BNZ   *+10                                                             
         MVC   KEY,SVKEY                                                        
*                                                                               
LR05     GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR10     GOTO1 SEQ                                                              
*                                                                               
LR20     CLC   KEY(3),SVKEY        TEST SAME AGY/MED                            
         BNE   LRX                                                              
         OC    SVSCHEME,SVSCHEME   TEST SCHEME FILTER                           
         BZ    LR30                                                             
         LA    R6,KEY                                                           
         CLC   CLSKSCHM,SVBSCHEM                                                
         BE    LR30                                                             
         BH    *+14                                                             
         MVC   CLSKSCHM,SVBSCHEM   SKIP TO REQUEST SCHEME                       
         B     LR05                                                             
         MVI   CLSKSCHM,X'FF'      READ NEXT MARKET                             
         MVC   CLSKSCHM+1(CLSKCNTL-CLSKSCHM-1),CLSKSCHM                         
         B     LR05                                                             
*                                                                               
LR30     GOTO1 GETREC              GET RECORD                                   
         L     R6,AIO                                                           
         SR    RE,RE                                                            
         ICM   RE,3,CLSKMKT        MARKET                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         MVC   LISTAR(4),QMKT                                                   
         MVC   SVKEY,KEY                                                        
         BAS   RE,GETMKT           MARKET NAME                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SVKEY                                                        
         MVC   LISTAR+6(L'MKTNM),MKTNM                                          
         MVC   LISTAR+28(3),=C'ALL'       SCHEME                                
         OC    CLSKSCHM,CLSKSCHM                                                
         BZ    LR40                                                             
         GOTO1 CLUNPK,DMCB,CLSKSCHM,FULL                                        
         MVC   LISTAR+28(3),FULL                                                
*                                                                               
LR40     GOTO1 LISTMON                                                          
         B     LR10                                                             
*                                                                               
LRX      B     EXIT                                                             
         EJECT                                                                  
* PRINT REPORT                                                                  
*                                                                               
PR       LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HEDHOOK                                                       
         ST    R1,HEADHOOK                                                      
         LA    R2,P                                                             
         USING PLINED,R2                                                        
*                                                                               
         LA    R6,KEY                                                           
         USING CLSRECD,R6                                                       
         MVC   AIO,AIO2                                                         
         MVC   KEY,SVKEY                                                        
*                                                                               
PR05     GOTO1 HIGH                                                             
         B     PR20                                                             
*                                                                               
PR10     GOTO1 SEQ                                                              
*                                                                               
PR20     CLC   KEY(3),SVKEY        TEST SAME AGY/MED                            
         BNE   PRX                                                              
         OC    SVSCHEME,SVSCHEME   TEST SCHEME FILTER                           
         BZ    PR22                                                             
         LA    R6,KEY                                                           
         CLC   CLSKSCHM,SVBSCHEM                                                
         BE    PR22                                                             
         BH    *+14                                                             
         MVC   CLSKSCHM,SVBSCHEM   SKIP TO REQUEST SCHEME                       
         B     PR05                                                             
         MVI   CLSKSCHM,X'FF'      READ NEXT MARKET                             
         MVC   CLSKSCHM+1(CLSKCNTL-CLSKSCHM-1),CLSKSCHM                         
         B     PR05                                                             
*                                                                               
PR22     GOTO1 GETREC              GET RECORD                                   
         L     R6,AIO                                                           
         SR    RE,RE                                                            
         ICM   RE,3,CLSKMKT        MARKET                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PMKTNO,DUB                                                       
         MVC   QMKT,PMKTNO         MARKET NAME                                  
         MVC   SVKEY,KEY                                                        
         BAS   RE,GETMKT                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SVKEY                                                        
         MVC   PMKTNM,MKTNM                                                     
*                                                                               
         MVC   PSCHEME,=C'ALL'     SCHEME                                       
         OC    CLSKSCHM,CLSKSCHM                                                
         BZ    PR25                                                             
         GOTO1 CLUNPK,DMCB,CLSKSCHM,PSCHEME                                     
*                                                                               
PR25     LA    R0,12               STATIONS                                     
         LA    R3,PSTAS                                                         
         LA    R7,CLSSTA                                                        
         ZIC   R5,CLSELLN                                                       
         LA    R5,CLSEL(R5)                                                     
         BCTR  R5,0                                                             
         LA    R4,L'CLSSTA                                                      
*                                                                               
PR30     BAS   RE,PRTSTA                                                        
         LA    R3,8(R3)                                                         
         BCT   R0,PR40                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R0,12                                                            
         LA    R3,PSTAS                                                         
PR40     BXLE  R7,R4,PR30                                                       
         CLC   P,SPACES                                                         
         BNH   PR42                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PR42     LA    R7,CLSEL            FORMAT SPILL STATIONS AND MARKETS            
         SR    R0,R0                                                            
*                                                                               
PR44     CLI   0(R7),0                                                          
         BE    PR50                                                             
         CLI   0(R7),CSPELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     PR44                                                             
         USING CSPELD,R7                                                        
         LA    R4,L'CSPSTA                                                      
         ZIC   R5,CSPELLN                                                       
         AR    R5,R7                                                            
         BCTR  R5,0                                                             
         LA    R7,CSPSTA                                                        
         LA    RF,12                                                            
         LA    R3,PSTAS                                                         
*                                                                               
PR46     BAS   RE,PRTSTA                                                        
         LA    R3,8(R3)                                                         
         BCT   RF,PR48                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    RF,12                                                            
         LA    R3,PSTAS                                                         
PR48     BXLE  R7,R4,PR46                                                       
         CLC   P,SPACES                                                         
         BNH   PR50                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PR50     B     PR10                NEXT RECORD                                  
*                                                                               
PRX      B     EXIT                                                             
         SPACE 2                                                                
PRTSTA   DS    0H                                                               
         MVC   0(4,R3),0(R7)                                                    
         LR    R1,R3                                                            
         CLI   3(R1),C' '                                                       
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   4(R1),C'-'                                                       
         CLI   4(R7),C' '                                                       
         BNH   *+12                                                             
         CLI   4(R7),C'T'                                                       
         BNE   *+14                                                             
         MVC   5(2,R1),=C'TV'                                                   
         B     *+14                                                             
         MVC   5(1,R1),4(R7)                                                    
         MVI   6(R1),C'M'                                                       
         BR    RE                                                               
         EJECT                                                                  
* VALIDATE STATION ROUTINE                                                      
* OUTPUT : CC EQ - STATION IS VALID                                             
*          CC NE - STATION IS INVALID                                           
*                                                                               
VALSTA   NTR1                                                                   
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         MVI   ERROR,INVSTAT                                                    
         LA    R4,BLOCK                                                         
*                                                                               
         CLI   0(R4),3                                                          
         BL    VSNO                                                             
         CLI   0(R4),4                                                          
         BH    VSNO                                                             
         TM    2(R4),X'40'         TEST ALPHA                                   
         BZ    VSNO                                                             
         MVC   QSTA(4),12(R4)      SAVE CALL LETTERS                            
*                                                                               
         CLI   QMED,C'R'           TEST MEDIA IS RADIO                          
         BE    VS6                                                              
         SPACE 1                                                                
* MEDIA NOT RADIO *                                                             
         SPACE 1                                                                
         CLI   1(R4),0             TEST SUB-MEDIA ENTERED                       
         BNE   VS4                 YES                                          
         MVC   QSTA+4(1),QMED      ELSE SET SUB-MED = MEDIA                     
         B     VS10                                                             
VS4      MVC   QSTA+4(1),22(R4)    MOVE SUB-MEDIA                               
         CLI   1(R4),1                                                          
         BNE   VSNO                                                             
*                                                                               
         CLI   22(R4),C'L'         LOW POWERED STATION?                         
         BNE   *+12                                                             
         CLI   QMED,C'T'           YES, CHECK IF TV                             
         BE    VS10                                                             
*                                                                               
         CLC   QSTA+4(1),QMED      IF INPUT, MUST MATCH MEDIA CODE              
         BNE   VSNO                                                             
         B     VS10                                                             
         SPACE 1                                                                
* MEDIA = RADIO - AM OR FM IS REQUIRED *                                        
         SPACE 1                                                                
VS6      CLI   1(R4),2                                                          
         BH    VSNO                                                             
         MVC   QSTA+4(1),22(R4)                                                 
         ZIC   R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,STAAM                                                         
         BE    VS10                                                             
         EX    R5,STAFM                                                         
         BE    VS10                                                             
         B     VSNO                                                             
*                                                                               
STAAM    CLC   22(0,R4),=C'AM'                                                  
STAFM    CLC   22(0,R4),=C'FM'                                                  
         SPACE 1                                                                
* READ STATION MASTER RECORD *                                                  
         SPACE 1                                                                
VS10     DS    0H                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(5),QSTA                                                    
         MVC   KEY+7(2),AGENCY                                                  
****     MVC   KEY+9(3),QCLT       **** CLIENT NOT KNOWN                        
         L     R0,AIO                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BNE   VSNO                                                             
         L     R6,AIO                                                           
         ST    R0,AIO                                                           
         USING STARECD,R6                                                       
******** CLC   QMKT,SMKT           REMOVE BECAUSE THE NON-CLIENT                
******** BNE   VSNO                SPECIFIC STATION RECORD MAY NOT              
         B     VSYES               POINT TO THIS MARKET                         
*                                                                               
VSNO     LTR   RB,RB                                                            
         B     VSX                                                              
*                                                                               
VSYES    CR    RB,RB                                                            
*                                                                               
VSX      B     EXIT                                                             
         SPACE 2                                                                
TRAPERR  GOTO1 ERREX               NEVER TO RETURN                              
         EJECT                                                                  
* GET MARKET RECORD ROUTINE                                                     
* INPUT  : QMKT=MARKET                                                          
* OUTPUT : CC EQ  - FOUND                                                       
*          CC NE  - NOT FOUND                                                   
GETMKT   NTR1                                                                   
         L     R6,AIO3                                                          
         USING MKTRECD,R6                                                       
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
         L     R0,AIO                                                           
         MVC   AIO,AIO3                                                         
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
         ST    R0,AIO                                                           
         CLC   KEY(15),0(R6)                                                    
         BNE   GMNO                                                             
         MVC   MKTNM,MKTNAME       RETURN MARKET NAME TO USER                   
*                                                                               
GMYES    CR    RB,RB                                                            
         B     GMX                                                              
*                                                                               
GMNO     LTR   RB,RB                                                            
*                                                                               
GMX      B     EXIT                                                             
         EJECT                                                                  
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
CLRSCRN  NTR1                                                                   
         SR    RE,RE                                                            
*                                                                               
CS10     IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02' TEST EXTENDED HEADER                                 
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,CSCLC                                                         
         BE    CS20                                                             
         EX    RE,CSOC                                                          
         BZ    CS20                                                             
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS20     IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         CLI   0(R2),9                                                          
         BH    CS10                                                             
         B     CSX                                                              
*                                                                               
CSX      B     EXIT                                                             
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
* REPORT HEADLINE HOOK                                                          
*                                                                               
HEDHOOK  NTR1                                                                   
         MVC   H4+7(1),QMED                                                     
         MVC   H4+9(L'MEDNM),MEDNM                                              
         ICM   R4,15,ABOX                                                       
         BZ    HHX                                                              
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+59,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+26,C'C'                                                  
         MVI   BOXCOLS+130,C'R'                                                 
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVI   BOXOFF,0                                                         
         DROP  R4                                                               
*                                                                               
HHX      B     EXIT                                                             
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
         SSPEC H1,55,C'COMPETETIVE STATION LIST'                                
         SSPEC H2,55,C'------------------------'                                
         SSPEC H1,02,AGYNAME                                                    
         SSPEC H2,02,AGYADD                                                     
         SSPEC H1,96,REPORT                                                     
         SSPEC H1,108,RUN                                                       
         SSPEC H2,96,REQUESTOR                                                  
         SSPEC H2,120,PAGE                                                      
         SSPEC H4,02,C'MEDIA'                                                   
         SSPEC H7,02,C'MARKET'                                                  
         SSPEC H8,02,C'------'                                                  
         SSPEC H7,28,C'SCHEME'                                                  
         SSPEC H8,28,C'------'                                                  
         SSPEC H7,35,C'STATIONS'                                                
         SSPEC H8,35,C'--------'                                                
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PLINED   DSECT                     PRINT LINE                                   
         DS    C                                                                
PMKTNO   DS    CL4                                                              
         DS    C                                                                
PMKTNM   DS    CL20                                                             
         DS    C                                                                
PSCHEME  DS    CL3                                                              
         DS    CL4                                                              
PSTAS    DS    CL96                                                             
         EJECT                                                                  
       ++INCLUDE SPGENCLST                                                      
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMCAD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMDAD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPSFMWORKD                                                     
         ORG   SYSSPARE                                                         
SVSCHEME DS    CL3                                                              
SVBSCHEM DS    XL2                                                              
       ++INCLUDE SPRANSIDD                                                      
         SPACE 1                                                                
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SPSFM1A   08/11/11'                                      
         END                                                                    
