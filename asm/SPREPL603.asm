*          DATA SET SPREPL603  AT LEVEL 044 AS OF 09/15/05                      
*PHASE SPL603A                                                                  
         TITLE 'SPL603 - SPL6 SUB-CONTROLLER'                                   
SPL603   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPL603                                                         
*                                                                               
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         USING OFFICED,OFCBLK                                                   
         SPACE 2                                                                
* READ PROFILE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0L6'                                                 
         MVC   WORK+4(2),QAGY                                                   
         MVC   WORK+6(1),QMED                                                   
         XC    WORK+7(3),WORK+7                                                 
         CLC   =C'ALL',QCLT                                                     
         BE    CP5                                                              
         MVC   WORK+7(3),QCLT                                                   
CP5      GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         MVI   RCSUBPRG,2          DEFAULT IS PAGE BY CLIENT                    
         CLC   SVPROF,=16X'00'                                                  
         BE    CP10                                                             
         MVI   RCSUBPRG,1                                                       
         CLI   SVPROF,C'M'         PAGE SKIP BY AGENCY/MEDIA                    
         BE    CP10                                                             
         MVI   RCSUBPRG,2                                                       
         CLI   SVPROF,C'C'                      CLIENT                          
         BE    CP10                                                             
         MVI   RCSUBPRG,3                                                       
         CLI   SVPROF,C'P'                      PRODUCT                         
         BE    CP10                                                             
*                                                                               
CP5ERR   MVC   P(36),=C'** ERROR - INVALID PAGING PROFILE **'                   
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
         EJECT                                                                  
CP10     MVI   ALLBYTE,0                                                        
         CLI   QOPT5,C'*'          TURNAROUND                                   
         BNE   CP12                TURNAROUNDS GO BY PAGING LEVEL               
         OI    ALLBYTE,X'02'       ALL ESTIMATES                                
         CLI   RCSUBPRG,3                                                       
         BE    CP15                                                             
         OI    ALLBYTE,X'06'       ALL PRODUCTS AND ESTIMATES                   
         CLI   RCSUBPRG,2                                                       
         BE    CP15                                                             
         OI    ALLBYTE,X'0E'       ALL CLIENTS PRODUCTS AND ESTIMATES           
         CLI   RCSUBPRG,1                                                       
         BE    CP20                                                             
CP12     CLI   QOPT1,C' '                                                       
         BNE   *+8                                                              
         OI    ALLBYTE,X'80'       ALL COMMENTS                                 
*                                                                               
CP15     CLC   =C'ALL',QCLT                                                     
         BNE   *+12                                                             
         OI    ALLBYTE,X'0E'       ALL CLIENTS (PRODUCTS AND ESTIMATES)         
         B     CP20                                                             
         CLI   QPGR,C' '                                                        
         BE    *+12                                                             
         OI    ALLBYTE,X'02'                                                    
         B     CP20                                                             
         CLC   QPRD,SPACES                                                      
         BNE   *+10                                                             
         MVC   QPRD,=C'ALL'                                                     
         CLC   =C'ALL',QPRD                                                     
         BNE   *+8                                                              
         OI    ALLBYTE,X'04'       ALL PRODUCTS                                 
         CLC   QEST,SPACES                                                      
         BNE   *+10                                                             
         MVC   QEST,=C'ALL'                                                     
         CLC   =C'ALL',QEST                                                     
         BNE   CP20                                                             
         OI    ALLBYTE,X'02'       ALL ESTIMATES                                
         EJECT                                                                  
CP20     XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D0C'                                                  
         MVC   KEY+2(1),SVAGYMD                                                 
         TM    ALLBYTE,X'80'       ALL COMMENT TYPES                            
         BO    CP25                                                             
         MVC   KEY+3(1),QOPT1                                                   
         B     CP50                                                             
*                                                                               
CP25     DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BNE   CPEXIT                                                           
*                                                                               
CP50     MVI   MODE,REQFRST                                                     
         TM    ALLBYTE,X'08'                                                    
         BO    CP55                                                             
         CLI   QCLT,C'*'           AGENCY LEVEL COMMENT                         
         BNE   *+14                                                             
         MVC   KEY+4(2),QCLT                                                    
         B     CP90                                                             
         GOTO1 CLPACK,DMCB,QCLT,KEY+4                                           
         BAS   RE,RDCLT                                                         
         B     CP60                                                             
*                                                                               
CP55     XC    KEY+4(9),KEY+4                                                   
         B     CP90                                                             
*                                                                               
CP60     TM    ALLBYTE,X'04'                                                    
         BO    CP65                                                             
         CLI   QPGR,C' '                                                        
         BE    CP60A                                                            
         MVC   KEY+6(1),QPGR                                                    
         LA    R0,3                NUMERIC CHECK                                
         LA    R1,QPRD                                                          
CP600    CLI   0(R1),C'0'                                                       
         BL    CP601                                                            
         CLI   0(R1),C'9'                                                       
         BH    CP62ERR                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,CP600                                                         
         B     CP602                                                            
CP601    CLI   0(R1),C' '                                                       
         BNE   CP62ERR                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,CP601                                                         
CP602    PACK  WORK(2),QPRD        PRODUCT GROUP BREAK                          
         NI    WORK+1,X'F0'                                                     
         MVC   KEY+7(2),WORK                                                    
         B     CP62A                                                            
CP60A    L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         LA    RE,CLIST                                                         
CP61     CLI   0(RE),C'A'                                                       
         BL    CP62ERR                                                          
         CLC   0(3,RE),QPRD                                                     
         BE    CP62                                                             
         LA    RE,4(RE)                                                         
         B     CP61                                                             
CP62     MVC   KEY+8(1),3(RE)                                                   
CP62A    BAS   RE,RDPRD                                                         
         B     CP70                                                             
*                                                                               
CP62ERR  MVC   P(34),=C'** ERROR - INVALID PRODUCT CODE **'                     
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
CP65     GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE      A-M/TYPE/CLT                                 
         BE    CP70                                                             
         TM    ALLBYTE,X'80'       TEST ALL COMMENT TYPES                       
         BZ    CPEXIT              NO-DONE                                      
         CLC   KEY(4),KEYSAVE      SAME A-M/TYPE ?                              
         BE    CP100               YES-ADVANCE TO NEXT TYPE                     
         CLC   KEY(3),KEYSAVE      SAME AGYMED?                                 
         BNE   CPEXIT              NO-DONE                                      
         B     CP50                YES-PROCESS THIS COMMENT TYPE                
*                                                                               
CP70     TM    ALLBYTE,X'02'                                                    
         BO    CP75                                                             
         LA    R0,3                                                             
         LA    R1,QEST                                                          
CP72     CLI   0(R1),C'0'                                                       
         BL    CP72ERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    CP72ERR                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,CP72                                                          
         PACK  DUB,QEST(3)                                                      
         CVB   R0,DUB                                                           
         STC   R0,KEY+9                                                         
         BAS   RE,RDEST                                                         
         B     CP90                                                             
*                                                                               
CP72ERR  MVC   P(37),=C'** ERROR - INVALID ESTIMATE NUMBER **'                  
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
CP75     XC    KEY+9(4),KEY+9                                                   
         B     CP90                                                             
         EJECT                                                                  
CP90     GOTO1 HIGH                                                             
CP91     CLC   KEY(3),KEYSAVE      COMMENTS - THIS A/M                          
         BNE   CPEXIT                                                           
         CLC   KEY(4),KEYSAVE      COMMENT TYPE                                 
         BE    CP94                                                             
         TM    ALLBYTE,X'80'                                                    
         BZ    CPEXIT                                                           
         B     CP25                                                             
*                                                                               
CP94     CLC   KEY(6),KEYSAVE      CLIENT                                       
         BE    CP96                                                             
         TM    ALLBYTE,X'08'                                                    
         BO    CP110                                                            
         TM    ALLBYTE,X'80'                                                    
         BZ    CPEXIT                                                           
         B     CP100                                                            
*                                                                               
CP96     CLC   KEY(9),KEYSAVE      PRODUCT                                      
         BE    CP98                                                             
         TM    ALLBYTE,X'04'                                                    
         BO    CP120                                                            
         TM    ALLBYTE,X'80'                                                    
         BZ    CPEXIT                                                           
         B     CP100                                                            
*                                                                               
CP98     CLC   KEY(10),KEYSAVE      ESTIMATE                                    
         BE    CP99                                                             
         TM    ALLBYTE,X'02'                                                    
         BO    CP130                                                            
         TM    ALLBYTE,X'04'       ALL PRODUCTS                                 
         BO    CP101                                                            
         TM    ALLBYTE,X'80'                                                    
         BZ    CPEXIT                                                           
         B     CP100                                                            
*                                                                               
CP99     B     CP132               CHECK FOR MARKET/STATION                     
*                                                                               
* NEXT COMMENT TYPE                                                             
CP100    ZIC   RE,KEY+3                                                         
         LA    RE,1(RE)                                                         
         STC   RE,KEY+3                                                         
         XC    KEY+4(9),KEY+4                                                   
         B     CP25                                                             
*                                                                               
* NEXT PRODUCT                                                                  
CP101    ZIC   RE,KEY+9                                                         
         LA    RE,1(RE)                                                         
         STC   RE,KEY+9                                                         
         B     CP65                                                             
         EJECT                                                                  
*                                                                               
CP102    CLI   MODE,PROCREC                                                     
         BE    CP104                                                            
         MVC   SVBUYKEY,KEY                                                     
         CLI   MODE,REQFRST                                                     
         BE    HEADLINE                                                         
         CLI   MODE,CLTFRST                                                     
         BNE   HD01                                                             
         CLI   RCSUBPRG,2          CLIENT LEVEL                                 
         BL    CP103                                                            
         B     HEADLINE                                                         
HD01     CLI   MODE,PRDFRST                                                     
         BNE   HD02                                                             
         CLI   RCSUBPRG,3          PRODUCT LEVEL                                
         BL    CP103                                                            
         B     HEADLINE                                                         
HD02     CLI   MODE,ESTFRST                                                     
         BNE   CP103                                                            
         CLI   RCSUBPRG,4                                                       
         BL    CP103                                                            
*                                                                               
HEADLINE MVI   FORCEHED,C'Y'                                                    
*                                                                               
CP103    GOTO1 GO                                                               
         MVC   KEY,SVBUYKEY                                                     
CP104    GOTO1 HIGH                                                             
         MVC   AREC,ADBUY                                                       
         GOTO1 GET                                                              
         MVI   MODE,PROCREC                                                     
         MVC   SVBUYKEY,KEY                                                     
         GOTO1 GO                                                               
         MVC   KEY,SVBUYKEY                                                     
         GOTO1 SEQ                 READ SEQ                                     
         B     CP91                AND CHECK                                    
*                                                                               
*                                                                               
CP110    CLI   MODE,REQFRST                                                     
         BE    CP111                                                            
         MVI   MODE,CLTFRST                                                     
CP111    OC    KEY+4(2),KEY+4                                                   
         BZ    CP102                                                            
         BAS   RE,RDCLT                                                         
         B     CP121                                                            
*                                                                               
CP120    CLI   MODE,REQFRST                                                     
         BE    CP121                                                            
         MVI   MODE,PRDFRST                                                     
CP121    OC    KEY+6(3),KEY+6                                                   
         BZ    CP131                                                            
         BAS   RE,RDPRD                                                         
         B     CP131                                                            
*                                                                               
CP130    CLI   MODE,REQFRST                                                     
         BNE   *+12                                                             
         BAS   RE,RDPRD                                                         
         B     CP131                                                            
         MVI   MODE,PROCREC                                                     
CP131    OC    KEY+9(1),KEY+9                                                   
         BZ    CP132                                                            
         BAS   RE,RDEST                                                         
*                                                                               
CP132    OC    KEY+10(3),KEY+10    TEST STATION OR MARKET                       
         BZ    CP102                                                            
         CLI   KEY+10,0            YES                                          
         BNE   *+12                                                             
         BAS   RE,RDMKT            READ MARKET                                  
         B     CP102                                                            
         BAS   RE,RDSTA            READ STATION                                 
         B     CP102                                                            
         SPACE 2                                                                
CPEXIT   XIT1                                                                   
         EJECT                                                                  
RDCLT    NTR1                                                                   
         MVC   SVBUYKEY,KEY                                                     
         MVC   CLT,SPACES                                                       
         MVC   PRD,SPACES                                                       
         MVC   PGR1,SPACES                                                      
         MVC   EST,SPACES                                                       
         OC    SVBUYKEY+4(2),SVBUYKEY+4                                         
         BZ    RDCLTX              NO CLIENT - DON'T READ                       
         CLI   SVBUYKEY+4,C'*'                                                  
         BNE   RDCLT1                                                           
         MVC   CLT(2),SVBUYKEY+4                                                
         MVC   CLTNM,=CL24'OFFICE *'                                            
**NOP    MVC   CLTNM+8(1),SVBUYKEY+5                                            
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVBUYKEY+5                                                
         L     RF,ADCONLST                                                      
         L     RF,VOFFICER-SPADCONS(RF)                                         
         GOTO1 (RF),DMCB,(C'2',OFFICED),(0,ACOMFACS)                            
         MVC   CLTNM+8(2),OFCOFC2                                               
         B     RDCLTX                                                           
RDCLT1   XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVBUYKEY+4 PKD CLIENT                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RDCLT2                                                           
         MVC   CLT,=C'***'                                                      
         MVC   CLTNM,SPACES                                                     
         B     RDCLTX                                                           
RDCLT2   GOTO1 GETCLT                                                           
RDCLTX   MVC   KEY,SVBUYKEY                                                     
         B     CPEXIT                                                           
         SPACE 2                                                                
RDPRD    NTR1                                                                   
         MVC   SVBUYKEY,KEY                                                     
         MVC   PRD,SPACES                                                       
         MVC   PGR1,SPACES                                                      
         MVC   EST,SPACES                                                       
         CLC   SVBUYKEY+6(3),=X'000000'                                         
         BE    RDPRDX              PRODUCT NOT PRESENT                          
         CLI   SVBUYKEY+6,0                                                     
         BE    RDPRD1                                                           
         MVC   PGR1,=C'**** '                                                   
         MVC   PGR1NM,SPACES                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(1),SVAGYMD                                                 
         MVC   KEY+3(2),SVBUYKEY+4                                              
         MVC   KEY+5(1),SVBUYKEY+6                                              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   RDPRDX                                                           
         MVC   AREC,ADBUY                                                       
         L     R6,AREC                                                          
         GOTO1 GET                                                              
         USING PRGEL01,R6                                                       
         LA    R6,24(R6)                                                        
         MVC   PGR1LEN,PRGBK1LN                                                 
         ZIC   RE,PRGBK1LN         BREAK 1                                      
         DROP  R6                                                               
         MVC   WORK(1),SVBUYKEY+6                                               
         MVC   HALF,SVBUYKEY+7                                                  
         OI    HALF+1,X'0F'                                                     
         UNPK  WORK+1(3),HALF                                                   
         MVC   PGR1,SPACES                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PGR1(0),WORK        ** EXECUTED **                               
*                                                                               
* VERIFY BREAK 1 EXISTS                                                         
         MVC   KEY+5(3),SVBUYKEY+6                                              
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BNE   RDPRDX                                                           
         ZIC   RE,PGR1LEN                                                       
         BCTR  RE,0                                                             
         UNPK  DUB,KEY+6(2)                                                     
         UNPK  WORK(8),SVBUYKEY+7(2)                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   DUB+4(0),WORK+4     ** EXECUTED **                               
         BNE   RDPRDX                                                           
         MVC   AREC,ADBUY                                                       
         L     R6,AREC                                                          
         GOTO1 GET                                                              
         USING PRGEL10,R6                                                       
         LA    R6,24(R6)                                                        
         MVC   PGR1NM,PRGNAM1                                                   
         DROP  R6                                                               
         B     RDPRDX                                                           
RDPRD1   CLC   CLT,=C'***'         IF CLIENT INVALID                            
         BE    RDPRDERR            CANNOT READ PRODUCT                          
         L     R6,ADCLT            PRODUCT                                      
         USING CLTHDRD,R6                                                       
         MVC   KEY(13),0(R6)                                                    
         LA    RE,CLIST                                                         
RDPRD2   CLI   0(RE),C'A'          LOOK UP PRD CODE                             
         BL    RDPRDERR                                                         
         CLC   3(1,RE),SVBUYKEY+8                                               
         BE    RDPRD4                                                           
         LA    RE,4(RE)                                                         
         B     RDPRD2                                                           
RDPRD4   MVC   KEY+4(3),0(RE)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+8                                                              
         B     RDPRDERR                                                         
         GOTO1 GETPRD                                                           
         B     RDPRDX                                                           
RDPRDERR MVC   PRD(3),=C'***'                                                   
         MVC   PRDNM,SPACES                                                     
         DROP  R6                                                               
RDPRDX   MVC   KEY,SVBUYKEY                                                     
         B     CPEXIT                                                           
         SPACE 2                                                                
RDEST    NTR1                                                                   
         MVC   SVBUYKEY,KEY                                                     
         MVC   EST,SPACES                                                       
         CLI   SVBUYKEY+9,0                                                     
         BE    RDESTX              NO EST - DON'T READ                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVBUYKEY+4                                              
         MVC   KEY+4(3),PRD                                                     
         MVC   KEY+7(1),SVBUYKEY+9                                              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RDEST1                                                           
         MVC   EST,=C'***'                                                      
         MVC   ESTNM,SPACES                                                     
         B     RDESTX                                                           
RDEST1   GOTO1 GETEST                                                           
*                                                                               
RDESTX   MVC   KEY,SVBUYKEY                                                     
         B     CPEXIT                                                           
         EJECT                                                                  
RDMKT    NTR1                                                                   
         MVC   MKT,=C'****'                                                     
         MVC   MKTNM,SPACES                                                     
         XC    BMKT,BMKT                                                        
         MVC   SVBUYKEY,KEY                                                     
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         SR    RE,RE                                                            
         ICM   RE,3,SVBUYKEY+11                                                 
         BZ    RDMKTX                                                           
         STCM  RE,3,BMKT                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKT,DUB                                                          
         MVC   KEY+2(4),MKT                                                     
         MVC   KEY+6(2),AGY                                                     
         GOTO1 READMKT                                                          
*                                                                               
RDMKTX   MVC   KEY,SVBUYKEY                                                     
         B     CPEXIT                                                           
         SPACE 2                                                                
RDSTA    NTR1                                                                   
         MVC   STA,=C'*****'                                                    
         MVC   STAPRINT,SPACES                                                  
         XC    BSTA,BSTA                                                        
         MVC   SVBUYKEY,KEY                                                     
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         ICM   RE,7,SVBUYKEY+10                                                 
         BZ    RDSTAX                                                           
         XC    BMKTSTA,BMKTSTA                                                  
         STCM  RE,7,BSTA                                                        
         GOTO1 MSUNPK,DMCB,BMKTSTA,MKT,STA                                      
         MVC   KEY+2(5),STA                                                     
         CLI   KEY+6,C' '                                                       
         BH    *+10                                                             
         MVC   KEY+6(1),QMED                                                    
         MVC   KEY+7(2),AGY                                                     
         MVC   KEY+9(3),CLT                                                     
         GOTO1 READSTA                                                          
*                                                                               
RDSTAX   MVC   KEY,SVBUYKEY                                                     
         B     CPEXIT                                                           
         SPACE 2                                                                
ALLBYTE  DC    X'00'               "ALL" STATUS INDICATOR                       
SVPROF   DS    CL16                                                             
OFCBLK   DS    XL(OFCLENQ)                                                      
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PGRDSECT DSECT                                                                  
       ++INCLUDE SPGENPRG                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE DDOFFICED                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044SPREPL603 09/15/05'                                      
         END                                                                    
