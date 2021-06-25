*          DATA SET SPLFM1B    AT LEVEL 029 AS OF 03/31/04                      
*PHASE T2191BA                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'SPLFM1B - BRAND SUB-ESTIMATE ADD/CHANGE    T2191B'              
         PRINT NOGEN                                                            
T2191B   CSECT                                                                  
         NMOD1 0,T2191B,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
* `                                                                             
         L     RC,0(R1)                                                         
         LA    R9,2048(RC)                                                      
         LA    R9,2048(R9)                                                      
         USING GENOLD,RC,R9                                                     
*                                                                               
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
*                                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING ESTHDRD,R8                                                       
*                                                                               
         MVC   WORK(3),SVKEY+1                                                  
         MVC   WORK+3(1),SVKEY+7                                                
         CLC   WORK(4),SVMSTR      SAME A-M/CLT/EST                             
         BE    *+8                 YES                                          
         MVI   SVFMTSW,0           NO- FORCE FMT                                
*                                                                               
         CLI   SVFMTSW,0           TEST FMT OR EDT                              
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
LFMERR   GOTO1 ERROR               (NO RETURN HERE)                             
         EJECT                                                                  
FMT      MVC   SVMSTR,SVKEY+1      A-M/CLT                                      
         MVC   SVMSTR+3(1),SVKEY+7 EST                                          
*                                                                               
         XC    KEY,KEY             READ MASTER EST                              
         MVC   KEY(8),SVKEY                                                     
         CLI   SVACT,C'A'                                                       
         BNE   FMT2                                                             
         MVC   KEY+4(3),=C'POL'                                                 
         MVI   ERRCD,NOPOLMAS                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   LFMERR                                                           
         MVI   ERRCD,NOTMSTR                                                    
         B     FMT4                                                             
*                                                                               
FMT2     MVI   ERRCD,NOTMSTR                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   LFMERR                                                           
FMT4     GOTO1 GETREC                                                           
         CLI   EMSTRIND,C'M'                                                    
         BNE   LFMERR                                                           
* FORMAT MASTER EST DATES                                                       
         GOTO1 VDATCON,DMCB,ESTART,(5,WORK)                                     
         GOTO1 (RF),(R1),EEND,(5,WORK+9)                                        
         MVI   WORK+8,C'-'                                                      
         LA    R2,SUBMDTSH                                                      
         MVC   37(17,R2),WORK                                                   
         FOUT  (R2)                                                             
* FORMAT MASTER EST DEMOS                                                       
*                                                                               
         XC    SUBDEMS,SUBDEMS                                                  
         XC    SUBDEM2,SUBDEM2                                                  
         OC    EDEMLST(3),EDEMLST    SEE IF I HAVE ANY DEMOS                    
         BZ    FMT8                                                             
         XC    ELEM,ELEM                                                        
         XC    REC2(200),REC2                                                   
         LA    R5,ELEM                                                          
         USING DBLOCK,R5                                                        
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'      CANADIAN AGY                                 
         BNE   FMT5                                                             
         CLI   SVCLEX,C'U'         US DEMOS                                     
         BE    FMT5                                                             
         MVI   DBSELMED,C'C'                                                    
FMT5     MVC   DMCB+4(4),=X'D9000AE0' DEMOCON                                   
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(14,EDEMLST),(2,REC2),(C'S',DBLOCK),EUSRNMS            
         LA    R5,SUBDEMS                                                       
         LA    R6,L'SUBDEMS(R5)                                                 
         LA    R7,REC2                                                          
FMT5D    CLI   0(R7),C' '                                                       
         BNH   FMT5F                                                            
         BAS   RE,FMTDEMO                                                       
         ZIC   R1,WORK                                                          
         LR    R0,R5                                                            
         AR    R0,R1                                                            
         CR    R0,R6                                                            
         BNH   FMT5E                                                            
         BCTR  R5,0                                                             
         CLI   0(R5),C','          BLANK OUT LAST COMMA                         
         BNE   *+8                                                              
         MVI   0(R5),C' '                                                       
         LA    R5,SUBDEM2                                                       
         LA    R6,L'SUBDEM2(R5)                                                 
FMT5E    BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK+1                                                   
         AR    R5,R1                                                            
         LA    R5,1(R5)                                                         
         CR    R5,R6                                                            
         BH    *+8                                                              
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         LA    R7,7(R7)            NEXT DEMO                                    
         B     FMT5D                                                            
*                                                                               
FMT5F    BCTR  R5,0                                                             
         CLI   0(R5),C','                                                       
         BNE   FMT8                                                             
         MVI   0(R5),C' '                                                       
FMT8     FOUT  SUBDEMSH                                                         
         FOUT  SUBDEM2H                                                         
* READ SUB-ESTIMATES                                                            
*                                                                               
         LA    R2,SUBEST1H                                                      
         MVI   KEY+7,1             RESET EST                                    
         GOTO1 HIGH                                                             
         B     FMT12                                                            
*                                                                               
FMT10    GOTO1 SEQ                                                              
*                                                                               
FMT12    CLC   KEY(7),KEYSAVE      SAME A/M/CLT/PRD                             
         BNE   FMTX                                                             
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   FMT10                                                            
* ESTHDR                                                                        
         CLC   KEY+7(1),SVKEY+7    IS IT US                                     
         BE    FMT10               YES - SKIP                                   
         GOTO1 GETREC                                                           
         CLI   EMSTRIND,C'S'       TEST SUB-EST                                 
         BNE   FMT10                                                            
         CLC   EMSTREST,SVKEY+7    RIGHT MASTER                                 
         BNE   FMT10                                                            
*                                                                               
         MVC   8(6,R2),=C'SUBEST'                                               
         ZIC   R0,EKEY+7                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  16(3,R2),DUB                                                     
*                                                                               
         GOTO1 VDATCON,DMCB,ESTART,(5,WORK)                                     
         GOTO1 (RF),(R1),EEND,(5,WORK+9)                                        
         MVI   WORK+8,C'-'                                                      
         MVC   22(17,R2),WORK                                                   
         FOUT  (R2)                                                             
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(20,R2),EDESC                                                   
         FOUT  (R2)                                                             
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             CHECK E-O-S                                  
         BNE   FMT10                                                            
         B     EXIT                                                             
         EJECT                                                                  
* CLEAR REMAINING FIELDS                                                        
*                                                                               
FMTX     ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,FMTOC                                                         
         BE    FMTX2                                                            
         EX    RE,FMTXC                                                         
         FOUT  (R2)                                                             
FMTX2    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BNE   FMTX                                                             
         B     EXIT                                                             
FMTOC    OC    8(0,R2),8(R2)                                                    
FMTXC    XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
EDT      DS    0H                                                               
         MVC   KEY,SVKEY           READ MASTER EST                              
         CLI   SVACT,C'A'                                                       
         BNE   EDT2                                                             
         MVC   KEY+4(3),=C'POL'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVI   ERRCD,NOTMSTR                                                    
         CLI   EMSTRIND,C'M'                                                    
         BNE   LFMERR                                                           
         B     EDT4                                                             
*                                                                               
EDT2     GOTO1 GETREC                                                           
         CLI   EMSTRIND,C'M'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,SUBDEMSH                                                      
* READ POL MASTER EST                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(8),SVKEY                                                     
         MVC   KEY+4(3),=C'POL'                                                 
         CLC   KEY(13),EKEY        DO WE HAVE POL MASTER ALREADY                
         BE    EDT4                                                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R8,REC2                                                          
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
* POL MASTER EST SHOULD BE IN REC 2                                             
EDT4     XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING DBLOCK,R5                                                        
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE,=C'TPT'                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'      CANADIAN AGY                                 
         BNE   EDT5                                                             
         CLI   SVCLEX,C'U'         US DEMOS                                     
         BE    EDT5                                                             
         MVI   DBSELMED,C'C'                                                    
EDT5     MVC   DMCB+4(4),=X'D9000AD9' DEMOVAL                                   
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(2,0(R2)),(14,WKDEMLST),(C'S',DBLOCK),        X        
               WKUSRNMS                                                         
         CLI   DMCB+4,0            DEMOVAL ERROR                                
         BE    LFMERR                                                           
         ZIC   R4,DMCB+4                                                        
         MH    R4,=H'3'                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),WKDEMLST                                                
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   INDEMLST(0),WKDEMLST SAVE DEMOS AS INPUT                         
         GOTO1 =V(XSORT),DMCB,(C'N',WORK2),14,3,3,0,RR=RELO                     
* CHECK FOR DUPS                                                                
         MVI   ERRCD,DEMINV                                                     
         LA    R4,WORK2                                                         
*                                                                               
EDT10    CLC   0(3,R4),=3X'00'                                                  
         BE    EDT12                                                            
         CLC   0(3,R4),3(R4)                                                    
         BE    LFMERR                                                           
         LA    R4,3(R4)                                                         
         B     EDT10                                                            
         EJECT                                                                  
EDT12    CLI   SVPRD,X'FF'         TEST CHANGING POL                            
         BNE   EDT14                                                            
         MVI   ERRCD,NOTSAME                                                    
         CLC   REC2+EDEMLST-ESTHDR(L'EDEMLST),INDEMLST                          
         BNE   LFMERR              DEMOS MUST MATCH                             
         CLC   REC2+EUSRNMS-ESTHDR(35),WKUSRNMS                                 
         BNE   LFMERR              USER NAMES AND WTD DEM MUST MATCH            
         B     EDT20                                                            
* BRAND DEMOS MUST BE SUBSET OF POL DEMOS                                       
EDT14    MVI   ERRCD,DEMNIPOL                                                   
         LA    R4,INDEMLST         USE UNSORTED BRAND LIST                      
EDT15    LA    R5,REC2+EDEMLST-ESTHDR       POL LIST                            
         LA    R6,DMAX                                                          
EDT16    CLC   0(3,R4),0(R5)                                                    
         BE    EDT18                                                            
         LA    R5,3(R5)                                                         
         BCT   R6,EDT16                                                         
         B     LFMERR              NOT IN POL                                   
EDT18    LA    R4,3(R4)                                                         
         OC    0(3,R4),0(R4)                                                    
         BNZ   EDT15                                                            
*                                  CHK USER DEMOS                               
         CLI   WKUSRNMS,C' '        SEE IF I HAVE ANY                           
         BNH   EDT20                                                            
         LA    R4,REC2+EUSRNMS-ESTHDR                                           
         LA    R3,5                FOR BCT                                      
EDT19    LA    R5,WKUSRNMS          HAS USER DEMOS                              
         LA    R6,5                USERDEMOS AND WTD DEM                        
EDT19B   CLC   0(7,R4),0(R5)                                                    
         BE    EDT19D                                                           
         LA    R5,7(R5)                                                         
         BCT   R6,EDT19B                                                        
         B     LFMERR                                                           
*                                                                               
EDT19D   LA    R4,7(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNH   EDT20                                                            
         BCT   R3,EDT19                                                         
*                                                                               
* DEMOS VALID - BUILD WEIGHT LIST FROM POL MASTER                               
*                                                                               
EDT20    XC    ELEM(20),ELEM                                                    
         CLI   WKUSRNMS+28,C' '      SHOULD POINT TO WTD DEM                    
         BNH   EDT26X                                                           
         LA    R4,INDEMLST          THIS IS UNSORTED BRAND LIST                 
         LA    R7,DMAX                                                          
         LA    R5,ELEM             BUILD WEIGHT LIST IN ELEM                    
         XC    0(20,R5),0(R5)                                                   
EDT22    LA    R0,DMAX                                                          
         LA    R1,REC2+EDEMLST-ESTHDR                                           
         LA    R6,REC2+EWGTLST-ESTHDR                                           
EDT24    CLC   0(3,R4),0(R1)                                                    
         BE    EDT26                                                            
         LA    R1,3(R1)                                                         
         LA    R6,1(R6)                                                         
         BCT   R0,EDT24                                                         
         DC    H'0'                                                             
EDT26    MVC   0(1,R5),0(R6)      STORE WEIGHT                                  
         LA    R5,1(R5)                                                         
         LA    R4,3(R4)                                                         
         OC    0(3,R4),0(R4)                                                    
         BZ    EDT26X                                                           
         BCT   R7,EDT22                                                         
EDT26X   DS    0H                                                               
         EJECT                                                                  
*                                  ELEM HAS WGT LIST                            
*                                  INDEMLST HAS EDEMLST                         
*                                  WKUSRNMS HAS USER DEMOS AND WTD DEM          
         MVC    WORK(20),ELEM                                                   
         XC    ELEM,ELEM                                                        
         MVC   ELEM(L'EDEMLST),INDEMLST                                         
         MVC   ELEM+L'EDEMLST(L'EWGTLST),WORK                                   
         MVC   ELEM+EUSRNMS-EDEMLST(L'EUSRNMS),WKUSRNMS                         
         MVC   ELEM+EWGTNM-EDEMLST(L'EWGTNM),WKUSRNMS+28                        
* UPDATE FILE                                                                   
         LA    R8,REC              RESTORE I/O ADDRESS                          
         ST    R8,AREC                                                          
**                                                                              
         LA    R2,SUBEST1H                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(7),SVKEY                                                     
         CLI   SVACT,C'A'                                                       
         BNE   *+10                                                             
         MVC   KEY+4(3),=C'POL'    FOR ADDS, READ POL ESTS                      
         MVI   KEY+7,1                                                          
         GOTO1 HIGH                                                             
         B     EDT32                                                            
*                                                                               
EDT30    MVC   KEY+8(5),=5X'FF'                                                 
         GOTO1 HIGH                                                             
*                                                                               
EDT32    CLC   KEY(7),KEYSAVE      A-M/CLT/PRD                                  
         BNE   EXIT                                                             
         OC    KEY+8(5),KEY+8                                                   
         BNZ   EDT30                                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         CLC   KEY+7(1),SVKEY+7    TEST FOR MASTER EST NUM                      
         BE    EDT34               YES                                          
         CLI   EMSTRIND,C'S'                                                    
         BNE   EDT30                                                            
         CLC   EMSTREST,SVKEY+7    TEST RIGHT MASTER                            
         BNE   EDT30                                                            
*                                                                               
EDT34    CLI   SVACT,C'A'                                                       
         BE    EDT40                                                            
* CHANGE                                                                        
         MVC   EDEMOS,ELEM                                                      
*                                                                               
         CLI   EMSTRIND,C'M'       TEST MASTER                                  
         BE    EDT36                                                            
         ZIC   R0,0(R2)            GET DESC FIELD IF INPUT                      
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BE    EDT36                                                            
         GOTO1 MOVE                                                             
         MVC   EDESC,WORK                                                       
*                                                                               
EDT36    GOTO1 PUTREC                                                           
         B     EDT50                                                            
         EJECT                                                                  
* ADD                                                                           
*                                                                               
EDT40    MVC   EKEY+4(3),SVKEY+4   SET PRD IN KEY                               
         MVC   EPRDCD+1(1),SVPRD   AND IN REC                                   
         OI    ECNTRL,X'01'        SET ON CONVERTED IND                         
*                                                                               
         CLI   EMSTRIND,C'M'       TEST MASTER                                  
         BE    EDT42               YES - USE MASTER DESC                        
         ZIC   R0,0(R2)            ELSE USE DESC IF INPUT                       
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BE    EDT42                                                            
         GOTO1 MOVE                                                             
         MVC   EDESC,WORK                                                       
*                                                                               
EDT42    MVC   EDEMOS,ELEM                                                      
*                                                                               
         ZAP   ECURPDN,=P'0'                                                    
*                                                                               
         LHI   R0,13                                                            
         LA    R1,EAUTH                                                         
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LHI   R0,26                                                            
         LA    R1,EORD                                                          
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LHI   R0,26                                                            
         LA    R1,EPAID                                                         
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
* MAKE SURE REC NOT ON FILE                                                     
* SUB-EST ADD WAS SUPPOSED TO GUARANTEE THIS                                    
*                                                                               
         MVC   WORK(20),KEY        SAVE KEY                                     
         MVC   KEY(13),EKEY                                                     
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EDT44                                                            
         TM    KEY+13,X'C0'       TEST IF CLOSED OUT                            
         BNO   *+6                                                              
         DC    H'0'                                                             
         TM    KEY+13,X'80'        TEST DELETED                                 
         BO    *+6                 IF YES, WRITE OVER IT                        
         DC    H'0'                ELSE DIE                                     
         LA    RE,REC2             READ DELETED EST TO DISCARD IT               
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         ST    R8,AREC             RESTORE I/O ADDRESS                          
         GOTO1 PUTREC                                                           
*                                                                               
         NI    KEY+13,X'3F'        UNSET DELETE IN DIRECTORY                    
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         B     EDT46                                                            
         EJECT                                                                  
EDT44    GOTO1 ADDREC                                                           
*                                                                               
EDT46    DS    0H                                                               
         MVC   KEY(20),WORK        RESTORE KEY                                  
EDT50    CLI   EMSTRIND,C'M'       SEE IF DOING MASTER EST                      
         BE    EDT30               YES DON'T BUMP TO NEXT FIELD                 
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     EDT30               NEXT SUB-EST                                 
         EJECT                                                                  
FMTDEMO  NTR1                      ROUTINE TO FORMAT DEMOS                      
*                                  R7 POINTS TO 7 CHAR DESC                     
*                                  WORK(1) RETURNS LENGHT                       
*                                  WORK+1 RETURNS DESC                          
         MVC   WORK(10),SPACES                                                  
         MVI   WORK,0                                                           
         CLI   0(R7),C' '                                                       
         BNH   FMTDEMOX                                                         
         LA    R1,7                                                             
         LA    R2,6(R7)        SCAN BACKWARDS FOR NON-SPACE                     
FMTD5    CLI   0(R2),C' '                                                       
         BNE   FMTD10                                                           
         BCTR  R2,0                                                             
         BCT   R1,FMTD5                                                         
FMTD10   STC   R1,WORK                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),0(R7)                                                  
         CLI   EUSRNMS,C' '        CHK FOR USER DEMOS                           
         BNH   FMTD20                                                           
         LA    R2,EUSRNMS                                                       
         LA    R3,4                FOR BCT                                      
FMTD12   CLC   WORK+1(7),0(R2)                                                  
         BNE   FMTD15                                                           
         MVC   WORK+10(7),WORK+1                                                
         MVC   WORK+1(2),=C'U/'                                                 
FMTD14   MVC   WORK+3(7),WORK+10                                                
         IC    R1,WORK                                                          
         AH    R1,=H'2'                                                         
         STC   R1,WORK                                                          
         B     FMTDEMOX                                                         
FMTD15   LA    R2,7(R2)                                                         
         BCT   R3,FMTD12                                                        
FMTD20   CLC   WORK+1(7),EWGTNM    SEE IF IT MATCHES WEIGHTED DEMO              
         BNE   FMTDEMOX                                                         
         MVC   WORK+10(7),WORK+1                                                
         MVC   WORK+1(2),=C'W/'                                                 
         B     FMTD14                                                           
FMTDEMOX XIT1                                                                   
DMAX     EQU   14                  MAXIMUM NUMBER OF DEMOS                      
         LTORG                                                                  
*SPLFMWRK                                                                       
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
       ++INCLUDE SPLFMFBD                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
*SPGENCLT                                                                       
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
*SPGENEST                                                                       
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
GENOLD   DSECT                                                                  
         ORG   DEMAREA                                                          
WKDEMLST DS    XL100                                                            
INDEMLST DS    XL100                                                            
WKUSRNMS DS    XL100                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SPLFM1B   03/31/04'                                      
         END                                                                    
