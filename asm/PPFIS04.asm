*          DATA SET PPFIS04    AT LEVEL 090 AS OF 03/28/07                      
*PHASE T41B04A                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'PPFIS04 - PRINTPAK FIS REC PROCESS'                             
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
*   BOBY  01/07   ADD PLANNED COSTS OPTION                                      
*                                                                               
*   SMYE  04/06   COSMETIC STEWARD ESTIMATE CHANGES                             
*                                                                               
*   BOBY 10/05    NEW 2 CHARACTER OFFICE CODES                                  
*                                                                               
*   SMYE 5/02     NEW LIMIT ACCESS SECURITY (INCLUDING TRAFFIC OFFICE)          
*                                                                               
*   BPLA 5/95       CHANGES FOR DATASW AND PREVDSW                              
*                                                                               
*   BPLA 1/95       CHECK SVMAXIO IN GETNCLT                                    
*                                                                               
*   BPLA 3/1/94     CHANGES FOR BILLING GROUPS                                  
*                                                                               
*   BPLA 11/5/93    CHANGES FOR PROCESSING OFFICE LIST (SVCLT=$N)               
*                                                                               
T41B04   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41B04,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T41BFFD,RA                                                       
         XC    FISHD1,FISHD1                                                    
         FOUT  FISHD1H                                                          
         XC    FISHD2,FISHD2                                                    
         LA    R4,FISHD2                                                        
         LA    R5,9                FOR BCT                                      
HEAD5    MVC   0(7,R4),=C'PRD/EST'                                              
         LA    R4,9(R4)                                                         
         BCT   R5,HEAD5                                                         
         CLC   SVCLT,=C'ALL'                                                    
         BE    HEAD10                                                           
         CLI   SVCLT,C'*'          OFFICE                                       
         BE    HEAD10                                                           
         CLI   SVCLT,C'&&'         BILLING GROUP                                
         BE    HEAD10                                                           
         CLI   SVCLT,C'$'          OFFICE LIST                                  
         BE    HEAD10                                                           
         B     HEAD20                                                           
*                                                                               
HEAD10   XC    FISHD2,FISHD2                                                    
         LA    R4,FISHD2                                                        
         LA    R5,6                                                             
HEAD15   MVC   0(11,R4),=C'CLT/PRD/EST'                                         
         LA    R4,13(R4)                                                        
         BCT   R5,HEAD15                                                        
HEAD20   FOUT  FISHD2H                                                          
*                                                                               
SCLEAR   LA    R4,14                                                            
         LA    R3,FISOT01H                                                      
SCLEAR5  XC    8(80,R3),8(R3)                                                   
         FOUT  (R3)                                                             
         LA    R3,LINLEN(R3)                                                    
         BCT   R4,SCLEAR5                                                       
*                                                                               
         MVI   DATASW,0                                                         
*                                                                               
         OC    PREVKEY,PREVKEY     SEE IF CONTINUATION                          
         BZ    RCLEAR                                                           
         MVC   DATASW,PREVDSW    USE PREVIOUS DATASW                            
*                                                                               
         CLI   MAXIOERR,C'Y'       WAS CAUSED BY MAXIO ERROR                    
         BE    RBUYS          IF SO - DON'T CLEAR CLT/PRD/EST TABLE             
*                                                                               
RCLEAR   LA    R3,CPETAB                                                        
*****                                                                           
         LA    R4,5                                                             
RCLEAR5  XC    0(250,R3),0(R3)                                                  
         LA    R3,250(R3)                                                       
         BCT   R4,RCLEAR5                                                       
*                                                                               
         EJECT                                                                  
RBUYS    DS    0H                  DOING ONE PUB - SO READ BUYS                 
         MVI   MAXIOERR,C'N'                                                    
         OC    PREVKEY,PREVKEY     SEE IF CONTINUATION                          
         BNZ   RB05X                                                            
*                                                                               
RB0      MVC   WORKCLT,SVCLT                                                    
         CLI   SVCLT,C'*'          CHK FOR OFFICE                               
         BE    RB03                                                             
         CLI   SVCLT,C'&&'         CHK FOR BILLING GROUP                        
         BE    RB03                                                             
         CLI   SVCLT,C'$'          CHK FOR OFFICE LIST                          
         BE    RB03                                                             
         CLC   SVCLT,=C'ALL'                                                    
         BNE   RB05                                                             
RB03     XC    WORKCLT,WORKCLT                                                  
         BAS   RE,GETNCLT          GET FIRST CLIENT                             
         OC    WORKCLT,WORKCLT                                                  
         BZ    RBXX                NONE FOUND                                   
         CLC   WORKCLT(3),=X'FFFFFF'   MEANS GETNCLT REACHED MAX I/O'S          
         BE    RBXX                                                             
*                                                                               
RB05     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'21'         USE 21 POINTERS                              
         MVC   KEY+4(3),WORKCLT                                                 
         MVC   DUB(6),SVPUB                                                     
         CLI   SVPUB+4,X'FF'       SEE IF DOING ALL ZONES/EDTS                  
         BNE   *+10                                                             
         XC    DUB+4(2),DUB+4                                                   
         MVC   KEY+7(6),DUB                                                     
RB05X    LA    R7,12               FOR KEY COMP                                 
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
*                                                                               
RB1      DS    0H                                                               
         CLI   SVPUB+4,X'FF'       CHK FOR ALL ZONES/EDTS                       
         BNE   *+8                                                              
         BCTR  R7,0                COMPARE ONLY 8 DIGIT PUB                     
         BCTR  R7,0                                                             
         OC    PREVKEY,PREVKEY     SEE IF CONTINUATION                          
         BZ    RB2                                                              
         MVC   KEY,PREVKEY                                                      
         MVC   DATASW,PREVDSW                                                   
         MVC   WORKCLT,KEY+4      MUST RESET MY WORKCLT                         
*                                 SINCE IT WAS CLOBBERED AT RB0                 
         XC    PREVKEY,PREVKEY                                                  
RB2      BAS   RE,HIGH                                                          
         B     RB4                                                              
RB3      BAS   RE,SEQ                                                           
RB4      DS    0H                                                               
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         CLC   FATIOCNT,SVMAXIO                                                 
         BL    RB4X       MAX IO'S REACHED TREAT LIKE SCREEN FULL               
         MVI   MAXIOERR,C'Y'                                                    
         B     RB8CXX                                                           
*                                                                               
         DROP  R1                                                               
*                                                                               
RB4X     EX    R7,KEYCOMP                                                       
         BNE   RBX                 END OF PUB                                   
*                                                                               
*                                                                               
RB5      CLC   SVPRD,=C'ALL'                                                    
         BE    RB6                                                              
         CLC   KEY+13(3),SVPRD                                                  
         BE    RB6                                                              
         B     RB3                                                              
*                                                                               
RB6      DS    0H                                                               
         CLI   MTHSW,C'B'          SEE IF DOING BILLABLE DATES                  
         BE    RB7                                                              
*                                                                               
         CLC   KEY+16(2),SVSTRTB   CHK DATES                                    
         BNL   RB6C                                                             
         CLI   PRSW,1              SEE IF DOING PRIOR                           
         BE    RB7                                                              
         MVC   KEY+16(3),SVSTRTB   READ HIGH FOR THIS DATE                      
         XC    KEY+19(2),KEY+19    CLEAR EST                                    
         B     RB2                                                              
*                                                                               
RB6C     CLC   KEY+16(2),SVENDB                                                 
         BNH   RB7                                                              
         CLI   SUBSW,1             SEE IF DOING SUBSEQUENT                      
         BE    RB7                                                              
         MVC   KEY+16(3),=3X'FF'   BUMP TO NEXT PRD                             
         B     RB2                                                              
*                                                                               
RB7      CLC   SVEST,=C'ALL'                                                    
         BE    RB8                                                              
         OC    SVESTB,SVESTB          SEE IF DOING ONE EST                      
         BZ    RB8                                                              
         CLC   KEY+19(2),SVESTB                                                 
         BE    RB8                                                              
         B     RB3                 MUST USE SEQ READ                            
*                                                                               
KEYCOMP  CLC   KEYSAVE(0),KEY      EXECUTED                                     
*                                                                               
RB8      OC    KEY+21(3),KEY+21                                                 
         BNZ   RB3                 SKIP PASSIVE BUYS                            
         TM    KEY+25,X'C0'        SEE IF CLOSED OUT OR FF DELETE               
         BO    RB3                 YES - SKIP                                   
*                                                                               
RB8A     DS    0H                                                               
         CLI   MTHSW,C'B'         SEE IF DOING BILLABLE MONTHS                  
         BNE   RB8A4                                                            
         BAS   RE,GETREC                                                        
         CLC   PBDBDATE(2),SVSTRTB                                              
         BNL   RB8A1                                                            
         CLI   PRSW,1             SEE IF DOING PRIOR                            
         BE    RB8A1                                                            
         B     RB3                SKIP                                          
*                                                                               
RB8A1    CLC   PBDBDATE(2),SVENDB                                               
         BNH   RB8A4                                                            
         CLI   SUBSW,1            SEE IF DOING SUBSEQUENT                       
         BE    RB8A4                                                            
         B     RB3                SKIP                                          
*                                                                               
RB8A4    DS    0H                                                               
         MVI   DATASW,X'01'       SET DATA FOUND                                
*                                  FIND/STORE IN LIST OF CLT/PRD/EST            
         MVC   WORK(3),KEY+4       CLIENT                                       
         MVC   WORK+3(3),KEY+13      PRD                                        
         MVC   WORK+6(2),KEY+19      ESTIMATE                                   
         MVI   WORK+8,0                                                         
RB8A5    DS    0H                                                               
RB8A7    LA    R6,CPETAB           SAVE CLT/PRD/EST                             
*****                                                                           
         LA    R8,84               MAX FOR CLT/PRD/EST                          
         CLC   SVCLT,=C'ALL'                                                    
         BE    RB8C                                                             
         CLI   SVCLT,C'*'          OFFICE                                       
         BE    RB8C                                                             
         CLI   SVCLT,C'&&'         BILLING GROUP                                
         BE    RB8C                                                             
         CLI   SVCLT,C'$'          OFFICE LIST                                  
         BE    RB8C                                                             
         LA    R8,126              MAX FOR PRD/EST                              
RB8C     CLI   0(R6),0             OPEN SLOT                                    
         BE    RB8E                                                             
         CLC   WORK(8),0(R6)                                                    
         BE    RB8E5               ALREADY IN TABLE                             
         LA    R6,9(R6)                                                         
         BCT   R8,RB8C             MASTER BCT                                   
*                                                                               
*        OUT OF ROOM IN TABLE                                                   
*                                                                               
RB8CXX   MVC   PREVKEY,KEY                                                      
         MVC   PREVDSW,DATASW                                                   
         MVC   WORKCLT,KEY+4       SAVE THIS CLIENT                             
         B     RBXX                                                             
*                                                                               
RB8E     DS    0H                  READ EST TO GET 'TEST' STATUS                
         MVC   ESAVKEY,KEY                                                      
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         MVC   KEY+7(3),KEY+13                                                  
         MVC   KEY+10(2),KEY+19                                                 
         MVI   KEY+3,X'07'                                                      
         XC    KEY+12(20),KEY+12                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                EST MUST BE ON FILE                          
         BAS   RE,GETREC                                                        
         LA    R0,REC              RESET AREC TO REC                            
         ST    R0,AREC                                                          
         MVC   KEY,ESAVKEY                                                      
         BAS   RE,HIGH                                                          
         TM    PESTTEST,X'80'      SE IF TEST ESTIMATE                          
         BZ    RB8E3                                                            
         MVC   WORK+8(1),PESTTEST                                               
*                                                                               
RB8E3    MVC   0(9,R6),WORK        CLIENT/PRD/EST (+PESTTEST)                   
RB8E5    B     RB3                 GO DO SEQ READ                               
RBX      DS    0H                                                               
         MVI   DMINBTS,X'C0'       RESET DMINBTS                                
         CLI   SVCLT,C'*'          CHK FOR OFFICE                               
         BE    RBX5                                                             
         CLI   SVCLT,C'&&'         CHK FOR BILLING GROUP                        
         BE    RBX5                                                             
         CLI   SVCLT,C'$'          CHK FOR OFFICE LIST                          
         BE    RBX5                                                             
         CLC   SVCLT,=C'ALL'       SEE IF DOING ALL CLTS                        
         BNE   RBXX                                                             
RBX5     BAS   RE,GETNCLT                                                       
         CLC   WORKCLT,=X'FFFFFF'   MEANS GETNCLT REACHED MAX I/O'S             
         BE    RBXX                                                             
*                                                                               
         XC    PREVKEY,PREVKEY     CLEAR PREVKEY                                
         OC    WORKCLT,WORKCLT                                                  
         BNZ   RB05                SEE IF END OF CLIENTS- NO                    
         B     RBXX                YES THEN DONE                                
*                                  WRITE BUCKETS TO TWA                         
RBXX     DS    0H                                                               
         EJECT                                                                  
REND     MVI   DMINBTS,X'C0'      RESET DMINBTS                                 
*****                                                                           
         LA    R6,CPETAB                                                        
         CLI   0(R6),0                                                          
*****                                                                           
         BNZ   FORMAT                                                           
         B     FRMTEND                                                          
*                                                                               
FORMAT   XC    DMWORK(40),DMWORK                                                
         LA    R6,CPETAB                                                        
*****                                                                           
         LA    R7,0                USED TO COUNT ENTRIES                        
FORMAT1  CLI   0(R6),0                                                          
         BE    FORMAT2                                                          
         MVI   DATASW,X'01'       THIS IS PROBABLY REDUNDANT                    
         LA    R6,9(R6)                                                         
         LA    R7,1(R7)                                                         
         B     FORMAT1                                                          
*                                                                               
*              SORT CLT/PRD/EST OR PRD/EST LIST                                 
*                                                                               
*****                                                                           
FORMAT2  LA    R6,CPETAB                                                        
         GOTO1 =V(XSORT),DMCB,(0,(R6)),(R7),9,9,0,RR=RELO                       
*                                                                               
*                                                                               
FORMAT3  CLC   SVCLT,=C'ALL'                                                    
         BE    FORMAT10                                                         
         CLI   SVCLT,C'*'            OFFICE                                     
         BE    FORMAT10                                                         
         CLI   SVCLT,C'&&'           BILLING GROUP                              
         BE    FORMAT10                                                         
         CLI   SVCLT,C'$'            OFFICE LIST                                
         BE    FORMAT10                                                         
*****                                                                           
         LA    R6,CPETAB                                                        
         GOTO1 =A(FMTALPH),DMCB,(9,(R6)),(R7),14,(9,DMWORK),RR=RELO             
*                                                                               
         LA    R2,FISOT01H                                                      
FORMAT3C LA    R6,DMWORK                                                        
         LA    RF,8(R2)                                                         
         CLI   0(R6),0                                                          
         BE    FRMTEND                                                          
FORMAT4  CLI   0(R6),0                                                          
         BE    FRMTSEND                                                         
         L     R7,0(R6)                                                         
         MVC   0(3,RF),3(R7)       PRODUCT                                      
         MVI   3(RF),C'/'                                                       
         MVC   HALF,6(R7)                                                       
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,RF),DUB                                                      
         TM    8(R7),X'80'         SEE IF TEST ESTIMATE                         
         BZ    *+8                                                              
         MVI   7(RF),C'T'                                                       
         TM    8(R7),X'40'         SEE IF STEWARD ESTIMATE                      
         BZ    *+8                                                              
         MVI   7(RF),C'S'                                                       
*                                                                               
         SR    RE,RE                                                            
         IC    RE,0(R6)                                                         
         BCTR  RE,0                                                             
         L     R5,0(R6)                                                         
         LA    R5,9(R5)                                                         
         ST    R5,0(R6)                                                         
         STC   RE,0(R6)                                                         
         LA    R6,4(R6)                                                         
         LA    RF,9(RF)                                                         
         B     FORMAT4                                                          
*                                                                               
FRMTSEND FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     FORMAT3C                                                         
*                                                                               
FORMAT10 DS    0H                                                               
*****                                                                           
         LA    R6,CPETAB                                                        
         GOTO1 =A(FMTALPH),DMCB,(9,(R6)),(R7),14,(6,DMWORK),RR=RB               
*                                                                               
         LA    R2,FISOT01H                                                      
FORMAT13 LA    R6,DMWORK                                                        
         LA    RF,8(R2)                                                         
         CLI   0(R6),0                                                          
         BE    FRMTEND                                                          
FORMAT14 CLI   0(R6),0                                                          
         BE    FORMAT18                                                         
         L     R7,0(R6)                                                         
         MVC   0(3,RF),0(R7)       CLIENT                                       
         MVI   3(RF),C'/'                                                       
         MVC   4(3,RF),3(R7)       PRODUCT                                      
         MVI   7(RF),C'/'                                                       
         MVC   HALF,6(R7)                                                       
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(3,RF),DUB                                                      
*                                                                               
         TM    8(R7),X'80'         SEE IF TEST ESTIMATE                         
         BZ    *+8                                                              
         MVI   11(RF),C'T'                                                      
         TM    8(R7),X'40'         SEE IF STEWARD ESTIMATE                      
         BZ    *+8                                                              
         MVI   11(RF),C'S'                                                      
*                                                                               
         SR    RE,RE                                                            
         IC    RE,0(R6)                                                         
         BCTR  RE,0                                                             
         L     R5,0(R6)                                                         
         LA    R5,9(R5)                                                         
         ST    R5,0(R6)                                                         
         STC   RE,0(R6)                                                         
         LA    R6,4(R6)                                                         
         LA    RF,13(RF)                                                        
         B     FORMAT14                                                         
*                                                                               
FORMAT18 FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     FORMAT13                                                         
*                                                                               
FRMTEND  DS    0H                                                               
         LA    R2,FISMDIAH                                                      
         MVC   FISEMSG,=CL60'** NO DATA FOUND **'                               
         CLI   DATASW,0                                                         
         BE    MODEXIT                                                          
         MVC   FISEMSG,=CL60'** END OF REQUESTED DATA **'                       
         OC    PREVKEY,PREVKEY                                                  
         BZ    MODEXIT                                                          
         MVC   PREVDSW,DATASW     SAVE DATASW AS WELL                           
         LA    R2,FISEND1H                                                      
         OI    FISOPTH+1,X'01'     MODIFY OPTIONS FIELD                         
         FOUT  FISOPTH                                                          
         MVC   FISEMSG,=CL60'** HIT ENTER FOR NEXT PAGE **'                     
         CLI   MAXIOERR,C'Y'                                                    
         BNE   *+10                                                             
         MVC   FISEMSG,=CL60'** HIT ENTER TO CONTINUE PROCESSING **'            
MODEXIT  OI    6(R2),X'C0'         CURSOR                                       
         FOUT  FISEMSGH                                                         
         XMOD1                                                                  
         EJECT                                                                  
*                                  ROUTINE TO GET NEXT CLIENT                   
GETNCLT  NTR1                                                                   
*                                                                               
         MVC   PREVKEY,KEY     NEED TO DO FOR MAX IO CHECK                      
         MVC   PREVDSW,DATASW                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'02'                                                      
GETNC1   MVC   KEY+4(3),WORKCLT                                                 
         MVI   KEY+7,X'FF'                                                      
         BAS   RE,HIGH                                                          
*                                                                               
         XC    WORKCLT,WORKCLT     CLEAR WORKCLT                                
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         CLC   FATIOCNT,SVMAXIO                                                 
         BL    GETNC2     MAX IO'S REACHED TREAT MUST STOP                      
         MVC   WORKCLT(3),=X'FFFFFF'                                            
         MVI   MAXIOERR,C'Y'                                                    
         B     GETNCX                                                           
         DROP  R1                                                               
*                                                                               
GETNC2   DS    0H                                                               
         XC    PREVKEY,PREVKEY     MUST CLEAR PREVKEY HERE                      
*                                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   GETNCX                                                           
*          DATA SET PPFIS01    AT LEVEL 106 AS OF 06/03/02                      
*                                                                               
         CLI   SVCLT,C'$'          SEE IF DOING OFFICE LIST                     
         BE    GETNC9                                                           
*                                                                               
*                                                                               
         CLI   SVCLT,C'&&'         SEE IF DOING BILLING GROUP                   
         BE    GETNC10                                                          
*                                                                               
         CLI   SVCLT,C'*'          SEE IF DOING OFFICE                          
         BNE   GETNC20                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,CKTRA            TRAFFIC OFFICE REPLACEMENT RTN               
         CLI   SVCLT+1,C'-'          SEE IF DOING ALL BUT                       
         BNE   GETNC3                                                           
         CLC   PCLTOFF(1),SVCLT+2                                               
         BE    GETNC8              BYPASS THIS CLIENT                           
         B     GETNC20                                                          
*                                                                               
GETNC3   CLI   SVCLT+2,0           SEE IF DOING RANGE                           
         BE    GETNC5              NO                                           
*                                                                               
*        TRANSLATE PCLTOFF TO 2 CH OFFICE CODE                                  
*                                                                               
         XC    WORK2,WORK2        WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK2           (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF      CLT OR CLT TRAFFIC OFFICE CODE               
         MVC   OFCCLT,PCLTKCLT                                                  
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT,6(RA)                                                     
         LA    R0,SECBLK                                                        
         ST    R0,OFCSECD          A("SECRET BLOCK")                            
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',WORK2),ACOMFACS                              
         CLI   0(R1),0                                                          
         BNE   GETNC8              SKIP CLIENT IF INVALID OFFICE CODE           
*                                                                               
         LA    R1,WORK2            ESTABLISH OFFICER CONTROL BLOCK              
         USING OFFICED,R1                                                       
*                                                                               
         CLC   OFCOFC2,SVOFCST2    MAKE SURE OFFICE IN RANGE                    
         BL    GETNC8              LOW SKIP                                     
         CLC   OFCOFC2,SVOFCEN2                                                 
         BH    GETNC8              HIGH SKIP                                    
*                                                                               
         B     GETNC20             PROCESS                                      
*                                                                               
         DROP  R1                                                               
*                                                                               
GETNC5   CLC   PCLTOFF(1),SVCLT+1  ONE OFFICE - MUST MATCH                      
         BNE   GETNC8              NO SKIP                                      
         B     GETNC20                                                          
*                                                                               
GETNC8   MVC   WORKCLT,KEY+4                                                    
         B     GETNC1              GO CHK NEXT CLIENT                           
*                                                                               
GETNC9   DS    0H                  HERE FOR OFFICE LIST CHECKING                
         BAS   RE,GETREC           MUST READ CLT                                
         BAS   RE,CKTRA            TRAFFIC OFFICE REPLACEMENT RTN               
         MVI   LIMITSW,C' '        SET TO USE OFFICER TO "SELECT"               
         BAS   RE,PPCLIVER         CALL OFFICER                                 
         BE    GETNC20             CHECK FOR SECURITY                           
         B     GETNC8              SKIP THIS CLT                                
*                                                                               
GETNC10  DS    0H                  BILLING GROUP (&) TESTING                    
         BAS   RE,GETREC                                                        
         CLC   PCLTBLGP,SVCLT+1                                                 
         BNE   GETNC8              SKIP THIS CLT                                
*NOP*    B     GETNC90             ACCEPT THIS CLT                              
*                                                                               
GETNC20  DS    0H                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    GETNC90             NO LIMIT ACCESS                              
*                                                                               
         MVI   LIMITSW,C'Y'        SET TO USE OFFICER FOR SECURITY              
         BAS   RE,PPCLIVER         CALL OFFICER                                 
         BNE   GETNC8              SKIP THIS CLT                                
*                                                                               
GETNC90  MVC   WORKCLT,KEY+4       PROCESS THIS CLIENT                          
GETNCX   XIT1                                                                   
         EJECT                                                                  
*                                                                               
CKTRA    NTR1                                                                   
         CLI   TRFAGSW,C'Y'        TRAFFIC AGENCY ID ?                          
         BNE   CKTRAX              NO                                           
*                                  SEE IF TRAFFIC OFFICE EXISTS                 
         MVI   ELCODE,X'50'                                                     
         LA    R2,PCLTREC+33                                                    
         BAS   RE,NEXTEL                                                        
         BNE   CKTRAX                                                           
         MVC   PCLTOFF,2(R2)    REPLACE CLT OFFICE WITH TRAFFIC OFFICE          
CKTRAX   XIT1                                                                   
         SPACE 3                                                                
NEXTEL   DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  SET CONDITION CODE NE                        
*                                                                               
STOP     DS    0H                                                               
         XC    FISEMSG,FISEMSG                                                  
         MVC   FISEMSG(49),=C'**MAXIMUM FILE READS EXCEEDED - REQUEST SX        
               TOPPED**'                                                        
         FOUT  FISEMSGH                                                         
         B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
*       *************************                                               
******  TEST OFFICE LIST SECURITY  ******                                       
*       *************************                                               
*                                                                               
*        ONLY GO TO THIS ROUTINE WHEN SVCLT IS $N - OFFICE LIST                 
*                                                                               
         SPACE 2                                                                
*NOP*PPCLIVER NTR1 ***** NOTE- I/O AREA M/B IN AREC ******                      
*NOP*    XC    WORK,WORK                                                        
*NOP*    LA    R1,WORK                                                          
*NOP*    USING OFFICED,R1                                                       
*NOP*    MVI   OFCSYS,C'P'                                                      
*NOP*    MVC   OFCAUTH,SVCLT   SVCLT SHOULD HAVE $N                             
*NOP*    MVC   OFCAGY,AGYALPHA                                                  
*NOP*    MVC   OFCOFC,PCLTOFF                                                   
*NOP*    DROP  R1                                                               
*NOP*    GOTO1 VOFFICER,DMCB,WORK,ACOMFACS                                      
*NOP*    CLI   0(R1),0                                                          
*NOP*    XIT1                                                                   
*******************************************************                         
         EJECT                                                                  
*                                                                               
PPCLIVER NTR1                   *****  LIMIT ACCESS TESTING   *****             
         XC    WORK2,WORK2        WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK2           (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         CLI   LIMITSW,C'Y'        DOING SECURITY ?                             
         BE    *+10                YES                                          
         MVC   OFCAUTH,SVCLT       MUST BE "SELECTING" FROM OFC LIST            
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF      CLT OR CLT TRAFFIC OFFICE CODE               
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),6(RA)                                                  
         CLI   LIMITSW,C'Y'        DOING SECURITY ?                             
         BE    *+10                YES                                          
         MVC   OFCLMT(2),SVCLT     MUST BE "SELECTING" FROM OFC LIST            
         LA    R0,SECBLK                                                        
         ST    R0,OFCSECD          A("SECRET BLOCK")                            
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'N',WORK2),ACOMFACS                              
         CLI   0(R1),0                                                          
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
BUYSTOP  DS    0H                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE PPGENEROL                                                      
         LTORG                                                                  
         EJECT                                                                  
FMTALPH  CSECT                                                                  
         NMOD1 0,FRMTALPH                                                       
*                                                                               
* FORMAT TABLE IN ALPHABETIC COLUMNS                                            
*                                                                               
*        PARAMETER 1     BYTE   0 = L'EACH ENTRY                                
*                        BYTE 1-3 = A(TABLE)                                    
*        PARAMETER 2     BYTE 0-3 = NUMBER OF ENTRIES IN TABLE                  
*        PARAMETER 3     BYTE 0-3 = NUMBER OF ROWS TO BE FORMATTED              
*                                                                               
*        PARAMETER 4     BYTE   0 = NUMBER OF COLUMNS TO BE FORMATTED           
*                        BYTE 1-3 = A(FORMAT LIST)                              
*                                                                               
*        OUTPUT FORMAT LIST (ONE ENTRY FOR EACH COLUMN)                         
*                BYTE  0 = NUMBER OF ENTRIES FOR THIS COLUMN                    
*                BYTE 1-3= A(START FIELD FOR THIS COLUMN)                       
*                                                                               
*               A(TABLE) IS SET TO NEXT ENTRY POINT                             
*               NUMBER OF ENTRIES IS SET TO NUMBER OF ENTRIES LEFT IN           
*                TABLE.                                                         
*                                                                               
         L     R6,12(R1)           CLEAR FORMAT LIST                            
         LA    R6,0(R6)                                                         
         SR    R7,R7                                                            
         IC    R7,12(R1)                                                        
         XC    0(4,R6),0(R6)                                                    
         LA    R6,4(R6)                                                         
         BCT   R7,*-10                                                          
*                                                                               
         SR    RF,RF               GET END OF TABLE                             
         IC    RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         MH    RF,6(R1)            NUMBER OF ENTRIES X ENTRY LENGTH             
         L     R6,0(R1)                                                         
         LA    RF,0(RF,R6)         END OF TABLE                                 
*                                                                               
         LA    R6,0(R6)            A(TABLE START)                               
         L     R7,12(R1)           A(FORMAT LIST)                               
         SR    R8,R8                                                            
         IC    R8,12(R1)           NUMBER OF COLUMNS                            
FA1      ST    R6,0(R7)            SAVE COLUMN ADDRESS                          
         SR    R4,R4                                                            
         CLC   4(4,R1),8(R1)       NUMBER OF ENTRIES GT NO OF ROWS              
         BH    FA2                                                              
         MVC   0(1,R7),7(R1)        NO - SET ROWS FOR THIS COLUMN               
         XC    4(4,R1),4(R1)       SET ENTRIES LEFT TO ZERO                     
         LR    R6,RF                                                            
         B     FAEX                                                             
FA2      MVC   0(1,R7),11(R1)      SET TO NUMBER OF ROWS                        
         SR    R9,R9                                                            
         IC    R9,0(R1)                                                         
         MH    R9,10(R1)           X ENTRY LENGTH                               
         LA    R6,0(R9,R6)         SET TO NEXT COLUMN ADDRESS                   
         LA    R7,4(R7)                                                         
         L     R9,4(R1)            DECREMENT NUMBER OF ENTRIES                  
         S     R9,8(R1)                                                         
         ST    R9,4(R1)            SAVE ENTRIES LEFT                            
         BCT   R8,FA1              GET NEXT COLUMN ADDRESS                      
*                                                                               
FAEX     IC    RF,0(R1)            SAVE A(NEXT START)                           
         ST    R6,0(R1)                                                         
         STC   RF,0(R1)                                                         
         B     FMTALPX                                                          
*                                                                               
FMTALPX  XMOD1 1                                                                
         LTORG                                                                  
       ++INCLUDE PPFISWRK                                                       
