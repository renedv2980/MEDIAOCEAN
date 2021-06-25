*          DATA SET SPINF11    AT LEVEL 005 AS OF 03/06/06                      
*PHASE T21A11A,+0,NOAUTO                                                        
         TITLE 'T21A11 - SPOT INFO CLIENT ROUTINES'                             
T21A11   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 40,T21A11                                                        
         LR    R3,RC                                                            
         USING CLTWRKD,R3                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         LA    RE,REC2                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         MVI   ERRCD,SCRTYERR                                                   
         OC    T21AFFD+6(2),T21AFFD+6                                           
         BZ    CD                                                               
         GOTO1 ERROR                                                            
*                                                                               
CD       XC    WORK,WORK                                                        
         MVI   SVSUBSC,0                                                        
         MVI   NMGFLTR,0                                                        
         GOTO1 USER1,DUB,(64,SINIFLT),(4,=C'NMG=')                              
         ICM   RF,15,4(R1)                                                      
         BZ    CD00                                                             
         MVC   NMGFLTR,4(RF)                                                    
         CLI   NMGFLTR,C'Y'                                                     
         BE    CD00                                                             
         CLI   NMGFLTR,C'N'                                                     
         BE    CD00                                                             
         LR    R6,RF               POINT TO INPUT                               
         LA    RE,5                SET A STUPID LENGTH                          
         B     FLTERR                                                           
*                                                                               
CD00     DS    0H                                                               
         MVI   J1FLTR,0                                                         
         GOTO1 USER1,DUB,(64,SINIFLT),(3,=C'J1=')                               
         ICM   RF,15,4(R1)                                                      
         BZ    CD10                                                             
         MVC   J1FLTR,3(RF)                                                     
         CLI   J1FLTR,C'Y'                                                      
         BE    CD10                                                             
         CLI   J1FLTR,C'N'                                                      
         BE    CD10                                                             
         LR    R6,RF               POINT TO INPUT                               
         LA    RE,4                SET A STUPID LENGTH                          
         B     FLTERR                                                           
*                                                                               
CD10     GOTO1 USER1,DUB,(64,SINIFLT),(5,=C'DATA=')    GETFLTR                  
         ICM   R6,15,4(R1)                                                      
         BNZ   PROFRTN                                                          
         USING FLDHDRD,R2                                                       
*                                                                               
*        CLIENT DEFAULT ROUTINES                                                
         MVI   CDFRST,1                                                         
         LA    R7,REC2                                                          
         XC    FULL,FULL                                                        
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING CLTHDRD,R5                                                       
         MVC   CKEYAM,SVAGYMD                                                   
         MVC   CKEYCLT,SVCLT                                                    
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                  FIRST TIME THROUGH                         
         MVC   KEY(13),PREVKEY       NO                                         
         XC    PREVKEY,PREVKEY                                                  
CDHIGH   GOTO1 HIGH                                                             
         B     CDHAVREC                                                         
CDSEQ    GOTO1 SEQ                                                              
*                                                                               
CDHAVREC LA    R5,KEY                                                           
         CLC   SVAGYMD,CKEYAM                                                   
         BNE   CDEND                                                            
         CLI   CKEYCLT+2,0           IS IT CLIENT HEADER RECORD                 
         BE    *+14                  YES                                        
         MVC   CKEYCLT+2(9),HIKEY    NO, SO MOVE HI TO REST OF KEY              
         B     CDHIGH                TO SKIP OVER OTHER RECORDS.                
         MVI   CDFRST,0              SET READ SWITCH ON                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R5,AREC                                                          
         CLI   NMGFLTR,0           TEST NEW MG FILTER                           
         BE    CDHAV10                                                          
         MVI   BYTE,C'Y'                                                        
         TM    COPT1,COP1NMG                                                    
         BO    *+8                                                              
         MVI   BYTE,C'N'                                                        
         CLC   NMGFLTR,BYTE                                                     
         BNE   CDSEQ                                                            
*                                                                               
CDHAV10  DS    0H                                                               
         CLI   J1FLTR,0            TEST J1 FILTR                                
         BE    CDHAV20                                                          
         MVI   BYTE,C'N'                                                        
         TM    COPT2,COP2EXJ1                                                   
         BO    *+8                                                              
         MVI   BYTE,C'Y'                                                        
         CLC   J1FLTR,BYTE                                                      
         BNE   CDSEQ                                                            
*                                                                               
CDHAV20  XC    0(24,R7),0(R7)      MOVE DATA TO TABLE                           
         MVC   DMCB+4(4),=X'D9000A15'   GET CLUNPK FROM CORE-                   
         GOTO1 VCALLOV,DMCB,0           RESIDENT PHASE.                         
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(CPROF+6,CKEYCLT),(R7)  UNPK 2C CLIENT 3C              
         MVI   4(R7),C'/'                                                       
         MVC   5(19,R7),CNAME                                                   
         LA    R7,24(R7)           INCREMENT LOAD TABLE                         
         L     RE,FULL             COUNT TABLE ENTRIES                          
         LA    RE,1(RE)                                                         
         ST    RE,FULL                                                          
         C     RE,=F'42'           IS TABLE FULL                                
         BL    CDSEQ               NO                                           
         MVC   PREVKEY,KEY         YES                                          
*                                                                               
CDEND    CLI   CDFRST,0            READ-RECORD SWITCH ON                        
         BE    CDEND2              YES                                          
         MVI   ERRCD,NOFNDERR      NO                                           
         GOTO1 ERROR                                                            
         B     MODEXIT                                                          
CDEND2   L     R9,FULL                                                          
* FORMAT ALPHA SCREEN                                                           
         GOTO1 USER2,DMCB,(24,REC2),(R9),14,(3,DMWORK)   FRMTALPH               
         LA    R2,SINHDRH            USER2 RETURNS 3 FULL WRDS IN               
         LA    R7,DMWORK             DMWORK(1 FOR EACH DISPLAY COL)             
         LA    R8,3                  WITH BEGINNING ADDRS OF OUTPUT             
         LA    RE,FLDDATA+1          FOR THAT COL. 1ST BYTE HAS NO              
         LA    RF,LINLEN(RE)         OF ELEMENTS IN THAT COL.                   
* MOVE IN HEADERS                                                               
CDEND3   CLI   0(R7),0                                                          
         BE    CDSEND                                                           
         MVC   0(16,RE),=C'CLIENT CODE/NAME'                                    
         MVC   0(16,RF),=C'----------------'                                    
         LA    RE,26(RE)                                                        
         LA    RF,26(RF)                                                        
         LA    R7,4(R7)                                                         
         BCT   R8,CDEND3                                                        
CDSEND   FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
         EJECT                                                                  
CDSEND2  LA    RE,FLDDATA+1        DISPLAY LOOP.                                
         LA    R7,DMWORK           ADDRESSES OF OUTPUT TO R7                    
         LA    R8,3                COL LIMIT TO R8                              
         CLI   0(R7),0             DONE - EXIT                                  
         BE    MODEXIT                                                          
CDSEND3  CLI   0(R7),0             LINE DONE                                    
         BE    CDSEND4              YES - SEND IT                               
         L     RF,0(R7)            ADDRS OF OUTPUT TO RF                        
         MVC   0(24,RE),0(RF)      MOVE OUTPUT TO SCREEN                        
         TM    SVSTEREO,SVSTEREO_FUL  ONLY FOR FULL STEREO                      
         BNO   CDSEND3A                                                         
         CHI   R8,1                                                             
         BE    *+8                 NO SEP AFTER COL 3                           
         MVI   25(RE),C'º'         FIELD SEPARATOR (VERTICAL BAR)               
CDSEND3A SR    RF,RF                                                            
         IC    RF,0(R7)            NO OF ELEMENTS TO RF                         
         BCTR  RF,0                   "      "      "                           
         L     R6,0(R7)            ADDRESS OF OUTPUT TO R6-                     
         LA    R6,24(R6)           INCREMENT TO NEXT OUTPUT-                    
         ST    R6,0(R7)            STORE NEW ADDRS IN DMWORK-                   
         STC   RF,0(R7)            NEW NO OF ELEM TO 1ST DMOWRK BYTE.           
         LA    R7,4(R7)            INCREMENT DMWORK TO NEXT ADDRS               
         LA    RE,26(RE)           INCREMENT SCREEN LINE TO NEXT COL            
         BCT   R8,CDSEND3                                                       
*                                                                               
CDSEND4  FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     CDSEND2                                                          
         EJECT                                                                  
*                                                                               
* CLIENT PROFILE DISPLAY SCREEN ROUTINE                                         
*                                                                               
PROFRTN  CLC   5(4,R6),=C'PROF'                                                 
         BNE   PRODRTN             NOT PROFILE - TRY PRODUCT                    
*                                                                               
* GET PROFILE FILTERS                                                           
PF1      MVI   SVSUBSC,1                                                        
         LA    R5,SINIFLT                                                       
         XC    CPROFLT,CPROFLT                                                  
         MVI   CDFRST,1                                                         
POS1     GOTO1 USER1,DMCB,(64,(R5)),(4,=C'CHAR')                                
         XC    WORK,WORK                                                        
         MVC   WORK(07),=C'CHARNN='                                             
         OC    4(4,R1),4(R1)                                                    
         BNZ   POS1A                                                            
         MVC   WORK(7),=C'BYTENN='                                              
         GOTO1 USER1,DMCB,(64,(R5)),(4,=C'BYTE')                                
         OC    4(4,R1),4(R1)                                                    
         BZ    POSEX                                                            
POS1A    L     R5,4(R1)            SET FOR NEXT SCAN                            
         LR    R6,R5               POINT TO FIELD                               
         LA    RE,8                SET FIELD LENGTH FOR ERROR                   
         LA    R5,1(R5)                                                         
         MVC   HALF,=C'00'                                                      
         CLI   5(R6),C'='                                                       
         BNE   *+14                                                             
         MVC   HALF+1(1),4(R6)                                                  
         B     POS2                                                             
         CLI   6(R6),C'='                                                       
         BNE   FLTERR              ERROR                                        
         MVC   HALF,4(R6)                                                       
*                                                                               
POS2     CLI   HALF,C'0'           EDIT FIELD NUMBER                            
         BL    FLTERR                                                           
         CLI   HALF,C'9'                                                        
         BH    FLTERR                                                           
         CLI   HALF+1,C'0'                                                      
         BL    FLTERR                                                           
         CLI   HALF+1,C'9'                                                      
         BH    FLTERR                                                           
         PACK  DUB,HALF                                                         
         CVB   R9,DUB                                                           
         LTR   R9,R9                                                            
         BZ    FLTERR                                                           
         CHI   R9,15                                                            
         BH    FLTERR                                                           
         BCTR  R9,0                                                             
         LA    R9,CPROFLT(R9)                                                   
POS3     CLI   0(R6),C'='                                                       
         BE    *+12                                                             
         LA    R6,1(R6)                                                         
         B     POS3                                                             
         LA    R6,1(R6)                                                         
POS4     MVC   0(1,R9),0(R6)                                                    
         LA    R9,1(R9)                                                         
         LA    R6,1(R6)                                                         
         CLI   0(R6),0                                                          
         BE    POSEX                                                            
         CLI   0(R6),C','                                                       
         BE    POS1                                                             
         B     POS4                                                             
         EJECT                                                                  
*                                                                               
* FILTERS HAVE BEEN SET NOW PROCESS RECORDS                                     
*                                                                               
POSEX    XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING CLTHDRD,R5                                                       
         MVC   CKEYAM,SVAGYMD                                                   
         MVC   CKEYCLT,SVCLT                                                    
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY,PREVKEY                                                      
         XC    PREVKEY,PREVKEY                                                  
         LA    R4,14               SET MAX LINES                                
PFHIGH   GOTO1 HIGH                                                             
         B     PFHAVREC                                                         
PFSEQ    GOTO1 SEQ                                                              
*                                                                               
PFHAVREC LA    R5,KEY                                                           
         CLC   SVAGYMD,CKEYAM                                                   
         BNE   PFEND                                                            
         CLI   CKEYCLT+2,0                                                      
         BE    *+14                                                             
         MVC   CKEYCLT+2(9),HIKEY                                               
         B     PFHIGH                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R5,AREC                                                          
         CLI   NMGFLTR,0           TEST NEW MG FILTER                           
         BE    PFHAV10                                                          
         MVI   BYTE,C'Y'                                                        
         TM    COPT1,COP1NMG                                                    
         BO    *+8                                                              
         MVI   BYTE,C'N'                                                        
         CLC   NMGFLTR,BYTE                                                     
         BNE   PFSEQ                                                            
*                                                                               
PFHAV10  DS    0H                                                               
         CLI   J1FLTR,0           TEST J1 FILTER                                
         BE    PFHAV20                                                          
         MVI   BYTE,C'N'                                                        
         TM    COPT2,COP2EXJ1                                                   
         BO    *+8                                                              
         MVI   BYTE,C'Y'                                                        
         CLC   J1FLTR,BYTE                                                      
         BNE   PFSEQ                                                            
*                                                                               
PFHAV20  OC    CPROFLT,CPROFLT                                                  
         BZ    PFSEND                                                           
*                                                                               
         LA    RF,CPROF                                                         
         LA    RE,CPROFLT                                                       
         LA    R9,15                                                            
PFFLTR   CLI   0(RE),0                                                          
         BE    PFFLTR1                                                          
         CLC   0(1,RF),0(RE)                                                    
         BNE   PFSEQ                                                            
PFFLTR1  LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R9,PFFLTR                                                        
         EJECT                                                                  
*                                                                               
* RECORD HAS PASSED ALL FILTERS SO DISPLAY IT                                   
*                                                                               
PFSEND   CLI   CDFRST,1                                                         
         BNE   PFSEND2                                                          
         LA    R2,SINHDRH                                                       
         MVC   FLDDATA+1(16),=C'CLIENT CODE/NAME'                               
         MVC   FLDDATA+40(14),=C'CLIENT PROFILE'                                
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(16),=C'----------------'                               
         MVC   FLDDATA+40(14),FLDDATA+1                                         
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
         MVI   CDFRST,0                                                         
*                                                                               
PFSEND2  MVC   DMCB+4(4),=X'D9000A15'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         LA    R6,FLDDATA+1                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),CKEYCLT,(R6)                                           
         MVI   FLDDATA+4,C'/'                                                   
         MVC   FLDDATA+5(20),CNAME                                              
         MVC   MYCPROF(15),CPROF                                                
         MVC   MYCPROF+15(15),CEXTRA                                            
         MVC   FLDDATA+30(5),MYCPROF                                            
         MVI   FLDDATA+35,C' '                                                  
         MVC   FLDDATA+36(5),MYCPROF+5                                          
         MVI   FLDDATA+41,C' '                                                  
         MVC   FLDDATA+42(5),MYCPROF+10                                         
         MVI   FLDDATA+47,C' '                                                  
         MVC   FLDDATA+48(5),MYCPROF+15                                         
         MVI   FLDDATA+53,C' '                                                  
         MVC   FLDDATA+54(5),MYCPROF+20                                         
         MVI   FLDDATA+59,C' '                                                  
         MVC   FLDDATA+60(5),MYCPROF+25                                         
         MVC   FLDDATA+66(1),COFFICE                                            
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         BCT   R4,PFSEQ                                                         
         MVC   PREVKEY,KEY                                                      
         B     MODEXIT                                                          
PFEND    XC    PREVKEY,PREVKEY                                                  
         B     MODEXIT                                                          
         EJECT                                                                  
*                                                                               
* DISPLAY PRODUCT CODES                                                         
*                                                                               
PRODRTN  CLC   5(3,R6),=C'PRD'                                                  
         BE    PD1                                                              
         CLC   5(4,R6),=C'PROD'                                                 
         BE    PD1                                                              
         LA    RE,9                                                             
         B     FLTERR                                                           
*                                                                               
* READ CLIENT HEADERS                                                           
PD1      MVI   SVSUBSC,2                                                        
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING CLTHDRD,R5                                                       
         MVC   CKEYAM,SVAGYMD                                                   
         MVC   CKEYCLT,SVCLT                                                    
         MVI   CDFRST,1                                                         
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY,PREVKEY                                                      
         XC    PREVKEY,PREVKEY                                                  
         LA    R4,14               SET MAX LINES                                
PDHIGH   GOTO1 HIGH                                                             
         B     PRHAVREC                                                         
*                                                                               
PRSEQ    GOTO1 SEQ                                                              
*                                                                               
PRHAVREC LA    R5,KEY                                                           
         CLC   SVAGYMD,CKEYAM                                                   
         BNE   PREND                                                            
         CLI   CKEYCLT+2,0                                                      
         BE    *+14                                                             
         MVC   CKEYCLT+2(9),HIKEY                                               
         B     PDHIGH                                                           
         CLI   CDFRST,1                                                         
         BNE   PR1                                                              
         MVI   CDFRST,0                                                         
         LA    R2,SINHDRH                                                       
         MVC   FLDDATA+1(16),=C'CLIENT CODE/NAME'                               
         MVC   FLDDATA+27(13),=C'PRODUCT CODES'                                 
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(16),=C'----------------'                               
         MVC   FLDDATA+27(13),=C'-------------'                                 
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
PR1      LA    R2,LINLEN(R2)                                                    
         BCT   R4,*+8                                                           
         B     MODEXIT                                                          
         GOTO1 GETREC                                                           
         L     R5,AREC                                                          
         LA    RE,CLIST                                                         
         LA    RF,CPRDMAX                                                       
         MHI   RF,4                                                             
         AR    RF,RE               SET ABSOLUTE END                             
         SR    R6,R6                                                            
PR2      CLI   0(RE),0             COUNT NUMBER OF PRODUCTS                     
         BE    PR3                                                              
         LA    R6,1(R6)                                                         
         LA    RE,4(RE)                                                         
         CR    RE,RF                                                            
         BL    PR2                                                              
*                                                                               
PR3      SRDL  R6,32               DETERMINE NUMBER OF LINES REQUIRED           
         D     R6,=F'13'                                                        
         LTR   R6,R6                                                            
         BZ    *+8                                                              
         LA    R7,1(R7)                                                         
         CR    R7,R4                                                            
         BH    MODEXIT                                                          
*                                                                               
         MVC   DMCB+4(4),=X'D9000A15'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         LA    R9,FLDDATA+1                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(CPROF+6,CKEYCLT),(R9)                                 
         MVI   FLDDATA+4,C'/'                                                   
         MVC   FLDDATA+5(20),CNAME                                              
         MVC   PREVKEY,KEY                                                      
         LA    RE,CLIST                                                         
         LA    RF,CPRDMAX                                                       
         MHI   RF,4                                                             
         AR    RF,RE                                                            
         CLI   0(RE),0                                                          
         BNE   PR4                                                              
         MVC   FLDDATA+27(20),=C'NO PRODUCTS ASSIGNED'                          
         B     PR6                                                              
PR4      LA    R7,13               SEND PRODUCT CODES                           
         LA    R6,FLDDATA+27                                                    
PR5      MVC   0(3,R6),0(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    R6,4(R6)                                                         
         CLI   0(RE),0                                                          
         BE    PR6                                                              
         CR    RE,RF                                                            
         BNL   PR6                                                              
         BCT   R7,PR5                                                           
*                                                                               
PR6      FOUT  (R2)                                                             
         BCTR  R4,0                                                             
         LA    R2,LINLEN(R2)                                                    
         CLI   0(RE),0                                                          
         BE    PRSEQ                                                            
         CR    RE,RF                                                            
         BL    PR4                                                              
         B     PRSEQ                                                            
PREND    XC    PREVKEY,PREVKEY                                                  
         B     MODEXIT                                                          
*                                                                               
         EJECT                                                                  
FLTERR   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SINMSG(0),0(R6)                                                  
         LA    RE,3(RE)                                                         
         LA    RF,SINMSG(RE)                                                    
         MVC   0(22,RF),=C'- INVALID FILTER FIELD'                              
         LA    RF,23(RF)                                                        
         MVC   0(20,RF),WORK                                                    
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,SINIFLTH                                                      
         B     MODX                                                             
MODEXIT  LA    R2,SINENDH                                                       
         OC    PREVKEY,PREVKEY                                                  
         BZ    MODX                                                             
         LA    R2,SINENDH                                                       
         MVI   PREVKEY+4,X'FF'                                                  
         MVC   PREVKEY+5(8),PREVKEY+4                                           
MODX     OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
HIKEY    DC    9X'FF'                                                           
         LTORG                                                                  
         EJECT                                                                  
CLTWRKD  DSECT                                                                  
CDFRST   DS    CL1                                                              
CPROFLT  DS    CL15                PROFILE FILTERS                              
NMGFLTR  DS    CL1                                                              
J1FLTR   DS    CL1                                                              
CPRDMAX  EQU   110                                                              
LINLEN   EQU   88                                                               
         EJECT                                                                  
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
* SPGENCLT                                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       SPACE 2                                                                  
* CEXTRA DSECT                                                                  
* ADDED TO EXTEND PROFILE FROM 15 TO 30                                         
*                                                                               
GENOLD DSECT                                                                    
         ORG   REC+1200                                                         
MYCPROF  DS    CL30                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPINF11   03/06/06'                                      
         END                                                                    
