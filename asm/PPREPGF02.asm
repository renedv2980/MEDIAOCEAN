*          DATA SET PPREPGF02  AT LEVEL 165 AS OF 05/01/02                      
************************\                                                       
*PHASE PPGF02A,+0,NOAUTO ++++++++++   NOTE 'A' APPENDED TO PHASE NAME           
************************/                                                       
*                                                                               
************  CHANGE LOG  ************                                          
*                                                                               
*  SMYE  12/12/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                     
*                                                                               
         TITLE 'PPGF02 - PRINTPAK CLIENT CONVERSION PROGRAM'                    
PPGF02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPGF02                                                         
*                                                                               
         LA    R7,1(RB)                                                         
         LA    R7,4095(R7)                                                      
         USING PPGF02+4096,R7                                                   
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPGFWRKD,R8                                                      
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BNE   EXIT                                                             
         RELOC (R3)                                                             
         L     RF,=A(BSTAB)                                                     
         AR    RF,R3                                                            
         ST    RF,ABSTAB                                                        
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
         ZAP   PRGCNT,=P'0'                                                     
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT1,C'H'                                                       
         BE    HDRS            CLIENT HDRS                                      
         B     EXIT                                                             
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
HDRS     DS    0H                                                               
         MVI   RCSUBPRG,50                                                      
*                                                                               
*                                                                               
         LA    R5,CTYPTAB                                                       
HD5      CLI   0(R5),X'FF'        DONE                                          
         BE    HD12                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVC   KEY+3(1),0(R5)      LOOK FOR TYPE                                
         MVC   KEY+4(3),QCLIENT                                                 
         GOTO1 HIGH                                                             
         B     HD6B                                                             
HD6      DS    0H                                                               
         GOTO1 SEQ                                                              
HD6B     DS    0H                                                               
         CLC   KEY(7),KEYSAVE       AGY/MED/TYPE/CLIENT                         
         BNE   HD10                 NOT FOUND GO DO NEXT TYPE                   
         BAS   RE,PRNTKEY                                                       
**************                                                                  
*        SWITCH CLIENT CODES HERE                                               
*        AND WRITE BACK RECORDS                                                 
**************                                                                  
*                                                                               
         B     HD6                                                              
*                                                                               
HD10     LA    R5,1(R5)                                                         
         B     HD5                                                              
*                                                                               
HD12     DS    0H                                                               
         XC    PUBREC(33),PUBREC                                                
         XC    LASTPUB,LASTPUB                                                  
         DS    0H                                                               
HD13     BAS   RE,NXTPUB                                                        
         CLI   PUBKEY,X'FF'        END OF FILE                                  
         BE    HD14                                                             
         BAS   R2,CKPUB           ADD 11/30/87                                  
         B     HD13                                                             
*                                                                               
HD14     BAS   RE,RESULTS                                                       
         B     EXIT                                                             
*                                                                               
CKPUB    EQU   *              ************************************              
*                             * CHECK PUB FILE FOR ACTIVE CLIENT *              
*                             * AND MARK X'FF' FOR THAT CLIENT.  *              
*                             *   ALL ELEMENTS SUCH AS           *              
*                             *  X'11',X'14',X'08',X'09' AND     *              
*                             *  X'0A' ARE CHECKED FOR REGULAR   *              
*                             *  PUB RECORD.  ELEMENT X'71' IS   *              
*                             *  CHECKED FOR PUB LITTLE RECORD.  *              
*                             ************************************              
         L     R3,ABSTAB      CLIENT TABLE                                      
         L     R4,BSPARS+8    # OF RECS IN THE CLIENT TABLE                     
CKPUB1   CLI   6(R3),X'FF'    ACTIVE CLIENT                                     
         BNE   CKPUBN                                                           
CKPUB2   LA    R3,7(R3)       NEXT CLIENT                                       
         BCT   R4,CKPUB1                                                        
         BR    R2             FINISH                                            
*                                                                               
ELHOLD   DS    CL3                                                              
         DS    0F                                                               
*                                                                               
CKPUBN   EQU   *              PROCESS PUB NORMAL REC                            
         LA    R5,PUBREC+33   ADDRESS OF THE FIRST ELEMENT                      
CKPUBN0  CLI   0(R5),X'11'    PUB SUPPL.ADDR ELEMENT                            
         BNE   CKPUBN0A                                                         
         MVC   ELHOLD(3),=C'SUP'                                                
         B     CKPUBN1                                                          
CKPUBN0A CLI   0(R5),X'14'    PUB REP ELEMENT                                   
         BNE   CKPUBN0B                                                         
         MVC   ELHOLD(3),=C'REP'                                                
         B     CKPUBN1                                                          
CKPUBN0B CLI   0(R5),X'08'    PAY ADDR                                          
         BNE   CKPUBN0C                                                         
         MVC   ELHOLD(3),=C'PAY'                                                
         B     CKPUBN1                                                          
CKPUBN0C CLI   0(R5),X'09'    TRAFFIC ADDR                                      
         BNE   CKPUBN0D                                                         
         MVC   ELHOLD(3),=C'TRA'                                                
         B     CKPUBN1                                                          
CKPUBN0D CLI   0(R5),X'0A'    CONTRACT ADDR                                     
         BNE   CKPUBN2                                                          
         MVC   ELHOLD(3),=C'CON'                                                
CKPUBN1  CLC   2(3,R5),0(R3)  MATCH CLIENT                                      
         BNE   CKPUBN2                                                          
         MVI   6(R3),X'FF'    MARK ACTIVE CLIENT                                
CKPUBN1A BAS   RE,PRNTKEY     PRINT INFO AS 'PUB CLI ELE ...'                   
         XC    ELHOLD,ELHOLD                                                    
         B     CKPUB2         PROCESS NEXT CLIENT                               
CKPUBN2  XR    R6,R6                                                            
         IC    R6,1(R5)       INSERT ELEMENT LENGTH                             
         AR    R5,R6          ADDR OF THE BEGINNING OF NEXT ELEM                
         CLI   0(R5),0        END OF PUB NORMAL REC                             
         BE    CKPUBL         PROCESS PUB LITTLE REC                            
         B     CKPUBN0        CHECK NEXT ELEMENT                                
*                                                                               
CKPUBL   EQU   *              PROCESS PUB LITTLE RECORD                         
         L     R5,ALTLREC                                                       
         LA    R5,33(R5)                                                        
CKPUBL0  CLI   0(R5),X'71'    PUB REG/DIST ELEMENT                              
         BNE   CKPUBL1                                                          
         CLC   2(3,R5),0(R3)  MATCH CLIENT                                      
         BNE   CKPUBL1                                                          
         MVI   6(R3),X'FF'    MARK ACTIVE CLIENT                                
         MVC   ELHOLD(3),=C'REG'                                                
         B     CKPUBN1A       PRINT AND PROCESS NEXT CLIENT                     
CKPUBL1  XR    R6,R6                                                            
         IC    R6,1(R5)       INSERT ELEMENT LENGTH                             
         AR    R5,R6          ADDR OF THE BEGINNING OF NEXT ELEM                
         CLI   0(R5),0        END OF PUB LITTLE REC                             
         BE    CKPUB2         PROCESS NEXT CLIENT                               
         B     CKPUBL0        CHECK NEXT ELEMENT                                
         EJECT                                                                  
*                             PRINT KEYS                                        
         SPACE 2                                                                
PRNTKEY  NTR1                                                                   
         SPACE 2                                                                
         CLI   QOPT2,C'A'                                                       
         BE    EXIT                                                             
         CLI   KEY+1,C'A'                                                       
         BL    PKPUB                                                            
         CLI   KEY+3,X'20'                                                      
         BE    PKBUY                                                            
         CLI   KEY+3,X'21'                                                      
         BE    PKBUY2                                                           
         CLI   KEY+3,X'10'                                                      
         BE    PKCON                                                            
         CLI   KEY+3,X'14'        AOR                                           
         BE    PKAOR                                                            
         CLI   KEY+3,X'15'                                                      
         BE    PKJOB                                                            
         CLI   KEY+3,X'02'                                                      
         BE    PKCLT                                                            
         CLI   KEY+3,X'06'                                                      
         BE    PKPRD                                                            
         CLI   KEY+3,X'07'                                                      
         BE    PKEST                                                            
         CLI   KEY+3,X'03'        DIVISIONS                                     
         BE    PKDIV                                                            
         CLI   KEY+3,X'04'        REGIONS                                       
         BE    PKREG                                                            
         CLI   KEY+3,X'05'        DISTRICTS                                     
         BE    PKDST                                                            
         CLI   KEY+3,X'09'        EST BUCKETS                                   
         BE    PKBUC                                                            
         CLI   KEY+3,X'08'        BILLRESC                                      
         BE    PKBIL                                                            
         CLI   KEY+3,X'17'        PUB LISTS                                     
         BE    PKPLT                                                            
         CLI   KEY+3,X'30'        USERP RECS                                    
         BE    PKUSR                                                            
         B     EXIT                                                             
*                                                                               
PK2      DS    0H                                                               
         CLI   QOPT1,C'P'                                                       
         BE    PK20                                                             
         CLI   QOPT1,C'R'          REPS                                         
         BNE   PK4                                                              
         L     RF,BSPARS                                                        
         CLI   MISSSW,C'Y'                                                      
         BE    PK2B                                                             
         MVC   P+25(4),=C'USES'                                                 
         MVC   P+30(4),0(RF)                                                    
         B     PK2D                                                             
*                                                                               
PK2B     MVC   P+22(7),=C'MISSING'                                              
         MVC   P+30(4),0(RF)                                                    
PK2D     CLI   QMEDIA,C'O'         CHK OUTDOOR                                  
         BNE   PK20                                                             
*              R4 FROM BASE PROGRAM                                             
         CH    R4,=H'2'            SEE IF DOING TRAFFIC REP                     
         BNE   PK20                NO                                           
         CLI   4(RF),0             SEE IF DOING SUFFIX                          
         BE    PK20                NO                                           
         MVI   P+34,C'.'                                                        
         MVC   P+35(1),4(RF)       SUFFIX                                       
         B     PK20                                                             
*                                                                               
PK4      DS    0H                                                               
         CLI   QOPT1,C'J'                                                       
         BNE   PK6                                                              
         L     RF,BSPARS                                                        
         MVC   P+49(4),=C'USES'                                                 
         MVC   P+54(6),6(RF)                                                    
         B     PK20                                                             
PK6      DS    0H                                                               
         CLI   QOPT1,C'C'                                                       
         BNE   PK20                                                             
         L     RF,BSPARS                                                        
         CLI   MISSSW,C'Y'                                                      
         BE    PK6C                                                             
         MVC   P+49(4),=C'USES'                                                 
         MVC   P+54(6),0(RF)                                                    
         B     PK20                                                             
*                                                                               
PK6C     MVC   P+49(6),0(RF)                                                    
         MVC   P+57(7),=C'MISSING'                                              
         B     PK20                                                             
*                                                                               
PK20     DS    0H                                                               
         BAS   RE,RPRT                                                          
         B     EXIT                                                             
         SPACE 3                                                                
PKBUY    DS    0H                                                               
         MVC   WORK(25),KEY                                                     
         B     PKBUY4                                                           
PKBUY2   DS    0H                                                               
         MVC   WORK(7),KEY                                                      
         MVC   WORK+7(3),KEY+13                                                 
         MVC   WORK+10(6),KEY+7                                                 
         MVC   WORK+16(9),KEY+16                                                
*                                                                               
PKBUY4   DS    0H                                                               
         MVC   P(3),=C'BUY'                                                     
         MVC   P+5(3),WORK+4                                                    
         MVC   P+9(3),WORK+7                                                    
         MVC   HALF,WORK+19                                                     
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+13(3),DUB                                                      
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),WORK+10),P+17                                 
*                                                                               
*        GOTO1 DTCNV,DMCB,(1,WORK+16),(3,P+35)                                  
         GOTO1 DATCON,DMCB,(3,WORK+16),(5,P+35)                                 
*                                                                               
         CLI   WORK+24,1                                                        
         BE    PKBUY6                                                           
         SR    R0,R0                                                            
         IC    R0,WORK+24                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+44(2),DUB                                                      
         MVI   P+43,C'-'                                                        
*                                                                               
PKBUY6   DS    0H                                                               
         B     PK2                                                              
         SPACE 3                                                                
PKPUB    DS    0H                                                               
         CLI   QOPT1,C'H'                                                       
         BNE   PKPUB10                                                          
         MVC   P(3),=C'PUB'                                                     
         MVC   P+5(3),0(3)         MVC CLIENT                                   
         MVC   P+9(3),ELHOLD       ONE OF 'SUP,REP,PAY,TRA,CON,REG'             
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+1),P+17                                   
         B     PK20                                                             
*                                                                               
PKPUB10  IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+1),P                                      
         CLI   QOPT1,C'R'          SEE IF PURGING REPS                          
         BE    PKPUB15                                                          
         CLI   QOPT1,C'C'          OR IF PURGING COMMENTS                       
         BNE   PK2                                                              
PKPUB15  MVC   P+20(3),2(R2)       CLIENT CODE FROM CLIENT ELEM                 
*                                                                               
         B     PK2                                                              
         SPACE 3                                                                
PKCON    DS    0H                                                               
         MVC   P(3),=C'CON'                                                     
         MVC   P+5(3),KEY+4                                                     
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+7),P+9                                    
*                                                                               
         MVC   HALF,KEY+13                                                      
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+26(3),DUB                                                      
*                                                                               
         B     PK2                                                              
         SPACE 3                                                                
PKJOB    DS    0H                                                               
         MVC   P(3),=C'JOB'                                                     
         MVC   P+5(3),KEY+4                                                     
         MVC   P+9(3),KEY+7                                                     
         MVC   P+13(6),KEY+10                                                   
         OC    KEY+16(6),KEY+16                                                 
         BZ    PKJOB2                                                           
         MVC   P+20(8),=C'ALL PUBS'                                             
         CLI   KEY+16,X'FF'                                                     
         BE    PKJOB2                                                           
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),KEY+16),P+20                                  
*                                                                               
PKJOB2   DS    0H                                                               
         B     PK2                                                              
         SPACE 3                                                                
PKCLT    DS    0H                                                               
         MVC   P(3),=C'CLT'                                                     
         MVC   P+5(3),KEY+4                                                     
         B     PK2                                                              
         SPACE 3                                                                
PKPRD    DS    0H                                                               
         MVC   P(3),=C'PRD'                                                     
         MVC   P+5(3),KEY+4                                                     
         MVC   P+9(3),KEY+7                                                     
         B     PK2                                                              
         SPACE 3                                                                
PKUSR    DS    0H                                                               
         MVC   P(3),=C'USR'                                                     
         MVC   P+5(4),KEY+4                                                     
         B     PK2                                                              
         SPACE 3                                                                
PKAOR    DS    0H                                                               
         MVC   P(3),=C'AOR'                                                     
         MVC   P+5(3),KEY+4                                                     
         MVC   P+9(3),KEY+7                                                     
         CLI   KEY+10,X'FF'                                                     
         BNE   PKAOR5                                                           
         MVC   P+14(3),=C'ALL'                                                  
         B     PK2                                                              
PKAOR5   ZICM  R0,KEY+10                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+14(3),DUB+6(2)        ESTIMATE                                 
         B     PK2                                                              
         SPACE 3                                                                
PKPLT    DS    0H                                                               
         MVC   P(3),=C'LST'        PUBLISTS                                     
         MVC   P+5(3),KEY+4                                                     
         MVC   P+9(6),KEY+7                                                     
         B     PK2                                                              
         SPACE 3                                                                
PKDIV    DS    0H                                                               
         MVC   P(3),=C'DIV'                                                     
         MVC   P+5(3),KEY+4                                                     
         MVC   P+9(3),KEY+7                                                     
         B     PK2                                                              
         SPACE 3                                                                
PKREG    DS    0H                                                               
         MVC   P(3),=C'REG'                                                     
         MVC   P+5(3),KEY+4                                                     
         MVC   P+9(6),KEY+7                                                     
         B     PK2                                                              
         SPACE 3                                                                
PKDST    DS    0H                                                               
         MVC   P(3),=C'DST'                                                     
         MVC   P+5(3),KEY+4                                                     
         MVC   P+9(9),KEY+7                                                     
         B     PK2                                                              
         SPACE 3                                                                
PKEST    DS    0H                                                               
         MVC   P(3),=C'EST'                                                     
         B     PKEST5                                                           
PKBIL    DS    0H                                                               
         MVC   P(3),=C'BIL'                                                     
         B     PKEST5                                                           
*                                                                               
PKBUC    MVC   P(3),=C'BKT'                                                     
PKEST5   MVC   P+5(3),KEY+4                                                     
         MVC   P+9(3),KEY+7                                                     
         MVC   HALF,KEY+10                                                      
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+13(3),DUB                                                      
         B     PK2                                                              
         EJECT                                                                  
*                             PRINT RESULTS AND MARK RECORDS                    
         SPACE 2                                                                
RESULTS  NTR1                                                                   
         SPACE 2                                                                
         MVI   FORCEHED,C'Y'                                                    
         IC    R1,RCSUBPRG                                                      
         LA    R1,1(R1)                                                         
         STC   R1,RCSUBPRG                                                      
         BAS   RE,RPRT                                                          
         L     R2,ABSTAB                                                        
         L     R3,BSPARS+8         NO.IN TABLE                                  
*                                                                               
RS2      DS    0H                                                               
         MVC   P,SPACES                                                         
         CLI   QOPT1,C'P'                                                       
         BE    RSPUB                                                            
         CLI   QOPT1,C'R'                                                       
         BE    RSREP                                                            
         CLI   QOPT1,C'J'                                                       
         BE    RSJOB                                                            
         CLI   QOPT1,C'C'                                                       
         BE    RSCOM                                                            
         CLI   QOPT1,C'H'                                                       
         BE    RSHDR                                                            
*                                                                               
*                                                                               
RS19     DS    0H                                                               
         CLI   QOPT2,C'A'                                                       
         BE    RS21                                                             
*                                                                               
RS20     DS    0H                                                               
         BAS   RE,RPRT                                                          
*                                                                               
RS21     DS    0H                                                               
         A     R2,BSPARS+12                                                     
         BCT   R3,RS2                                                           
*                                                                               
         MVI   SPACING,2                                                        
         MVC   P,SPACES                                                         
         BAS   RE,RPRT                                                          
         EDIT  (P5,PRGCNT),(9,P),COMMAS=YES                                     
         MVC   P+10(14),=C'RECORDS PURGED'                                      
         BAS   RE,RPRT                                                          
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
RSPUB    DS    0H                                                               
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),0(R2)),P                                      
*                                                                               
         MVC   P+18(6),=C'PURGED'                                               
         CLI   7(R2),0                                                          
         BE    RSPUB4                                                           
         MVC   P+18(10),=C'NOT PURGED'                                          
         B     RS19                                                             
*                                                                               
RSPUB4   DS    0H                                                               
         CLI   QOPT3,C'P'                                                       
         BNE   RSPUB5                                                           
*                                                                               
         MVC   CARD,SPACES                                                      
         MVC   CARD(05),=C'11ZZO'                                               
         MVC   CARD+15(17),P                                                    
         GOTO1 CARDS,DMCB,CARD,=C'PE00'                                         
*                                                                               
RSPUB5   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),0(R2)                                                   
         MVC   KEY+7(2),PAGYKAGY                                                
         MVI   KEY+9,X'81'                                                      
         GOTO1 READPUB                                                          
         OI    KEY+25,X'80'                                                     
         GOTO1 WRTPUB                                                           
         CLI   6(R2),C'L'                                                       
         BNE   RS20                                                             
         MVC   P+24(2),=C'-2'                                                   
         MVI   KEY+9,X'85'                                                      
         GOTO1 READPUB                                                          
         OI    KEY+25,X'80'                                                     
         GOTO1 WRTPUB                                                           
         B     RS20                                                             
         SPACE 3                                                                
RSREP    DS    0H                                                               
         MVC   P(4),0(R2)                                                       
         CLI   4(R2),0                                                          
         BE    RSREP2                                                           
         MVI   P+4,C'.'                                                         
         MVC   P+5(1),4(R2)                                                     
RSREP2   MVC   P+8(6),=C'PURGED'                                                
         CLI   5(R2),0                                                          
         BE    RSREP4                                                           
         MVC   P+8(10),=C'NOT PURGED'                                           
         B     RS19                                                             
RSREP4   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(5),0(R2)                                                   
         GOTO1 READ                                                             
         OI    KEY+25,X'80'                                                     
         GOTO1 WRT                                                              
         B     RS20                                                             
         SPACE 3                                                                
RSJOB    DS    0H                                                               
         MVC   P(3),0(R2)                                                       
         MVC   P+4(3),3(R2)                                                     
         MVC   P+8(6),6(R2)                                                     
         MVC   P+17(6),=C'PURGED'                                               
         CLI   12(R2),0                                                         
         BE    RSJOB2                                                           
         MVC   P+17(10),=C'NOT PURGED'                                          
         B     RS19                                                             
*                                                                               
RSJOB2   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+4(12),0(R2)                                                  
         GOTO1 HIGH                                                             
         B     RSJOB4B                                                          
RSJOB4   DS    0H                                                               
         GOTO1 SEQ                                                              
RSJOB4B  DS    0H                                                               
         CLC   KEY(16),KEYSAVE                                                  
         BNE   RS20                                                             
         GOTO1 READ                                                             
         OI    KEY+25,X'80'                                                     
         GOTO1 WRT                                                              
         B     RSJOB4                                                           
*                                                                               
         SPACE 3                                                                
*                             COMMENTS                                          
RSCOM    DS    0H                                                               
         MVC   P(6),0(R2)                                                       
         MVC   P+8(6),=C'PURGED'                                                
         CLI   6(R2),0                                                          
         BE    RSCOM2                                                           
         MVC   P+8(10),=C'NOT PURGED'                                           
         B     RS19                                                             
*                                                                               
RSCOM2   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),0(R2)                                                   
         GOTO1 READ                                                             
         OI    KEY+25,X'80'                                                     
         GOTO1 WRT                                                              
         B     RS20                                                             
*                                                                               
RSHDR    DS    0H                                                               
         MVC   P(3),0(R2)                                                       
         MVC   P+4(3),3(R2)                                                     
         MVC   P+10(6),=C'PURGED'                                               
         CLI   6(R2),0                                                          
         BE    RSHDR2                                                           
         MVC   P+10(10),=C'NOT PURGED'                                          
         B     RS19                                                             
*                                                                               
RSHDR2   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,X'02'          CLIENT HEADERS                              
         MVC   KEY+4(6),0(R2)                                                   
         GOTO1 READ                                                             
         OI    KEY+25,X'80'                                                     
         GOTO1 WRT                                                              
         B     RS20                                                             
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD4+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD4+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                             GET FIRST/NEXT PUB                                
NXTPUB   NTR1                                                                   
         SPACE 2                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),LASTPUB                                                 
         IC    R1,KEY+6                                                         
         LA    R1,1(R1)                                                         
         STC   R1,KEY+6                                                         
         MVC   KEY+7(2),PAGYKAGY                                                
         GOTO1 HIGHPUB                                                          
         B     NP2B                                                             
NP2      DS    0H                                                               
         GOTO1 SEQPUB                                                           
NP2B     DS    0H                                                               
         MVI   PUBKMED,X'FF'       SET EOF                                      
         CLC   KEY(1),KEYSAVE                                                   
         BNE   NPX                                                              
         CLC   KEY+7(2),PAGYKAGY        MUST BE RIGHT AGENCY                    
         BNE   NP2                                                              
         CLI   KEY+9,X'81'                                                      
         BNE   NP2                                                              
         MVC   LASTPUB,KEY+1                                                    
         GOTO1 GETNAME                                                          
         L     RF,ALTLREC                                                       
         XC    0(50,RF),0(RF)                                                   
         GOTO1 SEQPUB                                                           
         CLC   KEY(9),PUBKEY                                                    
         BE    NP5                                                              
         MVC   KEY(9),PUBKEY         MUST RESTORE KEY FOR PRINTING              
         B     NPX                                                              
*                                                                               
NP5      GOTO1 GETLTL                                                           
*                                                                               
NPX      DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
CTYPTAB  DC    X'03'             DIVISIONS                                      
         DC    X'04'             REGIONS                                        
         DC    X'05'             DISTRICTS                                      
         DC    X'06'             PRODUCTS                                       
         DC    X'07'             ESTIMATES                                      
         DC    X'08'             BILLS                                          
         DC    X'09'             EST BUCKETS                                    
         DC    X'10'             CONTRACTS                                      
         DC    X'14'             AOR RECORDS                                    
         DC    X'15'             ADCODES                                        
         DC    X'17'             PUB LISTS                                      
         DC    X'20'             BUYS                                           
         DC    X'FF'             END OF TABLE                                   
*                                                                               
         LTORG                                                                  
PATCH    DC    30X'00'                                                          
         EJECT                                                                  
PPGFWRKD DSECT                                                                  
         DS    0F                                                               
BSPARS   DS    CL24                                                             
ABSTAB   DS    A                                                                
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
MISSSW   DS    CL1                                                              
PRGCNT   DS    PL5                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
CARD     DS    CL80                                                             
*                                                                               
*                                                                               
*                                                                               
BILPROFD DSECT                                                                  
       ++INCLUDE PBILPROF                                                       
*                                                                               
         SPACE 3                                                                
BSTAB    CSECT                                                                  
         DS    400000C                                              L01         
BUFFL    EQU   400000                                               L01         
BSTAB6   EQU   400000/6                                             L01         
BSTAB8   EQU   400000/8                                             L01         
BSTAB13  EQU   400000/13                                            L01         
BSTAB7   EQU   400000/7                                             L01         
*                                                                               
USERPD   DSECT                                                                  
       ++INCLUDE PUSEREC                                                        
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'165PPREPGF02 05/01/02'                                      
         END                                                                    
