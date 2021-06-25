*          DATA SET PPREP5402  AT LEVEL 067 AS OF 12/09/03                      
*PHASE PP5402A                                                                  
         TITLE 'PP5402  ESTIMATE SUMMARY'                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 08/00/00 NEW PBILLREC (IN PNNEWFILE)                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PP5402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP5402                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R9,SPACEND                                                       
         USING PP54WRKD,R9                                                      
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BE    FXDELE                                                           
*                                                                               
         CLI   MODE,FBUYREQ                                                     
         BE    SCHDTE                                                           
*                                                                               
         CLI   MODE,FBUYCLI                                                     
         BE    SETPROF                                                          
*                                                                               
         CLI   MODE,FBUYEST                                                     
         BE    PEST                                                             
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    PBY                                                              
*                                                                               
         CLI   MODE,LBUYEST                                                     
         BE    RDBILLS                                                          
*                                                                               
         CLI   MODE,LBUYREQ                                                     
         BE    REQBK                                                            
*                                                                               
         CLI   MODE,LBUYPRO                                                     
         BE    PRDBK                                                            
*                                                                               
PP54EX   XMOD1 1                                                                
         EJECT                                                                  
*                                  ESTIMATE RECORDS                             
*                                     * SET UP MONTH (YM) LIST FOR              
*                                       ESTIMATE PERIOD                         
*                                     * SET UP MID LINE PRINTING                
*                                                                               
PEST     CLI   FRSTIM,1                                                         
         BE    PP54EX                                                           
         MVI   FRSTIM,1                                                         
*                                                                               
         MVC   LOWDTE,=3X'FF'                                                   
         XC    HIDTE,HIDTE                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(0,PESTST),(3,DUB)                                   
         MVC   DTELST(1),DUB                                                    
*                                                                               
         PACK  DUB,PESTST+2(2)                                                  
         CVB   R0,DUB                                                           
         CH    R0,=H'6'                                                         
         BH    PE00                                                             
         AH    R0,=H'12'                                                        
         IC    R1,DTELST                                                        
         SH    R1,=H'1'                                                         
         STH   R1,HALF                                                          
         MVC   DTELST(1),HALF+1                                                 
PE00     SH    R0,=H'6'                                                         
         ST    R0,FULL                                                          
         MVC   DTELST+1(1),FULL+3                                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,PESTEND),(3,DUB)                                  
         XC    FULL,FULL                                                        
         MVC   FULL+3(1),DUB                                                    
*                                                                               
         PACK  DUB,PESTEND+2(2)                                                 
         CVB   R0,DUB                                                           
         AH    R0,=H'3'                                                         
         CH    R0,=H'12'                                                        
         BNH   PE01                                                             
         SH    R0,=H'12'                                                        
         IC    R1,FULL+3                                                        
         LA    R1,1(R1)                                                         
         ST    R1,FULL                                                          
PE01     STH   R0,HALF                                                          
         MVC   HALF(1),FULL+3                                                   
         LA    R1,DTELST                                                        
         LA    R5,23                                                            
PE02     CLC   0(2,R1),HALF                                                     
         BE    PE20                                                             
         BCT   R5,PE03                                                          
         B     PE20                                                             
PE03     CLI   1(R1),12                                                         
         BNE   PE04                                                             
         IC    R2,0(R1)                                                         
         LA    R2,1(R2)                                                         
         STC   R2,2(R1)                                                         
         MVI   3(R1),1                                                          
         B     PE06                                                             
PE04     IC    R2,1(R1)                                                         
         LA    R2,1(R2)                                                         
         STC   R2,3(R1)                                                         
         MVC   2(1,R1),0(R1)                                                    
PE06     LA    R1,2(R1)                                                         
         B     PE02                                                             
*                                                                               
PE20     MVC   MID1(8),=C'ESTIMATE'                                             
         EDIT  (2,PESTKEST),(3,MID1+9),ALIGN=LEFT                               
         MVC   MID1+13(L'PESTNAME),PESTNAME                                     
         CLI   PESTNAM2,C' '                                                    
         BNH   *+10                                                             
         MVC   MID2+13(L'PESTNAM2),PESTNAM2                                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,PESTST),(10,MID1+40)                              
         MVI   MID1+48,C'-'                                                     
         GOTO1 DATCON,DMCB,(0,PESTEND),(10,MID1+49)                             
*                                                                               
         CLI   PESTSTAT,C'1'       SEE IF LOCKED REG OR PERM                    
         BL    *+10                                                             
         MVC   MID1+59(6),=C'LOCKED'                                            
         MVI   FORCEMID,C'Y'                                                    
         MVI   ALLOWLIN,4                                                       
         B     PP54EX                                                           
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
SETPROF  DS    0H                  READ PROFILES                                
         MVI   CDSW,0              SET DEFAULT VALUE                            
*                                  SUBTRACT CD                                  
         OC    PROGPROF,PROGPROF   CHK FOR PROFILE                              
         BZ    SETPRX                                                           
         CLI   PROGPROF,C'N'       SEE IF SUBTRACTING CD                        
         BNE   *+8                                                              
         MVI   CDSW,1                                                           
*                                                                               
SETPRX   B     PP54EX                                                           
*                                  SET UP KEY AND READ BILL RECS                
*                                                                               
RDBILLS  DS    0H                                                               
         CLI   DTELST,0            WILL BE NON-ZERO IF I GOT FBUYEST            
         BE    PP54EX              NO EST ACTIVITY                              
         MVC   SVKEYS(64),KEY      SAVE KEY AND KEYSAVE                         
         XC    KEY,KEY                                                          
         MVC   KEY(7),RCSVAGY                                                   
         CLC   QPRODUCT,=C'ZZZ'         NO PROD IF ZZZ                          
         BE    *+10                                                             
         MVC   KEY+7(3),PPRDKPRD                                                
         MVI   KEY+3,X'08'                                                      
         MVC   KEY+10(2),PESTKEST                                               
         BAS   R8,GETHI                                                         
         B     *+8                                                              
RB02     BAS   R8,GETSEQ                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BNE   RBEX                                                             
         OC    KEYSAVE+7(3),KEYSAVE+7                                           
         BZ    *+14                                                             
         CLC   KEY+7(3),KEYSAVE+7       PRODUCT                                 
         BNE   RB02                                                             
         CLC   KEY+10(2),KEYSAVE+10     ESTIMATE                                
         BNE   RB02                                                             
         LA    R2,BQSTART          USE BQSTART + BQEND                          
         CLI   QBPDATE,C'B'        IF BILL DATE REQ                             
         BE    *+8                                                              
         LA    R2,LOWDTE           OR LOWDTE AND HIDTE                          
         CLC   KEY+12(2),0(R2)      AS BILL DATE FILTERS                        
         BL    RB02                                                             
         CLC   KEY+12(2),3(R2)                                                  
         BH    RB02                                                             
*                                                                               
         BAS   R8,GETBILL1                                                      
         B     PBL                                                              
*                                                                               
RBEX     MVC   KEY(64),SVKEYS                                                   
         B     ESTBK                                                            
*                                  READ HI                                      
GETHI    LA    RF,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     GETIT                                                            
*                                  READ SEQ                                     
GETSEQ   LA    RF,DMRSEQ                                                        
*                                                                               
GETIT    ST    RF,DMCB                                                          
         LR    R0,R8                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,,PRTDIR,KEY,KEY                                     
*                                                                               
CHCKIT   TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R8,R0                                                            
         BR    R8                                                               
*                                                                               
GETBILL1 LR    R0,R8                                                            
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,PBILLREC,DMWORK               
         B     CHCKIT                                                           
*                                                                               
*                                  BILLING RECORDS                              
*                                  ADD SUMMARY BILLING TO ACCUM -               
*                                  IT IS SUMMARY BILLING IF                     
*                                  PBILLTYP NOT = 4                             
*                                  PBILLCAN = ZEROS                             
*                                  PBILLCDT = ZEROS                             
*                                                                               
PBL      CLI   PBILLTYP,C'4'                                                    
         BE    RB02                                                             
         CLC   PBILLCAN(6),=6C'0'                                               
         BNE   RB02                                                             
         CLC   PBILLCDT(6),=6C'0'                                               
         BNE   RB02                                                             
*                                  ADD TO ACCUMS                                
         LA    R2,ACCUM+40                                                      
         LA    R3,DTELST                                                        
         LA    R4,1                                                             
PB02     CLI   0(R3),0                                                          
         BE    RB02                                                             
         CLC   0(2,R3),PBILKMOS                                                 
         BE    PB04                                                             
         LA    R4,1(R4)                                                         
         LA    R3,2(R3)                                                         
         B     PB02                                                             
PB04     MH    R4,=H'64'                                                        
         SH    R4,=H'64'                                                        
         AR    R2,R4                                                            
         CLI   NETSW,1             SEE IF SHOWING NET                           
         BNE   PB05                                                             
         ZAP   DUB,PBILLNET        G-AC-CD                                      
         CLI   CDSW,0              SEE IF SUBTRACTING CD                        
         BE    PB06                YES                                          
         ZAP   DOUBLE,PBILLGRS     MUST CALC CD                                 
         SP    DOUBLE,PBILLBIL                                                  
         AP    DUB,DOUBLE          ADD CD                                       
         B     PB06                                                             
*                                                                               
PB05     ZAP   DUB,PBILLBIL        GR-CD                                        
         CLI   CDSW,0              SEE IF SUBTRACTING CD                        
         BE    PB06                YES                                          
         ZAP   DUB,PBILLGRS        ELSE SHOW GROSS                              
PB06     DS    0X                                                               
         AP    0(8,R2),DUB                                                      
         B     RB02                                                             
         EJECT                                                                  
*                                  BUY RECORDS                                  
*                                  ADD TO ACCUMS ON BASIS OF                    
*                                  BILLABLE DATE AND INSER-                     
*                                  TION DATE.                                   
*                                                                               
PBY      TM    PBUYCNTL,X'80'      TEST FOR A DELETE                            
         BZ    PBY01                                                            
         XC    GROSS(20),GROSS                                                  
*                                  BILLABLE DATE                                
PBY01    DS    0H                                                               
         MVC   BILLDT,PBDBDATE                                                  
         MVC   INSDTE,PBUYKDAT                                                  
*                                  SAVE LOWEST BUY DATE (INS OR BILL)           
         CLC   LOWDTE,PBDBDATE                                                  
         BNH   *+10                                                             
         MVC   LOWDTE,PBDBDATE                                                  
         CLC   LOWDTE,PBUYKDAT                                                  
         BNH   *+10                                                             
         MVC   LOWDTE,PBUYKDAT                                                  
*                                                                               
         CLC   HIDTE,PBDBDATE                                                   
         BNL   *+10                                                             
         MVC   HIDTE,PBDBDATE                                                   
         CLC   HIDTE,PBUYKDAT                                                   
         BNL   *+10                                                             
         MVC   HIDTE,PBUYKDAT                                                   
*                                                                               
*                                  INSERTION DATE GROSS                         
         LA    R2,DTELST                                                        
         LA    R3,ACCUM                                                         
         LA    R4,1                                                             
PBY02    CLI   0(R2),0                                                          
         BNE   PBY03                                                            
         TM    PBUYCNTL,X'80'                                                   
         BNO   PBY02C                                                           
         OC    BGROSS(12),BGROSS       CHK FOR BILLING                          
         BNZ   PBY02C                                                           
         OC    PGROSS(12),PGROSS       OR PAYMENTS                              
         BNZ   PBY02C                                                           
         B     PP54EX                                                           
*                                                                               
PBY02C   DC    H'0'                   UNDELETED BUY OUT OF PERIOD               
*                                     OR BILLED OR PAID DELETED BUY             
*                                                                               
PBY03    DS    0H                                                               
         CLC   0(2,R2),INSDTE                                                   
         BE    PBY04                                                            
         LA    R4,1(R4)                                                         
         LA    R2,2(R2)                                                         
         B     PBY02                                                            
PBY04    MH    R4,=H'64'                                                        
         SH    R4,=H'64'                                                        
         AR    R3,R4                                                            
         MVC   DUB,0(R3)                                                        
         ICM   R4,15,GROSS                                                      
         CVD   R4,DOUBLE                                                        
         AP    DUB,DOUBLE                                                       
         CLI   NETSW,1             SEE IF SHOWING NET                           
         BNE   PBY05                                                            
         ICM   R4,15,AGYCOM                                                     
         CVD   R4,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
PBY05    MVC   0(8,R3),DUB                                                      
*                                  BILLED MONTH GROSS                           
PBY10    LA    R2,DTELST                                                        
         LA    R3,ACCUM                                                         
         LA    R4,1                                                             
PBY12    CLI   0(R2),0                                                          
         BNE   PBY13                                                            
*                                  OUT OF PERIOD                                
         TM    PBUYCNTL,X'80'      SEE IF DELETED                               
         BNO   PBY12E              NO - DIE                                     
         OC    BGROSS(12),BGROSS       SEE IF BILLED                            
         BNZ   PBY12E                  DIE                                      
         OC    PGROSS(12),PGROSS       SEE IF PAID                              
         BNZ   PBY12E                  DIE                                      
         B     PP54EX                  CAN SKIP                                 
*                                                                               
PBY12E   DC    H'0'                                                             
*                                                                               
PBY13    CLC   0(2,R2),BILLDT                                                   
         BE    PBY14                                                            
         LA    R4,1(R4)                                                         
         LA    R2,2(R2)                                                         
         B     PBY12                                                            
PBY14    MH    R4,=H'64'                                                        
         SH    R4,=H'64'                                                        
         AR    R3,R4                                                            
         MVC   DUB,8(R3)                                                        
         ICM   R4,15,GROSS                                                      
         CVD   R4,DOUBLE                                                        
         AP    DUB,DOUBLE                                                       
         CLI   NETSW,1             SEE IF SHOWING NET                           
         BNE   PBY15                                                            
         ICM   R4,15,AGYCOM                                                     
         CVD   R4,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
PBY15    MVC   8(8,R3),DUB                                                      
*                                  CASH DISCOUNT                                
         ICM   R4,15,CSHDSC                                                     
         CVD   R4,DUB                                                           
         AP    16(8,R3),DUB                                                     
*                                  GROSS LESS CD                                
         MVC   DUB,24(R3)                                                       
         ICM   R4,15,GROSS                                                      
         CVD   R4,DOUBLE                                                        
         AP    DUB,DOUBLE                                                       
         CLI   NETSW,1             SEE IF SHOWING NET                           
         BNE   PBY16                                                            
         ICM   R4,15,AGYCOM                                                     
         CVD   R4,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
PBY16    ICM   R4,15,CSHDSC                                                     
         CVD   R4,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
         MVC   24(8,R3),DUB                                                     
*                                  PAID                                         
         MVC   DUB,32(R3)                                                       
         ICM   R4,15,PGROSS                                                     
         CVD   R4,DOUBLE                                                        
         AP    DUB,DOUBLE                                                       
         CLI   NETSW,1             SEE IF SHOWING NET                           
         BNE   PBY17                                                            
         ICM   R4,15,PAGYCOM       YES - SUBTRACT AGYCOM                        
         CVD   R4,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
PBY17    CLI   CDSW,1              SEE IF SUBTRACTING CD                        
         BE    PBY18               NO                                           
         ICM   R4,15,PCSHDSC                                                    
         CVD   R4,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
PBY18    MVC   32(8,R3),DUB                                                     
*                                  DETAIL BILLED                                
         MVC   DUB,48(R3)                                                       
         ICM   R4,15,BGROSS                                                     
         CVD   R4,DOUBLE                                                        
         AP    DUB,DOUBLE                                                       
         CLI   NETSW,1             SEE IF SHOWING NET                           
         BNE   PBY19                                                            
         ICM   R4,15,BAGYCOM       YES - SUBTRACT AGYCOM                        
         CVD   R4,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
PBY19    CLI   CDSW,1              SEE IF SUBTRACTING CD                        
         BE    PBY20               NO                                           
         ICM   R4,15,BCSHDSC                                                    
         CVD   R4,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
PBY20    MVC   48(8,R3),DUB                                                     
         B     PP54EX                                                           
         EJECT                                                                  
*                                  ESTIMATE BREAK                               
*                                  PRINT MONTHLY DETAIL LINES                   
*                                  ACCUM TOTALS & PRINT                         
ESTBK    LA    R2,DTELST                                                        
         LA    R3,ACCUM                                                         
         LA    R6,23                                                            
EB02     LR    R7,R3                                                            
         CLI   0(R2),0                                                          
         BE    EB10                                                             
*                                                                               
         ST    R3,FULL                                                          
         LA    RE,8                8 SETS                                       
EB02AA   CP    0(8,R3),=P'0'                                                    
         BE    *+12                                                             
         L     R3,FULL                                                          
         B     EB02A                                                            
*                                                                               
         LA    R3,8(R3)                                                         
         BCT   RE,EB02AA                                                        
         L     R3,FULL                                                          
*                                                                               
         LA    R3,64(R3)           NEXT ROW IN ACCUM TABLE                      
         B     EB09                                                             
EB02A    XC    P(132),P                                                         
         XR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         BAS   R8,PCVD                                                          
         MVC   DATE(2),DUB+6                                                    
         IC    R0,1(R2)                                                         
         BAS   R8,PCVD                                                          
         MVC   DATE+2(2),DUB+6                                                  
         MVC   DATE+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,DATE,(X'09',P)                                       
*                                                                               
EB03     DS    0H                                                               
         EDIT  (P8,0(R3)),(14,P+16),2,MINUS=YES,COMMAS=YES                      
         EDIT  (P8,8(R3)),(14,P+31),2,MINUS=YES,COMMAS=YES                      
         EDIT  (P8,16(R3)),(11,P+47),2,MINUS=YES,COMMAS=YES                     
         EDIT  (P8,24(R3)),(14,P+59),2,MINUS=YES,COMMAS=YES                     
         EDIT  (P8,32(R3)),(14,P+73),2,MINUS=YES,COMMAS=YES                     
         EDIT  (P8,40(R3)),(14,P+88),2,MINUS=YES,COMMAS=YES                     
         EDIT  (P8,48(R3)),(14,P+103),2,MINUS=YES,COMMAS=YES                    
*                                                                               
         MVC   DUB,24(R3)                                                       
         CLI   CDSW,0              SEE IF SUBTRACTING CD                        
         BE    *+10                YES                                          
         MVC   DUB,8(R3)           ELSE USE GROSS (BILL MTH)                    
         SP    DUB,40(8,R3)                                                     
         SP    DUB,48(8,R3)                                                     
         MVC   56(8,R3),DUB                                                     
         EDIT  (P8,56(R3)),(14,P+118),2,MINUS=YES,COMMAS=YES                    
         LA    R4,ACCUM+1472                                                    
         LA    R5,8                                                             
EB08     AP    0(8,R4),0(8,R3)                                                  
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R5,EB08                                                          
         MVC   HEAD4+48(22),STHD                                                
         MVC   HEAD3+49(20),TDHD                                                
         MVI   RCSUBPRG,0                                                       
         CLI   NETSW,1             SEE IF SHOWING NET CLEARED + BILLED          
         BNE   *+8                                                              
         MVI   RCSUBPRG,10                                                      
         GOTO1 REPORT                                                           
EB09     LA    R2,2(R2)                                                         
         BCT   R6,EB02                                                          
EB10     CLI   SW,1                                                             
         BE    EBEX                                                             
         MVI   SW,1                                                             
         LA    R3,ACCUM+1472                                                    
         MVC   P+1(5),=C'TOTAL'                                                 
         LA    R6,1                                                             
         MVI   SPACING,2                                                        
         B     EB03                                                             
*                                                                               
EBEX     LA    R2,ACCUM                                                         
         LA    R3,192              24 SETS OF 8PL8                              
EBXA     ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,EBXA                                                          
*                                                                               
         XC    DTELST(124),DTELST                                               
         B     PP54EX                                                           
         EJECT                                                                  
*                                  PRODUCT BREAK                                
*                                  SKIP TO A NEW PAGE                           
*                                                                               
PRDBK    MVI   FORCEHED,C'Y'                                                    
         CLI   DTELST,0            CHK FOR ACTIVITY                             
         BE    PP54EX              NONE                                         
*                                                                               
PRDBX    LA    R2,ACCUM                                                         
         LA    R3,192              24 SETS OF 8PL8                              
PRDBXA   ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,PRDBXA                                                        
*                                                                               
         XC    DTELST(124),DTELST  NEEDED IF DOING ONE EST-ALL PRDS             
         B     PP54EX                                                           
*                                  ON RUN FIRST, TURN ON DMBITS                 
*                                  TO HAVE DELETED RECORDS PASSED               
*                                                                               
FXDELE   OI    DMINBTS,X'08'                                                    
         OI    DMOUTBTS,X'FD'                                                   
         B     PP54EX                                                           
*                                                                               
*                                  START OF REQUEST                             
*                                  SET UP SCHEDULE DATES IF                     
*                                  EST = ALL.                                   
*                                                                               
SCHDTE   DS    0H                                                               
         MVI   NETSW,0             DEFAULT IS GROSS                             
         CLI   QOPT1,C'N'                                                       
         BNE   *+8                                                              
         MVI   NETSW,1             SET TO SHOW NET                              
         XC    DTELST(WKX-DTELST),DTELST                                        
SD01     LA    R2,ACCUM                                                         
         LA    R3,192              24 SETS OF 8PL8                              
SD02     ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,SD02                                                          
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         CLI   QSTART,C'0'                                                      
         BL    PP54EX                                                           
         MVC   STHD+9(4),=C'THRU'                                               
         GOTO1 DATCON,DMCB,QSTART,(X'05',STHD)                                  
         GOTO1 DATCON,DMCB,QEND,(X'05',STHD+14)                                 
         MVC   HEAD4+48(22),STHD                                                
         CLI   QBPDATE,C' '                                                     
         BE    SD04                                                             
         MVC   TDHD,=CL20'** BILLABLE DATES **'                                 
         CLI   QBPDATE,C'B'                                                     
         BE    SD04                                                             
         MVC   TDHD,=CL20'** PAYABLE DATES **'                                  
SD04     DS    0H                                                               
         B     PP54EX                                                           
*                                                                               
*                                                                               
*                                  END OF REQUEST                               
*                                  SKIP TO NEW PAGE                             
*                                                                               
REQBK    MVI   FORCEHED,C'Y'                                                    
         B     PP54EX                                                           
*                                                                               
*                                  CONVERT TO DECIMAL                           
*                                                                               
PCVD     CVD   R0,WORK                                                          
         UNPK  DUB,WORK(8)                                                      
         OI    DUB+7,X'F0'                                                      
         BR    R8                                                               
         EJECT                                                                  
         LTORG                                                                  
         DS    CL2000                                                           
*                                                                               
         EJECT                                                                  
PP54WRKD DSECT                                                                  
*                                  ACCUMULATORS - 1F FOR EACH MONTH             
*                                  (UP TO 13 MONTHS) PLUS THE TOTAL.            
*                                  INSERT GROSS       0                         
*                                  BILL GROSS         8                         
*                                  CASH DISCOUNT     16                         
*                                  GROSS LESS CD     24                         
*                                  PAID              32                         
*                                  SUMMARY BLLD      40                         
*                                  DETAIL BLLD       48                         
*                                  BILLABLE          56                         
         DS    0D                                                               
ACCUM    DS    192PL8              24 SETS OF 8PL8                              
*                                                                               
CDSW     DS    CL1                 SET FROM PROFILE Y=0 (SUBTRACT CD)           
NETSW    DS    CL1                 SET FROM PROFILE N=O,Y=1(SHOW NET)           
*                                  LIST OF MONTHS FOR ESTIMATE (YM)             
*                                  EOL = X'00'                                  
         DS    0F                                                               
DTELST   DS    CL48                                                             
*                                                                               
BILLDT   DS    CL2                                                              
INSDTE   DS    CL2                                                              
SW       DS    CL1                                                              
*                                  N=1 (DON'T SUBTRACT CD)                      
FRSTIM   DS    CL1                                                              
DATE     DS    CL6                                                              
SVKEYS   DS    CL64                                                             
STHD     DS    CL22                                                             
TDHD     DS    CL20                                                             
LOWDTE   DS    XL3                                                              
HIDTE    DS    XL3                                                              
WKX      EQU   *                                                                
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE         HAVE NEW PBILLREC DSECT                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067PPREP5402 12/09/03'                                      
         END                                                                    
