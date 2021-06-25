*          DATA SET ACREPPA02  AT LEVEL 008 AS OF 05/01/02                      
*PHASE ACPA02A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE COVAIL                                                                 
*INCLUDE UNDERLIN                                                               
*INCLUDE ACSLRY                                                                 
*INCLUDE ACSALHST                                                               
         TITLE 'ACPA02 - PROJECT ALLOCATION'                                    
ACPA02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACPA**                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING ACPA02+4096,R9                                                   
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACPA02D,RC                                                       
         EJECT                                                                  
PA1      CLI   MODE,RUNFRST                                                     
         BNE   PA10                                                             
         RELOC RELO                                                             
         LA    RE,RELOTAB          RELOCATE A-TYPES                             
         LA    R1,ATYPES                                                        
RELOOP   L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
         SPACE 1                                                                
         XC    ID,ID                                                            
         MVC   ID(2),ORIGINUM                                                   
         MVC   ID+2(3),=C'APA'                                                  
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
         SPACE 1                                                                
         ZAP   TOTCNT,=P'0'                                                     
         ZAP   TOTCSH,=P'0'                                                     
         ZAP   DDSDBT,=P'0'                                                     
         ZAP   DDSCRT,=P'0'                                                     
         ZAP   RUNCOST,=P'0'                                                    
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAY)                                 
         LA    RF,SALARE2                                                       
         USING SALARYD,RF                                                       
         XC    SALBLOCK(SALLNQ),SALBLOCK                                        
         DROP  RF                                                               
         SPACE 1                                                                
PAXIT    XIT1                                                                   
         EJECT                                                                  
PA10     CLI   MODE,REQFRST                                                     
         BNE   PA20                                                             
         MVI   OPTIONS,0                                                        
         CLI   QOPT1,C'P'                                                       
         BNE   *+8                                                              
         OI    OPTIONS,POST                                                     
         SPACE 1                                                                
         ZAP   REQHRS,=P'0'                                                     
         ZAP   REQCOST,=P'0'                                                    
         ZAP   POSTCOST,=P'0'                                                   
         MVC   MOS(1),QSTART+1     YEAR                                         
         ZIC   R1,QSTART+3          MONTH                                       
         CLI   QSTART+2,C'1'                                                    
         BNE   *+8                                                              
         SH    R1,=H'47'                                                        
         STC   R1,MOS+1            10=A, 11=B, 12=C                             
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,PDATE)                                   
         GOTO1 DATCON,DMCB,(0,WORK),(6,MONTH)                                   
         MVC   STEND(2),PDATE                                                   
         MVC   STEND+2(2),PDATE                                                 
         SPACE 1                                                                
         MVI   RCSUBPRG,0                                                       
         L     R4,ADCMPNAM                                                      
         LA    R6,COMPNAM                                                       
         BAS   RE,NAMOUT                                                        
         SPACE 1                                                                
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         LA    RF,SALARE2                                                       
         USING SALARYD,RF                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(8),SALBLOCK+8   ADDR AND LENGTH OF ACQUIRED BUFFER          
         XC    SALBLOCK(SALLNQ),SALBLOCK                                        
         MVC   SALBLOCK+8(8),WORK   RESTORE ADDR/LEN OF BUFFER                  
         MVC   SALCMPY,RCCOMPFL                                                 
         MVI   SALMETHD,C'1'        SET DEFAULT TO BE METHOD 1                  
         MVC   SALSTART(2),PDATE                                                
         MVI   SALSTART+2,X'01'                                                 
         MVC   SALEND(2),SALSTART                                               
         MVI   SALEND+2,X'31'                                                   
         MVC   SALACOVL,COVAIL                                                  
         OI    SALSTAT1,SALHRRAT   TURN ON INCLUDE HOURLY RATES                 
         LA    R0,SALACTM#         NUMBER OF MONTHLY HRLY ACCUMS                
         LA    R1,SALACTM1                                                      
         ZAP   0(L'SALACTM1,R1),=P'0'                                           
         LA    R1,L'SALACTM1(R1)                                                
         BCT   R0,*-10                                                          
         B     PAXIT                                                            
         EJECT                                                                  
PA20     CLI   MODE,LEDGFRST                                                    
         BNE   PA30                                                             
         L     R4,ADLDGHIR                                                      
         USING ACHEIRD,R4                                                       
         SR    R2,R2                                                            
         LA    R3,ACHRLEVA                                                      
         L     R5,LEVEL                                                         
         MVI   LONGDES,7                                                        
         SPACE 1                                                                
         USING LEVD,R5                                                          
PA22     CLI   0(R3),0                                                          
         BE    PA24                                                             
         LA    R2,1(R2)                                                         
         STC   R2,NUMLEV                                                        
         MVC   LEVLN,0(R3)                                                      
         MVC   LEVDES,1(R3)                                                     
         OC    LEVDES,SPACES                                                    
         GOTO1 SQUASHER,DMCB,LEVDES,15                                          
         ZIC   R6,DMCB+7                                                        
         ZIC   R7,LONGDES                                                       
         CR    R6,R7                                                            
         BL    *+8                                                              
         STC   R6,LONGDES                                                       
         CH    R2,=H'4'                                                         
         BE    PA24                                                             
         LA    R5,LEVLEN(R5)                                                    
         LA    R3,16(R3)                                                        
         B     PA22                                                             
         SPACE 1                                                                
PA24     DC    0H'0'                                                            
         L     R5,LEVEL                                                         
         LA    R5,LEVLEN(R5)                                                    
         CLI   NUMLEV,4                                                         
         BNE   *+8                                                              
         LA    R5,LEVLEN(R5)                                                    
         MVC   LOWDESC(15),LEVDES                                               
         LA    R5,LEVLEN(R5)                                                    
         MVI   LOWDESC+15,C'/'                                                  
         MVC   LOWDESC+16(15),LEVDES                                            
         GOTO1 SQUASHER,DMCB,LOWDESC,31                                         
         B     PAXIT                                                            
         EJECT                                                                  
PA30     CLI   MODE,LEVAFRST                                                    
         BNE   PA40                                                             
         MVC   OFFIC,SPACES                                                     
         L     R5,LEVEL                                                         
         USING LEVD,R5                                                          
         MVC   LEVHRS(32),=4PL8'0'                                              
         L     R2,ADHEIRA                                                       
         MVC   LEVACC,3(R2)                                                     
         MVC   OFFIC,LEVACC                                                     
         L     R4,ADLVANAM                                                      
         LA    R6,LEVNAM                                                        
         BAS   RE,NAMOUT                                                        
         MVI   FORCEHED,C'Y'                                                    
         B     PAXIT                                                            
         EJECT                                                                  
PA40     CLI   MODE,LEVBFRST                                                    
         BNE   PA50                                                             
         L     R5,LEVEL                                                         
         USING LEVD,R5                                                          
         ZIC   R1,LEVLN            LENGTH OF LEVEL A                            
         LA    RF,SALARE2                                                       
         USING SALARYD,RF                                                       
         STC   R1,SALLEVA          SAVE INDIVD LENGTH OF LEVEL                  
         LA    R5,LEVLEN(R5)                                                    
         MVC   LEVHRS(32),=4PL8'0'                                              
         MVC   LEVACC,SPACES                                                    
         MVC   DEPART,SPACES                                                    
         L     R2,ADHEIRB                                                       
         LA    R2,3(R1,R2)        ACCOUNT AT LEVEL B                            
         ZIC   R3,LEVLN                                                         
         SR    R3,R1                                                            
         STC   R3,SALLEVB         SAVE INDIVD LENGTH OF LEVEL                   
         DROP  RF                                                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LEVACC(0),0(R2)                                                  
         MVC   DEPART,LEVACC                                                    
         SPACE 1                                                                
         L     R4,ADLVBNAM                                                      
         LA    R6,LEVNAM                                                        
         BAS   RE,NAMOUT                                                        
         SPACE 1                                                                
         MVI   FRSTSUB,C'Y'                                                     
         CLI   NUMLEV,4                                                         
         BNE   PA45                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVI   FRSTSUB,C'N'                                                     
         SPACE 1                                                                
PA45     DC    0H'0'                                                            
         B     PAXIT                                                            
         EJECT                                                                  
PA50     CLI   MODE,LEVCFRST                                                    
         BNE   PA60                                                             
         L     R5,LEVEL                                                         
         USING LEVD,R5                                                          
         LA    R5,LEVLEN(R5)                                                    
         ZIC   R1,LEVLN            LENGTH OF LEVEL B                            
         LA    R5,LEVLEN(R5)                                                    
         MVC   LEVHRS(32),=4PL8'0'                                              
         MVC   LEVACC,SPACES                                                    
         MVC   SDEPART,SPACES                                                   
         SPACE 1                                                                
         L     R2,ADHEIRC                                                       
         LA    R2,3(R1,R2)         ACCOUNT AT LEVEL C                           
         ZIC   R3,LEVLN                                                         
         SR    R3,R1                                                            
         LA    RF,SALARE2                                                       
         USING SALARYD,RF                                                       
         STC   R1,SALLEVC          SAVE INDIVD LENGTH OF LEVEL                  
         DROP  RF                                                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LEVACC(0),0(R2)                                                  
         MVC   SDEPART,LEVACC                                                   
         SPACE 1                                                                
         L     R4,ADLVCNAM                                                      
         LA    R6,LEVNAM                                                        
         BAS   RE,NAMOUT                                                        
         MVI   FRSTSUB,C'Y'                                                     
         SPACE 1                                                                
         B     PAXIT                                                            
         EJECT                                                                  
PA60     CLI   MODE,PROCACC                                                     
         BNE   PA70                                                             
         MVI   ACTIVITY,C'N'                                                    
         L     R5,LEVEL                                                         
         USING LEVD,R5                                                          
         LA    R5,LEVLEN(R5)       LENGTH OF LEVEL B                            
         CLI   NUMLEV,4                                                         
         BNE   *+8                                                              
         LA    R5,LEVLEN(R5)       OR C IF THERE ARE 4                          
         ZIC   R1,LEVLN                                                         
         LA    R5,LEVLEN(R5)                                                    
         MVC   LEVHRS(32),=4PL8'0'                                              
         MVC   LEVACC,SPACES                                                    
         MVC   PERSON,SPACES                                                    
         ST    R5,LOWACC                                                        
         SPACE 1                                                                
         L     R2,ADHEIRC                                                       
         CLI   NUMLEV,4                                                         
         BNE   *+8                                                              
         L     R2,ADHEIRD                                                       
         LA    R2,3(R1,R2)                                                      
         ZIC   R3,LEVLN                                                         
         SR    R3,R1                                                            
         LA    RF,SALARE2                                                       
         USING SALARYD,RF                                                       
         STC   R1,SALLEVD          SAVE INDIVD LENGTH OF LEVEL                  
         DROP  RF                                                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LEVACC(0),0(R2)                                                  
         MVC   PERSON,LEVACC                                                    
         SPACE 1                                                                
         L     R4,ADLVCNAM                                                      
         CLI   NUMLEV,4                                                         
         BNE   *+8                                                              
         L     R4,ADLVDNAM                                                      
         LA    R6,LEVNAM                                                        
         BAS   RE,NAMOUT                                                        
         SPACE 1                                                                
         MVI   FCRDTRNS,C'N'                                                    
*        L     R4,ADACC                                                         
*        MVI   ELCODE,X'52'                                                     
*        BAS   RE,GETEL                                                         
*        BNE   PAXIT               IGNORE IF NO SALARY ELEMENT                  
         L     R4,ADACC                                                         
         CLI   3(R4),C'9'          OR OVERHEAD ACCOUNTS                         
         BE    PAXIT                                                            
         CLC   4(2,R4),=C'999'                                                  
         BE    PAXIT                                                            
         CLC   8(3,R4),=C'999'                                                  
         BE    PAXIT                                                            
         MVI   FCRDTRNS,C'Y'                                                    
         B     PAXIT                                                            
         EJECT                                                                  
PA70     CLI   MODE,PROCTRNS                                                    
         BNE   PA80                                                             
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   TRNSEL,X'44'                                                     
         BNE   PAXIT                                                            
         CLC   TRNSBTCH(2),MOS                                                  
         BNE   PAXIT                                                            
         SPACE 1                                                                
         USING LEVD,R5                                                          
         USING TASKD,R6                                                         
         L     R5,LOWACC                                                        
         LA    R6,WRKA                                                          
         XC    WRKA,WRKA                                                        
         MVC   TSKDTE,TODAY                                                     
*        AP    LEVCOST,TRNSAMNT    ADD TO TOTAL COST                            
         MVI   ELCODE,X'50'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   PA72                                                             
         SPACE 1                                                                
         USING TRCASHD,R4                                                       
         CLI   TRCSTYPE,C'H'                                                    
         BNE   PA72                                                             
         AP    LEVHRS,TRCSAMNT     ADD TO TOTAL HOURS                           
         MVI   ACTIVITY,C'Y'                                                    
         ZAP   TSKHRS,TRCSAMNT                                                  
         ZAP   TSKCOST,=P'0'                                                    
         SPACE 1                                                                
PA72     L     R4,ADTRANS                                                       
         MVI   ELCODE,X'51'                                                     
         BAS   RE,NEXTEL           NO PROJECT ELEMENT                           
         BNE   PAXIT                                                            
         SPACE 1                                                                
         USING ACPCD,R4                                                         
         CLI   ACPCLEN,X'22'                                                    
         BL    PAXIT               NO PROJECT OR TASK                           
         SPACE 1                                                                
PA74     MVC   TSKCLI(14),ACPCPRJT+3 CLI/DIV/PROJECT/TASK                       
         CLC   TSKPRJ,SPACES                                                    
         BNH   PAXIT               NO JOB CAN'T POST AT THIS LEVEL              
         CLC   TSKCDE,SPACES                                                    
         BNH   PAXIT               NO WORK CODE NO POST AT THIS LEVEL           
         BAS   RE,PRJNME           GET PROJECT NAME INTO TABLE                  
         L     R3,ADTRANS                                                       
         SH    R3,DATADISP                                                      
         USING ACKEYD,R4                                                        
         L     R4,RECORD                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(42),0(R3)                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                CAN'T READ TRANSACTION                       
         CLC   WORK,SPACES                                                      
         BE    PAXIT               SKIP DELETED PROJECTS                        
         ZAP   TSKPHRS,=P'0'                                                    
         ZAP   TSKPCST,=P'0'                                                    
         ZAP   TSKCNT,=P'1'  FORCE RETURN OF ALL RECORDS FROM BUFFALO           
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,(R6)                                  
         B     PAXIT                                                            
         EJECT                                                                  
PA80     CLI   MODE,ACCLAST                                                     
         BNE   PA90                                                             
         MVC   PROFS(16),PROGPROF                                               
         L     R5,LOWACC                                                        
         USING LEVD,R5                                                          
*                                                                               
         USING SALARYD,R4                                                       
         LA    R4,SALARE2                                                       
         ZAP   DUB,=P'0'                                                        
         MVC   SALOFFC(L'SALOFFC+L'SALDEPT+L'SALSDPT+L'SALPRSN),OFFIC           
         CLC   ALPHAID,=C'BD'                                                   
         BE    *+14                                                             
         CLC   ALPHAID,=C'BP'                                                   
         BNE   PA80_A                                                           
         GOTO1 ACSALHST,DMCB,ACWORKD,(R4),IO2                                   
         OC    SALSTAT2,SALSTAT2   ANY ERRORS?                                  
         BNZ   PA80_B                                                           
         ZAP   DUB,SALSALRY                                                     
         CLI   PROF1,C'Y'          SALARY ONLY - NO BENEFITS                    
         BE    *+16                                                             
         AP    DUB,SALBENFT                                                     
         AP    DUB,SALPENSN                                                     
         B     PA80_B                                                           
*                                                                               
PA80_A   L     R4,ADACC                                                         
*MN      GOTO1 ACSLRY,DMCB,(R4),STEND,SALAREA                                   
         GOTO1 ACSLRY,DMCB,(X'80',(R4)),STEND,SALAREA,ADCOMFAC                  
         LA    R4,SALAREA                                                       
         USING SLRD,R4                                                          
         ZAP   DUB,SLRTOT                                                       
         CLI   PROF1,C'Y'          SALARY ONLY - NO BENEFITS                    
         BNE   *+10                                                             
         ZAP   DUB,SLRSAL                                                       
PA80_B   ZAP   LEVCOST,DUB                                                      
         CLI   ACTIVITY,C'Y'                                                    
         BE    *+14                                                             
         CP    LEVHRS,=P'0'        NO HOURS                                     
         BE    PA89                                                             
         CP    LEVCOST,=P'0'       OR COST                                      
         BE    PA89                                                             
         CLI   PROF2,0             STANDARD HOURS                               
         BE    PA80A                                                            
         ZIC   RE,PROF2                                                         
         MH    RE,=H'100'                                                       
         CVD   RE,DUB                                                           
         ZAP   LEVHRS,DUB                                                       
         CLI   PROF2,1             1 = 133.33                                   
         BNE   PA80A                                                            
         ZAP   LEVHRS,=P'13333'                                                 
PA80A    CLI   PROF5,C'D'          PROFILE 3 AND 4 ARE DOLLARS                  
         BNE   PA80A1                                                           
         ZIC   RE,PROF3            DOLLARS                                      
         MH    RE,=H'100'                TIMES 100                              
         ZIC   R1,PROF4                  DECIMAL PLACE                          
         AR    RE,R1               ADD TOGETHER                                 
         CVD   RE,DUB                                                           
         ZAP   CPH,DUB                                                          
         B     PA80B                                                            
PA80A1   ZAP   WORK(12),LEVCOST                                                 
         MP    WORK(12),=P'1000'                                                
         CP    LEVHRS,=P'0'                                                     
         BE    PA89                                                             
         DP    WORK(12),LEVHRS+5(3)     COST / HOURS                            
         AP    WORK(9),=P'5'                                                    
         DP    WORK(9),=P'10'                                                   
         ZAP   CPH,WORK+3(4)                                                    
         CLI   PROF3,0                                                          
         BE    PA80B               BILLING FACTOR                               
         ZIC   RE,PROF3            UNITS                                        
         MH    RE,=H'100'                TIMES 100                              
         ZIC   R1,PROF4                  DECIMAL PLACE                          
         AR    RE,R1               ADD TOGETHER                                 
         CVD   RE,DUB                                                           
         ZAP   WORK(12),CPH                                                     
         MP    WORK(12),DUB+4(4)   MULTIPLY BY COST/HOURS                       
         DP    WORK(12),=P'10'     AND ROUND                                    
         AP    WORK(10),=P'5'                                                   
         DP    WORK(10),=P'10'                                                  
         ZAP   CPH,WORK+4(4)       PUT BACK IN COST/HOURS                       
         SPACE 1                                                                
         USING TASKD,R6                                                         
PA80B    LA    R6,WRKA                                                          
         MVC   WRKA,SPACES                                                      
         ZAP   ITEMS,=P'0'                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,(R6),1                               
         TM    DMCB+8,X'80'                                                     
         BO    PA89                                                             
         BAS   RE,PRNTIT                                                        
         MVC   WORK,SPACES                                                      
         MVC   WORK(12),LEVACC                                                  
         MVC   WORK+13(36),LEVNAM                                               
         GOTO1 SQUASHER,DMCB,WORK,49                                            
         GOTO1 CHOPPER,DMCB,(49,WORK),(40,P+1),(C'P',2)                         
         B     PA82                                                             
         SPACE 1                                                                
PA81     GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,(R6),1                                
         TM    DMCB+8,X'80'                                                     
         BO    PA85                                                             
         SPACE 1                                                                
PA82     MVC   P+42(3),TSKCLI                                                   
         MVC   P+47(3),TSKDIV                                                   
         MVC   P+51(6),TSKPRJ                                                   
         MVC   P+88(2),TSKCDE                                                   
         ZAP   WORK(12),CPH                                                     
         MP    WORK(12),TSKHRS+5(3)                                             
         DP    WORK(12),=PL2'100'                                               
         ZAP   TSKCOST,WORK+2(8)                                                
         LA    R2,TSKHRS                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,PRJNME           GET PROJECT NAME                             
         GOTO1 CHOPPER,DMCB,(36,WORK),(28,P+58),(C'P',2)                        
         BAS   RE,PRNTIT                                                        
         AP    ITEMS,=P'1'                                                      
         BAS   RE,PRIOR            GET PREVIOUS POSTINGS                        
         BAS   RE,POSTIT                                                        
         SPACE 1                                                                
         AP    LEVTHRS,TSKHRS                                                   
         AP    LEVTCOST,TSKCOST                                                 
         B     PA81                                                             
         SPACE 1                                                                
PA85     CP    ITEMS,=P'0'                                                      
         BE    PA89                                                             
         CP    ITEMS,=P'1'                                                      
         BNE   PA86                                                             
         MVC   P,SPACES                                                         
         B     PA87                                                             
         SPACE 1                                                                
PA86     MVC   P+42(10),=C'TOTALS FOR'                                          
         MVC   P+53(36),LEVNAM                                                  
         LA    R2,LEVTHRS                                                       
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
PA87     BAS   RE,PRNTIT                                                        
         LA    R3,LEVLEN                                                        
         LR    R6,R5                                                            
         SR    R6,R3               R6 TO HIGHER LEVEL                           
         AP    LEVTHRS-LEVLN(L'LEVTHRS,R6),LEVTHRS                              
         AP    LEVTCOST-LEVLN(L'LEVTCOST,R6),LEVTCOST                           
         SPACE 1                                                                
PA89     GOTO1 BUFFALO,DMCB,=C'CLEAR',ABUFF,(X'80',1)                           
         L     R3,ADACC                                                         
         USING ACKEYD,R4                                                        
         L     R4,RECORD                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC,0(R3)                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         CLI   DMCB+8,0                                                         
         BE    PAXIT                                                            
         DC    H'0'                CAN'T READ ACCOUNT                           
         EJECT                                                                  
PA90     CLI   MODE,LEVCLAST                                                    
         BNE   PA100                                                            
         MVI   FRSTSUB,C'N'                                                     
         CLI   NUMLEV,4                                                         
         BNE   PAXIT               NOT INTERESTED IF 3 LEVELS                   
         L     R6,LEVEL                                                         
         LA    R6,LEVLEN(R6)       R6 TO LEVEL B                                
         LA    R5,LEVLEN(R6)       R5 TO LEVEL C                                
         SPACE 1                                                                
         USING LEVD,R5                                                          
         CLC   LEVTHRS(16),=2PL8'0'                                             
         BE    PAXIT                                                            
         AP    LEVTHRS-LEVLN(L'LEVTHRS,R6),LEVTHRS                              
         AP    LEVTCOST-LEVLN(L'LEVTCOST,R6),LEVTCOST                           
         MVC   P+42(10),=C'TOTALS FOR'                                          
         MVC   P+53(36),LEVNAM                                                  
         LA    R2,LEVTHRS                                                       
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     PAXIT                                                            
         EJECT                                                                  
PA100    CLI   MODE,LEVBLAST                                                    
         BNE   PA110                                                            
         MVI   FRSTSUB,C'N'                                                     
         L     R6,LEVEL            R6 TO LEVEL A                                
         LA    R5,LEVLEN(R6)       R5 TO LEVEL B                                
         SPACE 1                                                                
         USING LEVD,R5                                                          
         CLC   LEVTHRS(16),=2PL8'0'                                             
         BE    PAXIT                                                            
         AP    LEVTHRS-LEVLN(L'LEVTHRS,R6),LEVTHRS                              
         AP    LEVTCOST-LEVLN(L'LEVTCOST,R6),LEVTCOST                           
         MVC   P+42(10),=C'TOTALS FOR'                                          
         MVC   P+53(36),LEVNAM                                                  
         LA    R2,LEVTHRS                                                       
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         BAS   RE,PRNTIT                                                        
         B     PAXIT                                                            
         EJECT                                                                  
PA110    CLI   MODE,LEVALAST                                                    
         BNE   PA120                                                            
         L     R5,LEVEL                                                         
         SPACE 1                                                                
         USING LEVD,R5                                                          
         CLC   LEVTHRS(16),=2PL8'0'                                             
         BE    PAXIT                                                            
         AP    REQHRS,LEVTHRS                                                   
         AP    REQCOST,LEVTCOST                                                 
         MVC   P+42(10),=C'TOTALS FOR'                                          
         MVC   P+53(36),LEVNAM                                                  
         LA    R2,LEVTHRS                                                       
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         MVI   FRSTSUB,C'N'                                                     
         BAS   RE,PRNTIT                                                        
         B     PAXIT                                                            
         EJECT                                                                  
PA120    CLI   MODE,REQLAST                                                     
         BNE   PA130                                                            
         MVI   FRSTSUB,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,POSTLAST         ONE BIG CREDIT PER REQUEST                   
         MVC   P+42(18),=C'TOTALS FOR REQUEST'                                  
         LA    R2,REQHRS                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTIT                                                        
         TM    OPTIONS,POST                                                     
         BZ    PAXIT                                                            
         AP    TOTCSH,POSTCOST                                                  
         AP    DDSDBT,POSTCOST                                                  
         AP    RUNCOST,POSTCOST                                                 
         B     PAXIT                                                            
         EJECT                                                                  
PA130    CLI   MODE,RUNLAST                                                     
         BNE   PAXIT                                                            
         BAS   RE,POSTEND                                                       
         CP    TOTCNT,=P'0'                                                     
         BE    PAXIT                                                            
         SPACE 1                                                                
         USING LOGOD,R4                                                         
         L     R4,LOGOC                                                         
         SPACE 1                                                                
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 LOGO,DMCB,(R4)                                                   
         SPACE 1                                                                
         MVI   LOGOEND,C'X'                                                     
         MVI   LOGOTYPE,C'S'                                                    
         MVC   LOGONAME,=CL33'******** INTERNAL CONTROL *******'                
         MVC   LOGOADD,=CL33'******** DO NOT SEND OUT ******'                   
         MVC   LOGO1,=C'CONTROL'                                                
         GOTO1 LOGO,DMCB,(R4)                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         SPACE 1                                                                
         MVI   RCSUBPRG,3                                                       
         EDIT  (P8,DDSDBT),(12,P+44),2,MINUS=YES                                
         EDIT  (P8,DDSCRT),(12,P+65),2,MINUS=YES                                
         BAS   RE,PRNTIT                                                        
         B     PAXIT                                                            
         EJECT                                                                  
PRNTIT   NTR1                                                                   
         TM    OPTIONS,POST                                                     
         BO    *+10                                                             
         MVC   HEAD6+84(5),=C'DRAFT'                                            
         ZIC   R3,LONGDES                                                       
         LA    R2,HEAD4+1                                                       
         LA    R2,2(R3,R2)                                                      
         MVC   0(36,R2),COMPNAM    COMPANY NAME                                 
         CLI   MODE,REQLAST                                                     
         BE    PRNT4                                                            
         CLI   MODE,RUNLAST                                                     
         BE    PRNT5                                                            
         SPACE 1                                                                
         LA    R2,132(R2)                                                       
         L     R5,LEVEL            LEVEL A  ACCOUNT AND NAME                    
         USING LEVD,R5                                                          
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   HEAD5+1(0),LEVDES                                                
         MVC   0(48,R2),LEVACC                                                  
         GOTO1 SQUASHER,DMCB,(R2),48                                            
         SPACE 1                                                                
         CLI   MODE,LEVALAST                                                    
         BE    PRNT4                                                            
         CLI   NUMLEV,4                                                         
         BNE   PRNT3                                                            
         LA    R2,132(R2)          LEVEL B ACCOUNT AND NAME                     
         LA    R5,LEVLEN(R5)                                                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   HEAD6+1(0),LEVDES                                                
         MVC   0(48,R2),LEVACC                                                  
         GOTO1 SQUASHER,DMCB,(R2),48                                            
         SPACE 1                                                                
PRNT3    CLI   FRSTSUB,C'Y'        LEVEL C OR B                                 
         BNE   PRNT4                                                            
         LA    R5,LEVLEN(R5)                                                    
         MVC   MID1+1(48),LEVACC                                                
         GOTO1 SQUASHER,DMCB,MID1+1,48                                          
         GOTO1 UNDERLIN,DMCB,(48,MID1+1),(0,MID2+1)                             
         MVI   FORCEMID,C'Y'                                                    
         MVI   FRSTSUB,C'N'                                                     
         SPACE 1                                                                
PRNT4    MVC   HEAD5+98(6),MONTH                                                
         MVC   HEAD9+1(31),LOWDESC                                              
         GOTO1 UNDERLIN,DMCB,(31,HEAD9+1),(0,HEAD10+1)                          
PRNT5    GOTO1 ACREPORT                                                         
         B     PAXIT                                                            
         EJECT                                                                  
*              ROUTINE TO GET PREVIOUS POSTINGS FROM 1J                         
         SPACE 1                                                                
         USING TASKD,R6                                                         
         USING ACKEYD,R4                                                        
PRIOR    NTR1                                                                   
         L     R4,RECORD                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),QCOMPANY                                             
         MVC   ACKEYACC+1(2),=C'1J'                                             
         MVC   ACKEYACC+3(12),TSKCLI                                            
         MVC   ACKEYWRK,TSKCDE                                                  
         L     R3,ADACC                                                         
         MVC   ACKEYCON,0(R3)      CONTRA IS PERSON                             
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         CLI   DMCB+8,0                                                         
         BNE   PRIX                NOT FOUND GET OUT                            
         MVI   ELCODE,X'45'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRIHRS                                                           
         SPACE 1                                                                
         USING TRHISTD,R4                                                       
PRICST   CLC   TRHSYEAR(2),PDATE   GET COST POSTED FOR MONTH                    
         BE    PRICST2                                                          
         BAS   RE,NEXTEL                                                        
         BNE   PRIHRS                                                           
         B     PRICST                                                           
PRICST2  ZAP   TSKPCST,TRHSDR                                                   
         SPACE 1                                                                
PRIHRS   DC    0H'0'                                                            
         USING ACKEYD,R4                                                        
         L     R4,RECORD                                                        
         MVI   ACKEYREF,C'H'                                                    
         MVC   HRKEY,0(R4)         SAVE HOURS KEY                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',(R4),(R4)                    
         CLC   HRKEY,0(R4)                                                      
         BNE   PRIX                NOT FOUND GET OUT                            
         MVI   ELCODE,X'45'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRIX                                                             
         SPACE 1                                                                
         USING TRHISTD,R4                                                       
PRIHRS2  CLC   TRHSYEAR(2),PDATE   GET COST POSTED FOR MONTH                    
         BE    PRIHRS3                                                          
         BAS   RE,NEXTEL                                                        
         BNE   PRIX                                                             
         B     PRIHRS2                                                          
PRIHRS3  ZAP   TSKPHRS,TRHSDR                                                   
         SPACE 1                                                                
PRIX     DC    0H'0'                                                            
         B     PAXIT                                                            
         EJECT                                                                  
*              ROUTINE TO CREATE POSTING FILE                                   
         SPACE 1                                                                
         USING TASKD,R6                                                         
POSTIT   NTR1                                                                   
         L     R4,RECORD                                                        
         LA    R4,4(R4)                                                         
         USING PSHEADD,R4                                                       
         MVI   PSHDEL,X'50'                                                     
         MVC   PSHDLEN,=AL1(PSHDSBNM-PSHDEL+L'PSHDSBNM)                         
         MVC   PSHDACC(1),QCOMPANY                                              
         MVC   PSHDACC+1(2),=C'1J'                                              
         MVC   PSHDACC+3(12),TSKCLI                                             
         MVC   PSHDANAL,TSKCDE                                                  
         L     R3,ADACC                                                         
         MVC   PSHDSBAC,0(R3)                                                   
         LA    R6,PSHDSBNM                                                      
         LR    R5,R4                                                            
         L     R4,ADACCNAM                                                      
         BAS   RE,NAMOUT           PERSON NAME TO POSTING RECORD                
         LR    R4,R5                                                            
         LA    R6,WRKA                                                          
         SPACE 1                                                                
         ZIC   R3,PSHDLEN                                                       
         AR    R4,R3                                                            
         USING TRANSD,R4                                                        
         MVI   TRNSEL,X'44'                                                     
         MVC   TRNSLEN,=AL1(TRNSNARR-TRNSEL+1)                                  
         MVC   TRNSDATE,TSKDTE                                                  
         MVC   TRNSREF,MONTH                                                    
         MVI   TRNSSBRF,X'00'                                                   
         MVI   TRNSTYPE,X'91'                                                   
         MVI   TRNSSTAT,X'80'                                                   
         MVC   TRNSBTCH,SPACES                                                  
         MVC   TRNSBTCH(2),MOS                                                  
         MVC   TRNSANAL,TSKCDE                                                  
         MVI   TRNSNARR,X'40'                                                   
         ZAP   TRNSAMNT,TSKCOST                                                 
         SP    TRNSAMNT,TSKPCST    LESS PRIOR POSTING                           
         ZAP   DUB,TRNSAMNT        SAVE POSTING AMOUNT                          
         AP    POSTCOST,TRNSAMNT                                                
         ZIC   R3,TRNSLEN                                                       
         AR    R4,R3                                                            
         SPACE 1                                                                
         USING TRCASHD,R4                                                       
         MVC   TRCSEL(2),=X'5009'                                               
         MVI   TRCSTYPE,C'H'                                                    
         ZAP   TRCSAMNT,TSKHRS                                                  
         SP    TRCSAMNT,TSKPHRS                                                 
         CP    TRCSAMNT,=P'0'                                                   
         BNE   *+14                                                             
         CP    DUB,=P'0'                                                        
         BE    PAXIT               HOURS AND COST ARE ZERO                      
         ZIC   R3,TRCSLEN                                                       
         AR    R4,R3                                                            
         MVI   0(R4),0                                                          
         L     R3,RECORD                                                        
         XC    0(4,R3),0(R3)                                                    
         LA    R4,1(R4)                                                         
         SR    R4,R3                                                            
         STH   R4,0(R3)                                                         
         SPACE 1                                                                
         TM    OPTIONS,POST                                                     
         BZ    PAXIT                                                            
         AP    TOTCNT,=P'1'                                                     
         GOTO1 WORKER,DMCB,=C'ADD',POSTBUF,ID,RECORD                            
         B     PAXIT                                                            
         EJECT                                                                  
POSTLAST NTR1                                                                   
         L     R4,RECORD           POST ONE BIG CREDIT TO 1J                    
         LA    R4,4(R4)                                                         
         USING PSHEADD,R4                                                       
         MVI   PSHDEL,X'50'                                                     
         MVC   PSHDLEN,=AL1(PSHDSBNM-PSHDEL+L'PSHDSBNM)                         
         MVC   PSHDACC(1),QCOMPANY                                              
         MVC   PSHDACC+1(2),=C'1J'                                              
         MVC   PSHDACC+3(12),=12C'9'                                            
         MVC   PSHDANAL,=C'99'                                                  
         MVC   PSHDSBAC(15),PSHDACC                                             
         MVC   PSHDSBNM,=CL36'PROJECT ALLOCATION - POSTING CONTROL'             
         SPACE 1                                                                
         ZIC   R3,PSHDLEN                                                       
         AR    R4,R3                                                            
         USING TRANSD,R4                                                        
         MVI   TRNSEL,X'44'                                                     
         MVC   TRNSLEN,=AL1(TRNSNARR-TRNSEL+1)                                  
         MVC   TRNSDATE,TODAY                                                   
         MVC   TRNSREF,MONTH                                                    
         MVI   TRNSSBRF,X'00'                                                   
         MVI   TRNSTYPE,X'91'                                                   
         MVI   TRNSSTAT,0                                                       
         MVC   TRNSBTCH,SPACES                                                  
         MVC   TRNSBTCH(2),MOS                                                  
         MVC   TRNSANAL,=C'99'                                                  
         MVI   TRNSNARR,X'40'                                                   
         ZAP   TRNSAMNT,POSTCOST                                                
         ZIC   R3,TRNSLEN                                                       
         AR    R4,R3                                                            
         SPACE 1                                                                
         MVI   0(R4),0                                                          
         L     R3,RECORD                                                        
         XC    0(4,R3),0(R3)                                                    
         LA    R4,5(R4)                                                         
         SR    R4,R3                                                            
         STH   R4,0(R3)                                                         
         SPACE 1                                                                
         TM    OPTIONS,POST                                                     
         BZ    PAXIT                                                            
         AP    TOTCNT,=P'1'                                                     
         AP    DDSCRT,POSTCOST                                                  
         GOTO1 WORKER,DMCB,=C'ADD',POSTBUF,ID,RECORD                            
         B     PAXIT                                                            
         EJECT                                                                  
*              POST TOTAL RECORD                                                
POSTEND  NTR1                                                                   
         CP    TOTCNT,=P'0'                                                     
         BE    PAXIT                                                            
         L     R4,RECORD                                                        
         XC    0(256,R4),0(R4)                                                  
         USING PSSUBFD,R4                                                       
         MVC   0(2,R4),=H'34'                                                   
         MVC   2(2,R4),=H'0'                                                    
         LA    R4,4(R4)                                                         
         MVI   PSSBEL,X'52'                                                     
         MVI   PSSBLEN,X'1D'                                                    
         MVC   PSSBDESC,=CL15'PROJECT ALLOCAT'                                  
         ZAP   PSSBRECS,TOTCNT                                                  
         ZAP   PSSBCASH,TOTCSH                                                  
         MVI   PSSBCASH+6,0                                                     
         GOTO1 WORKER,DMCB,=C'ADD',POSTBUF,ID,RECORD                            
         GOTO1 WORKER,DMCB,=C'CLOSE',POSTBUF,ID                                 
         B     PAXIT                                                            
         EJECT                                                                  
*              ROUTINE TO GET PROJECT NAMES                                     
         SPACE 1                                                                
         USING TASKD,R6                                                         
         USING PRJD,R5                                                          
PRJNME   NTR1                                                                   
         MVC   WRKB,SPACES                                                      
         LA    R5,WRKB                                                          
         MVC   PRJK(1),QCOMPANY                                                 
         MVC   PRJK+1(2),=C'1J'                                                 
         MVC   PRJK+3(12),TSKCLI                                                
         L     R3,PRJLIST                                                       
         SPACE 1                                                                
         USING BIND,R3                                                          
         OC    BININ,BININ                                                      
         BZ    PRJRD               NOTHING IN TABLE YET                         
         L     R1,BININ                                                         
         LA    R2,BINTABLE                                                      
PRJNM1   CLC   PRJK,0(R2)          LOOK FOR PROJECT NAME IN TABLE               
         BE    PRJNM3                                                           
         LA    R2,PRJLEN(R2)                                                    
         BCT   R1,PRJNM1                                                        
         B     PRJRD               IF NOT FOUND READ IT                         
PRJNM3   LR    R5,R2               AND ADD TO TABLE                             
         MVC   WORK,SPACES                                                      
         MVC   WORK(36),PRJNAME                                                 
         B     PAXIT                                                            
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
PRJRD    L     R4,ADACC            READ 1J TO GET PROJECT NAME                  
         MVC   SAVEKEY,ACKEYACC                                                 
         L     R4,RECORD                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC,PRJK                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',(R4),(R4)                    
         MVC   WORK,SPACES                                                      
         CLC   ACKEYACC,PRJK                                                    
         BNE   PAXIT               PROJECT HAS BEEN DELETED                     
         SPACE 1                                                                
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND NAME ELEMENT                      
         LA    R6,PRJNAME                                                       
         BAS   RE,NAMOUT                                                        
         SPACE 1                                                                
         L     R3,PRJLIST                                                       
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,PRJK),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         MVC   WORK(36),PRJNAME                                                 
         SPACE 1                                                                
PRJXIT   DC    0H'0'                                                            
         B     PAXIT                                                            
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 1                                                                
         USING ACNAMED,R4                                                       
NAMOUT   LTR   R4,R4                                                            
         BZR   RE                                                               
         MVC   0(36,R6),SPACES                                                  
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R6),ACNMNAME                                                 
         SPACE 1                                                                
FORMAT   EDIT  (P8,0(R2)),(9,P+90),2,MINUS=YES                                  
         EDIT  (P8,8(R2)),(11,P+99),2,MINUS=YES                                 
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE 1                                                                
RELOTAB  DS    0A                                                               
         DC    V(SQUASHER)                                                      
         DC    A(ALEVEL)                                                        
         DC    A(APRJLIST)                                                      
         DC    A(ARECORD)                                                       
         DC    A(AIO2)                                                          
         DC    V(COVAIL)                                                        
         DC    V(UNDERLIN)                                                      
         DC    A(APOSTBUF)                                                      
         DC    V(BUFFALOC)                                                      
         DC    V(ACSLRY)                                                        
         DC    V(ACSALHST)                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
*              DSECT FOR WORKING STORAGE                                        
ACPA02D  DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
SQUASHER DS    V                                                                
LEVEL    DS    A                                                                
PRJLIST  DS    A                                                                
RECORD   DS    A                                                                
IO2      DS    A                                                                
COVAIL   DS    V                                                                
UNDERLIN DS    V                                                                
POSTBUF  DS    A                                                                
ABUFF    DS    V                                                                
ACSLRY   DS    V                                                                
ACSALHST DS    V                                                                
         SPACE 1                                                                
ID       DS    CL16                                                             
TOTCNT   DS    PL8                                                              
TOTCSH   DS    PL8                                                              
DDSDBT   DS    PL8                                                              
DDSCRT   DS    PL8                                                              
TODAY    DS    CL3                                                              
         SPACE 1                                                                
REQHRS   DS    PL8                                                              
REQCOST  DS    PL8                                                              
POSTCOST DS    PL8                                                              
RUNCOST  DS    PL8                                                              
         SPACE 1                                                                
OPTIONS  DS    CL1                                                              
POST     EQU   X'80'               MAKE POSTINGS                                
         SPACE 1                                                                
ITEMS    DS    PL2                 LINES FOR PERSON                             
SAVEKEY  DS    CL15                                                             
HRKEY    DS    CL42                                                             
FRSTSUB  DS    CL1                                                              
ACTIVITY DS    CL1                                                              
         SPACE 1                                                                
MOS      DS    CL2                                                              
PDATE    DS    CL3                                                              
MONTH    DS    CL6                                                              
         SPACE 1                                                                
PROFS    DS    0CL16                                                            
PROF1    DS    CL1        SALARY ONLY             Y,N                           
PROF2    DS    CL1        STANDARD HOURS          1=133.33                      
PROF3    DS    CL1        BILLING FACTOR          WHOLE NUMBER                  
PROF4    DS    CL1        BILLING FACTOR          FRACTION                      
PROF5    DS    CL1        D=PROF3 & 4 REPRESENT DOLLAR AMOUNTS                  
PROF6    DS    CL1                                                              
PROF7    DS    CL1                                                              
PROF8    DS    CL1                                                              
PROF9    DS    CL1                                                              
PROF10   DS    CL1                                                              
PROF11   DS    CL1                                                              
PROF12   DS    CL1                                                              
PROF13   DS    CL1                                                              
PROF14   DS    CL1                                                              
PROF15   DS    CL1                                                              
PROF16   DS    CL1                                                              
         SPACE 1                                                                
ELCODE   DS    CL1                                                              
CPH      DS    PL5                                                              
COST     DS    PL8                                                              
COMPNAM  DS    CL36                                                             
LONGDES  DS    CL1                 LENGTH OF LONGEST DESCRIPTION                
NUMLEV   DS    CL1                 NUMBER OF LEVELS                             
LOWACC   DS    A                   ADDRESS OF LOW ACCOUNT                       
LOWDESC  DS    CL31                DESCRIPTION OF TWO LOWEST LEVELS             
WRKA     DS    CL100                                                            
WRKB     DS    CL100                                                            
STEND    DS    CL4                                                              
*                                 SAVE 1R KEY IN SALARYD FORMAT(SALHST)         
OFFIC    DS    CL(L'SALOFFC)      OFFICE                                        
DEPART   DS    CL(L'SALDEPT)      DEPARTMENT                                    
SDEPART  DS    CL(L'SALSDPT)      SUBDEPARTMENT                                 
PERSON   DS    CL(L'SALPRSN)      PERSON                                        
SALAREA  DS    CL(SLRLEN)         SALARY AREA  (ACSLRY)                         
SALARE2  DS    CL(SALLNQ)         SALARY AREA  (ACSALHST)                       
         EJECT                                                                  
*              DSECT FOR LEVEL DESCRIPTION AND NAMES                            
         SPACE 1                                                                
LEVD     DSECT                                                                  
LEVLN    DS    CL1                 LENGTH OF LEVEL                              
LEVDES   DS    CL15                DESCRIPTION                                  
LEVACC   DS    CL12                ACCOUNT CODE                                 
LEVNAM   DS    CL36                ACCOUNT NAME                                 
LEVHRS   DS    PL8                 HOURS                                        
LEVCOST  DS    PL8                 COST                                         
LEVTHRS  DS    PL8                 TASK HOURS                                   
LEVTCOST DS    PL8                 TASK COST                                    
LEVLEN   EQU   *-LEVLN                                                          
         SPACE 1                                                                
*              DSECT FOR PROJECT/TASK TABLE                                     
         SPACE 1                                                                
TASKD    DSECT                                                                  
TSKCLI   DS    CL3                 CLIENT                                       
TSKDIV   DS    CL3                 DIVISION                                     
TSKPRJ   DS    CL6                 PROJECT                                      
TSKCDE   DS    CL2                 TASK CODE                                    
TSKDTE   DS    CL3                 TASK TRANSACTION DATE                        
TSKHRS   DS    PL8                 HOURS                                        
TSKCOST  DS    PL8                 COST                                         
TSKPHRS  DS    PL8                 PREVIOUS HOURS                               
TSKPCST  DS    PL8                 PREVIOUS COST                                
TSKCNT   DS    PL8                                                              
TSKLEN   EQU   *-TSKCLI                                                         
         SPACE 1                                                                
*              DSECT FOR THE PROJECT NAME RECORD                                
         SPACE 1                                                                
PRJD     DSECT                                                                  
PRJK     DS    CL15                KEY                                          
PRJNAME  DS    CL36                NAME                                         
PRJLEN   EQU   *-PRJK                                                           
         SPACE 1                                                                
*              DSECT FOR THE BINSRCH LIST                                       
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINTABLE DS    0CL1                                                             
         EJECT                                                                  
         BUFF  LINES=100,ROWS=1,COLUMNS=5,FLAVOR=P,KEYLIST=(17,A)               
ACPA02   CSECT                                                                  
         ENTRY ALEVEL                                                           
ALEVEL   DS    0D                                                               
         DS    (4*LEVLEN)C                                                      
         SPACE 1                                                                
         ENTRY APRJLIST                                                         
APRJLIST DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(PRJLEN)         RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(L'PRJK)         KEY LENGTH                                   
         DC    F'600'              MAX IN TABLE                                 
         DS    (600*PRJLEN)C       THE TABLE                                    
         SPACE 1                                                                
         ENTRY ARECORD                                                          
ARECORD  DS    0D                                                               
         DS    CL42                                                             
         DS    CL2000                                                           
         SPACE 1                                                                
         ENTRY AIO2                                                             
AIO2     DS    0D                                                               
         DS    CL42                                                             
         DS    CL2000                                                           
         SPACE 1                                                                
         ENTRY APOSTBUF                                                         
APOSTBUF DS    0D                                                               
         DC    4500X'00'                                                        
         SPACE 2                                                                
*ACGENBOTH                                                                      
*ACREPWORKD                                                                     
*ACGENMODES                                                                     
*ACGENPOST                                                                      
*DDLOGOD                                                                        
*DDSLRD                                                                         
*ACSALHSTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDSLRD                                                         
       ++INCLUDE ACSALHSTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREPPA02 05/01/02'                                      
         END                                                                    
