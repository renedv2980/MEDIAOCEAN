*          DATA SET SPREPK202  AT LEVEL 037 AS OF 04/04/11                      
*PHASE SPK202A                                                                  
*INCLUDE WGTLIST                                                                
*INCLUDE SORTER                                                                 
         TITLE 'SPK202 - SPOTPAK UNALLOCATION PROGRAM'                          
SPK202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPK202,RR=R5                                                   
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPK202+4096,RC                                                   
*                                                                               
         ST    R5,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         L     R7,MEDBUFF                                                       
         USING MEDBLOCK,R7                                                      
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    UN100                                                            
         CLI   MODE,RUNFRST                                                     
         BE    UN10                                                             
         CLI   MODE,CLTFRST                                                     
         BE    UN20                                                             
         CLI   MODE,PRDFRST                                                     
         BE    UN25                                                             
         CLI   MODE,ESTFRST                                                     
         BE    UN30                                                             
         CLI   MODE,MKTFRST                                                     
         BE    UN40                                                             
         CLI   MODE,STAFRST                                                     
         BE    UN50                                                             
         CLI   MODE,STALAST                                                     
         BE    UN200                                                            
         CLI   MODE,MKTLAST                                                     
         BE    UN210                                                            
         CLI   MODE,ESTLAST                                                     
         BE    UN220                                                            
         CLI   MODE,CLTLAST                                                     
         BE    UN230                                                            
         CLI   MODE,RUNLAST                                                     
         BE    UN240                                                            
EXIT     XMOD1 1                                                                
         EJECT                                                                  
* RUNFRST                                                                       
*                                                                               
UN10     DS    0H                                                               
*                                                                               
         L     RE,SSB                                                           
         OI    3(RE),X'08'         TURN ON FULL OFF-LINE RECOVERY               
*                                                                               
         STM   R7,RC,HDHKR7                                                     
         LA    R0,UNHDHK                                                        
         ST    R0,HEADHOOK                                                      
*                                                                               
         STM   R7,RC,SPHKR7                                                     
         LA    R0,UNSPHK                                                        
         ST    R0,SPOTHOOK                                                      
*                                                                               
         MVC   MEDNUMPE,=F'1'                                                   
         MVI   SPSUPMKT,C'Y'                                                    
         MVC   MEDLCHNK,=F'200'                                                 
*                                                                               
         MVI   SORTOPEN,C'N'                                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* CLTFRST                                                                       
*                                                                               
UN20     DS    0H                                                               
         MVC   P,SPACES                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,0          SUBPRG = NUMBER OF RE-ALLOC PRDS             
*                                                                               
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         CLC   QAREA+49(6),=C'DELETE'                                           
         BE    *+12                                                             
         CLI   CPROF,C'0'          TEST BRAND POOL CLIENT                       
         BNE   CLTERR                                                           
         DROP  R6                                                               
* DO NOT ALLOW CANADIAN MEDIA C OR N                                            
         L     R6,ADAGY                                                         
         USING AGYHDR,R6                                                        
         CLI   AGYPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   UN20A                                                            
         CLI   QMED,C'N'                                                        
         BE    CNCLTERR                                                         
         CLI   QMED,C'C'                                                        
         BE    CNCLTERR                                                         
*                                                                               
UN20A    XC    TOTCLT,TOTCLT                                                    
         XC    BRDTAB,BRDTAB                                                    
         XC    BRDLIST,BRDLIST                                                  
         CLC   QAREA+49(3),SPACES  ALLOW SPACES FOR UNA                         
         BE    EXIT                                                             
         CLC   QAREA+49(3),=C'UNA'                                              
         BE    EXIT                                                             
         CLC   QAREA+49(6),=C'DELETE'                                           
         BE    EXIT                                                             
* EDIT LIST OF PRDS TO BE ALLOCATED                                             
         LA    R4,BRDLIST                                                       
         LA    R5,QAREA+49                                                      
         LA    R3,4                                                             
*                                                                               
UN21     DS    0H                                                               
         IC    RE,RCSUBPRG                                                      
         LA    RE,1(RE)            INCREMENT PRD COUNT                          
         STC   RE,RCSUBPRG                                                      
*                                                                               
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         LA    R6,CLIST                                                         
         DROP  R6                                                               
*                                                                               
UN22     CLC   0(3,R5),0(R6)                                                    
         BE    UN22X                                                            
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BH    UN22                                                             
         DC    H'0'                                                             
*                                                                               
UN22X    MVC   0(1,R4),3(R6)       MOVE PRD CODE                                
         MVC   1(1,R4),3(R5)        AND WEIGHT                                  
*                                                                               
* X'40' = THE SPACE, EQUALS TO 64, WHICH IS A VALID PERCENT WEIGHT              
*        CLI   1(R4),C' '                                                       
*        BNE   *+8                                                              
*        MVI   1(R4),1             SET DEFAULT WEIGHT                           
         TM    1(R4),X'F0'         TEST EBCDIC VALUE                            
         BNO   *+8                 NO                                           
         NI    1(R4),X'0F'         DROP SIGN BITS                               
*                                                                               
         LA    R4,2(R4)                                                         
         LA    R5,4(R5)                                                         
         CLC   0(3,R5),SPACES                                                   
         BE    *+8                                                              
         BCT   R3,UN21                                                          
*                                                                               
* NOW BUILD WEIGHTED BRAND SELECTION TABLE                                      
*                                                                               
         MVC   WORK(9),BRDLIST     PRESERVE ORIGINAL LIST                       
         GOTO1 =V(WGTLIST),DMCB,WORK,BRDTAB,XSORT,RR=RELO                       
*                                                                               
         CLC   =C'ALL',QEST        TEST SINGLE ESTIMATE REQUEST                 
         BE    EXIT                                                             
         CLC   =C'NO ',QEST                                                     
         BE    EXIT                                                             
         CLC   QEST,SPACES                                                      
         BNH   EXIT                                                             
         CLC   QESTEND,SPACES                                                   
         BNH   *+14                                                             
         CLC   QEST,QESTEND                                                     
         BNE   EXIT                                                             
         XC    WORK,WORK           YES-CHECK EST HDR IS OPEN FOR EACH           
         L     R1,ADCLT                REALLOCATE PRODUCT                       
         MVC   WORK(3),1(R1)                                                    
         PACK  DUB,QEST                                                         
         CVB   R1,DUB                                                           
         STC   R1,WORK+9           WORK IS DUMMY BUY RECORD KEY                 
         LA    R4,BRDLIST                                                       
         LA    R3,4                                                             
         SR    R5,R5                                                            
*                                                                               
UN23     ICM   R5,1,0(R4)                                                       
         BZ    EXIT                                                             
         GOTO1 =V(SPESTCHK),HKDMCB,((R5),WORK),ESTTAB,(RA),RR=RELO              
         CLI   8(R1),0                                                          
         BNE   ESTERR              EST NOT OPEN - STOP RIGHT HERE               
         LA    R4,2(R4)                                                         
         BCT   R3,UN23                                                          
         B     EXIT                                                             
         SPACE 2                                                                
ESTERR   MVC   P(L'ESTMSG),ESTMSG                                               
         B     ERR                                                              
*                                                                               
CLTERR   MVC   P(L'CLTMSG),CLTMSG                                               
         B     ERR                                                              
*                                                                               
CNCLTERR MVC   P(L'CNCLTMSG),CNCLTMSG                                           
         B     ERR                                                              
*                                                                               
ERR      GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
ESTMSG   DC    C'** ERROR ** REALLOCATE ESTIMATE DOES NOT EXIST'                
CLTMSG   DC    C'** ERROR ** MAY NOT REALLOCATE BRAND POOL CLIENT'              
CNCLTMSG DC    C'** ERROR ** MAY NOT RE-ALLOCATE MEDIA C OR N'                  
         EJECT                                                                  
* PRDFRST                                                                       
*                                                                               
UN25     CLI   QOPT5,C'Y'          FOR PRODUCT CHANGE REPORT,                   
         BNE   EXIT                GET ALL ESTIMATE NAMES                       
         LA    R1,ESTNAMES                                                      
         LA    RF,255                                                           
         XC    0(L'ESTNAMES,R1),0(R1)                                           
         LA    R1,L'ESTNAMES(R1)                                                
         BCT   RF,*-10                                                          
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING ESTHDRD,R6                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,PRD                                                      
*                                                                               
UN26     GOTO1 HIGH                                                             
         B     UN28                                                             
*                                                                               
UN27     GOTO1 SEQ                                                              
*                                                                               
UN28     CLC   KEY(EKEYEST-EKEY),KEYSAVE                                        
         BNE   EXIT                                                             
         LA    R6,KEY                                                           
         CLI   EKEYEST+1,0                                                      
         BE    *+14                                                             
         MVC   EKEYEST+1(5),=X'FFFFFFFFFF'                                      
         B     UN26                                                             
         L     R6,ADEST                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         ZIC   R1,EKEYEST                                                       
         BCTR  R1,0                                                             
         MH    R1,=Y(L'ESTNAMES)                                                
         LA    R1,ESTNAMES(R1)                                                  
         MVC   0(L'ESTNAMES,R1),EDESC                                           
         B     UN27                                                             
         DROP  R6                                                               
         EJECT                                                                  
* ESTFRST                                                                       
*                                                                               
UN30     DS    0H                                                               
         XC    TOTEST,TOTEST                                                    
         MVI   MEDBRAND,220        GET ALL SPOTS (ALLOC AND UNALL)              
         CLC   QPRD,=C'POL'        TEST QPRD=POL                                
         BNE   *+8                                                              
         MVI   MEDBRAND,219        YES-ONLY GET UNALLOCATED                     
* CLEAR ESTIMATE TABLE                                                          
         LA    R0,4                                                             
         LA    R1,ESTTAB                                                        
         XC    0(256,R1),0(R1)                                                  
         LA    R1,256(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
*                                                                               
         CLI   QOPT5,C'Y'          GET PRODUCT NAMES FOR PRODUCT                
         BNE   EXIT                CHANGE REPORT                                
         MVC   SVQPRD,QPRD                                                      
         MVC   QPRD,=C'ALL'                                                     
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
         MVC   QPRD,SVQPRD                                                      
         B     EXIT                                                             
         SPACE 2                                                                
* MKTFRST                                                                       
*                                                                               
UN40     DS    0H                                                               
         XC    TOTMKT,TOTMKT                                                    
         MVI   MKTSW,C'N'          RESET MKT NAME PRINTED                       
         MVI   STASW,C'N'          RESET STATION PRINTED                        
*                                                                               
         LA    R0,BRDTAB                                                        
         ST    R0,NEXTBRD                                                       
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* STAFRST                                                                       
*                                                                               
UN50     DS    0H                                                               
         XC    TOTSTA,TOTSTA                                                    
         MVI   STASW,C'N'          RESET STATION PRINTED                        
         L     R6,ADSTAT                                                        
         USING STARECD,R6                                                       
         MVC   SVSYSNM,SSYSNAME    SAVE CABLE SYSTEM NAME (IF ANY)              
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* PROCBUY                                                                       
*                                                                               
UN100    DS    0H                                                               
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         CLC   4(2,R6),KEY+4       TEST SAME MARKET (SPILL)                     
         BNE   EXIT                NO - SKIP SPILL BUYS                         
*                                                                               
*        CLI   QOPT4,C' '                                                       
         CLI   QFILTER,C' '                                                     
         BNH   *+14                                                             
*        CLC   BDDAYPT,QOPT4                                                    
         CLC   BDDAYPT,QFILTER                                                  
         BNE   EXIT                                                             
*                                                                               
*****====== NEW CODE FOR AGENCY DF ONLY ========= BPOO ********                 
         CLC   QAGY,=C'DF'         IF NOT AGENCY DF JUST DO USUAL               
         BNE   UN101               ROUTINE                                      
*                                                                               
         CLI   Q2OPT1,C'T'         TRADE BUYLINES                               
         BNE   *+12                                                             
         CLI   BDCIND,0            IF REC IS NOT TRADE BUYLINE EXIT             
         BNE   EXIT                                                             
         CLI   Q2OPT1,C'C'         CASH  BUYLINES                               
         BNE   *+12                                                             
         CLI   BDCIND,0            IF CASH BUYLINE RECORD THEN PROCESS          
         BE    EXIT                                                             
         DROP  R6                                                               
******==================================================**********              
*                                                                               
UN101    XC    TOTBUY,TOTBUY                                                    
         XC    ADDLIST,ADDLIST     CLEAR ADDED PRD LIST                         
         MVI   ERRFLAG,C'N'        AND RESET ERROR FLAG                         
         MVI   REPSW,C'N'          RESET ACTIVITY FLAG                          
         MVI   FIRSTPRD,0                                                       
*                                                                               
         GOTO1 MEDGETBY,DMCB,(RA),0                                             
*                                                                               
         CLC   =C'DELETE',QAREA+49                                              
         BNE   UN104                                                            
* REMOVE ELEMENTS WITH ELCODE X'7F'                                             
* (I WANTED THEM TO BE X'FF' BUT MEDGETBY TURNS OFF X'80' BIT)                  
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCDLO,X'7F'                                                     
         MVI   ELCDHI,X'7F'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   UN104                                                            
         MVI   REPSW,C'Y'          SET ACTIVE                                   
*                                                                               
UN102    DS    0H                                                               
         GOTO1 RECUP,DMCB,ADBUY,(R6)                                            
         BAS   RE,NEXTEL2                                                       
         BE    UN102                                                            
*                                                                               
UN104    CLI   REPSW,C'Y'          TEST ACTIVITY THIS LINE                      
         BNE   EXIT                                                             
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         CLI   RCSUBPRG,0          TEST UNALL REQUEST                           
         BNE   UN104A                                                           
         CLC   QAREA+49(6),=C'DELETE'                                           
         BE    UN104A              YES - DON'T CLEAR MASPRD                     
         XC    BDMASPRD,BDMASPRD   YES-CLEAR THE MASTER PRODUCT                 
         B     UN106                                                            
*                                                                               
UN104A   CLI   FIRSTPRD,0          TEST HAVE A FIRST RE-ALLOCATION PRD          
         BE    UN106                                                            
         CLC   QPRD,=C'POL'        YES-TEST ALLOCATION REQUEST                  
         BNE   *+16                                                             
         CLI   BDMASPRD,0          YES-ONLY SET MASPRD IF PREVIOUSLY 0          
         BE    UN105                                                            
         B     UN106                                                            
         CLC   BPRD,BDMASPRD       FOR RE-ALLOCATION, ONLY SET MASPRD           
         BNE   UN106               IF IT CURRENTLY IS THE REQUESTED PRD         
*                                                                               
UN105    MVC   BDMASPRD(1),FIRSTPRD                                             
         MVI   BDMASPRD+1,0                                                     
         DROP  R6                                                               
*                                                                               
UN106    OC    ADDLIST,ADDLIST     ANY NEW PRDS                                 
         BZ    *+12                                                             
         LA    R0,ADDLIST          IF SO, SET ADDRESS OF LIST IN DM6            
         ST    R0,DM6                                                           
*                                                                               
         MVC   AREC,ADBUY                                                       
         GOTO1 PUT                                                              
*                                                                               
         CLC   =C'TRACE ',QUESTOR                                               
         BNE   *+8                                                              
         BAS   RE,UNTRACE                                                       
*                                                                               
         XC    DM6(4),DM6          CLEAR PRD LIST ADDR                          
         GOTO1 HIGH                RESTORE DIR FOR SEQ READ                     
* PRINT BUY LINE DATA                                                           
         LA    R4,P                                                             
         USING UNLINED,R4                                                       
         CLI   MKTSW,C'Y'          TEST MKT NAME PRINTED                        
         BE    UN110                                                            
         MVC   P2,P                MOVE ERROR MESSAGES IF ANY                   
         MVC   P,SPACES                                                         
         MVC   UNMKT,MKT                                                        
         MVC   UNMKT+6(24),MKTNM                                                
         LA    R4,P2                                                            
UN110    CLI   STASW,C'Y'          TEST STATION PRINTED                         
         BE    UN112                                                            
         MVI   ALLOWLIN,6                                                       
         MVC   UNSTA,BIGSTA                                                     
UN112    L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  UNEST,DUB                                                        
         MVI   UNEST+3,C'-'                                                     
*                                                                               
         LLC   R0,BUYKBUY                                                       
         TM    BUYRCNTL,BUYRLN2    2-BYTE LINE NUMBERS?                         
         BZ    *+8                                                              
         ICM   R0,3,BUYRLIN                                                     
         EDIT  (R0),UNLIN                                                       
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    R1,TOTBUY                                                        
         BAS   RE,UNFMT                                                         
         GOTO1 REPORT                                                           
         CLI   STASW,C'Y'          TEST STATION FIRST                           
         BE    UN114                                                            
         CLC   SVSYSNM,SPACES      YES-PRINT CABLE SYSTEM NAME (IF ANY)         
         BNH   UN114                                                            
         MVC   UNSTA(L'SVSYSNM),SVSYSNM                                         
         GOTO1 REPORT                                                           
*                                                                               
UN114    MVI   MKTSW,C'Y'                                                       
         MVI   STASW,C'Y'                                                       
         SPACE 2                                                                
* ADD BUY TO OTHER TOTALS                                                       
         LA    R4,TOTSTA                                                        
UN120    LA    R0,5                                                             
         LA    R1,TOTBUY                                                        
UN122    LM    RE,RF,0(R4)                                                      
         A     RE,0(R1)            SPOTS                                        
         A     RF,4(R1)            DOLLARS                                      
         STM   RE,RF,0(R4)                                                      
         LA    R1,8(R1)                                                         
         LA    R4,8(R4)                                                         
         BCT   R0,UN122                                                         
*                                                                               
         LA    R0,TOTCLT                                                        
         CR    R4,R0                                                            
         BNH   UN120                                                            
*                                                                               
         XC    TOTBUY,TOTBUY                                                    
         B     EXIT                                                             
         SPACE 2                                                                
* TRACE ROUTINE FOR TESTING                                                     
*                                                                               
UNTRACE  NTR1                                                                   
*                                                                               
         GOTO1 REPORT              SKIP A LINE                                  
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,TRDMCB,=C'BUYREC',ADBUY,C'DUMP',                 X        
               (R0),=C'1D',RR=RELO                                              
         GOTO1 REPORT              SKIP A LINE                                  
         GOTO1 PRNTBL,TRDMCB,=C'ADDED PRD LIST',ADDLIST,C'DUMP',       X        
               128,=C'1D',RR=RELO                                               
         GOTO1 REPORT              SKIP A LINE                                  
         B     EXIT                                                             
*                                                                               
TRDMCB   DS    6F                                                               
         EJECT                                                                  
* SPOTHOOK    R1 = A(GETRATE AREA)                                              
*                                                                               
         CNOP  0,4                                                              
         USING *,RF                                                             
UNSPHK   NTR1                                                                   
         LM    R7,RC,SPHKR7                                                     
         B     SPHK2                                                            
SPHKR7   DC    6F'0'                                                            
         DROP  RF                                                               
*                                                                               
SPHK2    L     RE,0(R1)            POINT TO GETRATE AREA                        
         MVC   DUB,0(RE)           SAVE SPOTS AND DOLLARS                       
         L     R6,SPOTADDR                                                      
         TM    6(R6),X'C0'         IGNORE MINUS OR MINUSSED SPOTS               
         BNZ   SPHKX                                                            
         OC    4(2,R6),4(R6)       TEST PAID                                    
         BNZ   SPHKX                                                            
         CLC   QPRD,=C'POL'        TEST QPRD=POL                                
         BNE   *+16                                                             
         CLI   1(R6),10            YES-ONLY UNALLOCATED                         
         BE    SPHK4                                                            
         B     SPHKX                                                            
         CLI   1(R6),10                                                         
         BNH   SPHKX               IGNORE UNALL SPOTS                           
         CLI   1(R6),14            TEST SINGLE BRAND ALLOCATED                  
         BNE   SPHK3               NO                                           
         CLC   10(1,R6),BPRD       RIGHT BRAND                                  
         BNE   SPHKX                                                            
         OC    12(2,R6),12(R6)     TEST DETAIL BILLED                           
         BNZ   SPHKX               YES  - IGNORE                                
         B     SPHK4                                                            
*                                                                               
SPHK3    B     SPHKX             ***** IGNORE PIGGYBACKS FOR NOW *****          
  SPACE 2                                                                       
* POST SPOTS AND DOLLARS FOR UN-ALLOCATED BRAND                                 
*                                                                               
SPHK4    MVI   REPSW,C'Y'          SET ACTIVITY FLAG                            
         LM    RE,RF,TOTBUY                                                     
         A     RE,DUB              SPOTS                                        
         A     RF,DUB+4            DOLLARS                                      
         STM   RE,RF,TOTBUY                                                     
*                                                                               
         CLI   RCSUBPRG,0          TEST UNALL REQUEST                           
         BE    SPHK20                                                           
         EJECT                                                                  
* INSERT NEW PRODUCT IN ELEM IF EST ON FILE                                     
         L     R3,NEXTBRD                                                       
         MVC   NEWPRD,0(R3)                                                     
         CLC   BPRD,0(R3)                                                       
         BE    SPHK10                                                           
SPHK5    IC    R0,0(R3)                                                         
         GOTO1 =V(SPESTCHK),HKDMCB,((R0),ADBUY),ESTTAB,(RA),RR=RELO             
         CLI   8(R1),0                                                          
         BNE   SPHKERR                                                          
*                                                                               
         CLI   FIRSTPRD,0          RECORD FIRST RE-ALLOCATION PRODUCT           
         BNE   *+10                                                             
         MVC   FIRSTPRD,0(R3)                                                   
*                                                                               
         CLI   1(R6),10            TEST UNALLOCATED                             
         BE    *+14                                                             
         MVC   10(1,R6),0(R3)      NO-MOVE PRD CODE TO ELEM                     
         B     SPHK5A                                                           
         XC    WORK,WORK           YES-BUILD ALLOCATED ELEMENT                  
         MVC   WORK(10),0(R6)                                                   
         MVI   WORK+1,14                                                        
         MVC   WORK+10(1),0(R3)                                                 
         L     R1,ADBUY                                                         
         MVC   WORK+11(1),BDSEC-BUYREC(R1)                                      
         GOTO1 RECUP,DMCB,ADBUY,(R6)  DELETE OLD ELEMENT                        
         GOTO1 (RF),(R1),,WORK,(R6)   AND ADD NEW                               
*                                                                               
* TEST PRD CODE IN BUY ALREADY                                                  
SPHK5A   L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
SPHK5B   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    SPHK6                                                            
         CLI   0(R6),11                                                         
         BL    SPHK5B                                                           
         CLI   0(R6),X'0D'                                                      
         BH    SPHK5B                                                           
         C     R6,SPOTADDR                                                      
         BE    SPHK5B                                                           
         CLI   1(R6),14                                                         
         BNE   SPHK5B                                                           
         CLC   10(1,R6),0(R3)      TEST PRD ALREADY ALLOCATED                   
         BE    SPHK10              YES                                          
         B     SPHK5B                                                           
         SPACE 2                                                                
* TEST PRODUCT IN ADDED PRD LIST                                                
*                                                                               
SPHK6    LA    RE,ADDLIST                                                       
*                                                                               
SPHK6A   CLI   0(RE),0                                                          
         BE    SPHK8                                                            
         CLC   0(1,RE),0(R3)                                                    
         BE    SPHK10                                                           
         LA    RE,1(RE)                                                         
         B     SPHK6A                                                           
*                                                                               
SPHK8    MVC   0(1,RE),0(R3)   ADD BRAND TO LIST                                
         EJECT                                                                  
*                                                                               
* ADD TO BUY TOTALS FOR THIS BRAND                                              
*                                                                               
SPHK10   LA    R4,TOTBUY+8                                                      
         LA    RE,BRDLIST                                                       
         ZIC   RF,RCSUBPRG                                                      
SPHK12   CLC   0(1,RE),0(R3)       MATCH PRD                                    
         BE    SPHK14                                                           
         LA    R4,8(R4)                                                         
         LA    RE,2(RE)                                                         
         BCT   RF,SPHK12                                                        
         DC    H'0'                                                             
SPHK14   LM    RE,RF,0(R4)                                                      
         A     RE,DUB              SPOTS                                        
         A     RF,DUB+4            DOLLARS                                      
         STM   RE,RF,0(R4)                                                      
* UPDATE BRDTAB POINTER                                                         
         LA    R3,1(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         LA    R3,BRDTAB                                                        
         ST    R3,NEXTBRD                                                       
         B     SPHK24                                                           
         SPACE 2                                                                
* UNALLOCATION REQUEST - DELETE ELEM AND RE-ADD WITH LEN 10                     
*                                                                               
SPHK20   CLC   QAREA+49(6),=C'DELETE'                                           
         BNE   SPHK22                                                           
         MVI   0(R6),X'7F'         SET A MONSTER ELEMENT CODE                   
         B     SPHK24              AND GET OUT                                  
*                                                                               
SPHK22   MVC   WORK(10),0(R6)      MOVE ELEM                                    
         MVI   WORK+1,10           SET NEW ELEM LEN                             
         GOTO1 RECUP,DMCB,ADBUY,(R6)                                            
         GOTO1 (RF),(R1),,WORK,(R6)                                             
         SPACE 2                                                                
* IF PRODUCT CHANGE REPORT IS REQUESTED, WRITE A RECORD TO THE SORT             
*                                                                               
SPHK24   CLI   QOPT5,C'Y'                                                       
         BNE   SPHKX                                                            
         XC    WREC,WREC           BUILD WORK RECORD                            
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         MVC   WMED,QMED                                                        
         MVC   WMKT,MKT                                                         
         MVC   WSTA,BIGSTA                                                      
         MVC   WCLT,CLT                                                         
         MVC   WPRD,QPRD                                                        
         MVC   WSTART,QSTART                                                    
         MVC   WEND,QEND                                                        
         MVC   WEST,BUYKEST                                                     
         MVC   WDAY,BDDAY                                                       
         SR    R1,R1               SEQUENCE THE DAYS MON,TUE,WED,...            
         ICM   R1,8,BDDAY                                                       
         SR    RF,RF                                                            
         B     *+14                                                             
         SLL   R1,1                                                             
         LTR   R1,R1                                                            
         BM    *+8                                                              
         BCT   RF,*-10                                                          
         LPR   RF,RF                                                            
         STC   RF,WDAYSEQ                                                       
         MVC   WTIM,BDTIMST                                                     
         MVC   WPRG,BDPROGRM                                                    
         MVC   WLEN,BDSEC                                                       
         L     R1,SPOTADDR                                                      
         MVC   WPRD2,NEWPRD        NEW PRODUCT                                  
         MVC   WCOST,BDCOST                                                     
         TM    6(R1),X'20'         TEST COST OVERRIDE                           
         BZ    *+10                                                             
         MVC   WCOST,7(R1)                                                      
         MVC   WDAT,2(R1)          DATE                                         
         GOTO1 DATCON,DMCB,(2,WDAT),DUB     GET MONDAY                          
         GOTO1 GETDAY,(R1),DUB,FULL                                             
         CLC   FULL(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         LNR   RF,RF                                                            
         BZ    SPHK25                                                           
         ST    RF,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,DUB,WORK                                              
         GOTO1 DATCON,(R1),WORK,(2,WDAT)                                        
*                                                                               
SPHK25   MVC   WMKTNM,MKTNM        NAMES                                        
         MVC   WCLTNM,CLTNM                                                     
         MVC   WPRDNM,PRDNM                                                     
         MVC   WSYSNAME,SVSYSNM                                                 
         L     R1,ADBUY                                                         
         ZIC   RF,BUYKEST-BUYKEY(R1)                                            
         BCTR  RF,0                                                             
         MH    RF,=Y(L'ESTNAMES)                                                
         LA    RF,ESTNAMES(RF)                                                  
         MVC   WESTNM,0(RF)                                                     
         CLI   RCSUBPRG,0          TEST UNALL REQUEST                           
         BNE   SPHK26                                                           
         MVI   WPRD2,0                                                          
         CLC   QAREA+49(6),=C'DELETE'   YES-PRD2=0 FOR DELETE                   
         BE    SPHK27                                                           
         MVI   WPRD2,X'FF'                  PRD2=X'FF' FOR UNALL                
         B     SPHK27                                                           
         DROP  R6                                                               
*                                                                               
SPHK26   ZIC   RE,WPRD2            GET NAME OF NEW PRODUCT                      
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RE,0(RE,RF)                                                      
         MVC   WPRDCD2,1(RE)                                                    
         MVC   WPRDNM2,4(RE)                                                    
*                                                                               
SPHK27   CLI   SORTOPEN,C'Y'       TEST SORT OPEN YET                           
         BE    SPHK28                                                           
         MVI   SORTOPEN,C'Y'       NO-OPEN IT NOW                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
SPHK28   GOTO1 =V(SORTER),DMCB,=C'PUT',WREC                                     
*                                                                               
SPHKX    MVI   SPOTYORN,C'N'                                                    
         B     EXIT                                                             
*                                                                               
HKDMCB   DS    6F                                                               
         EJECT                                                                  
* ERROR HAS OCCURED - BRAND ESTIMATE NOT OPEN                                   
*                                                                               
SPHKERR  LA    R4,P                                                             
         USING UNLINED,R4                                                       
         LA    RE,BRDLIST                                                       
         ZIC   RF,RCSUBPRG                                                      
SPHKERR2 CLC   0(1,RE),0(R3)                                                    
         BE    SPHKERR4                                                         
         LA    R4,17(R4)                                                        
         LA    RE,2(RE)                                                         
         BCT   RF,SPHKERR2                                                      
         DC    H'0'                                                             
*                                                                               
SPHKERR4 MVC   UNPRD2(16),=CL16'**EST NOT OPEN**'                               
*                                                                               
         LA    R3,1(R3)            POINT TO NEXT PRD                            
         CLI   0(R3),0             TEST E-O-L                                   
         BNE   SPHK5                NO - TRY NEXT BRAND                         
         CLI   ERRFLAG,C'Y'        TEST REACHED E-O-L BEFORE                    
         BE    SPHKX                YES -FORGET IT                              
         MVI   ERRFLAG,C'Y'        ELSE SET FLAG                                
         LA    R3,BRDTAB           POINT TO START                               
         B     SPHK5                AND TRY AGAIN                               
         EJECT                                                                  
* STALAST                                                                       
*                                                                               
UN200    LA    R4,P                                                             
         USING UNLINED,R4                                                       
         OC    TOTSTA,TOTSTA                                                    
         BZ    EXIT                                                             
         LA    R1,TOTSTA                                                        
         BAS   RE,UNFMT                                                         
         MVC   UNSTA(14),=CL14'STATION TOTALS'                                  
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
* MKTLAST                                                                       
*                                                                               
UN210    LA    R4,P                                                             
         USING UNLINED,R4                                                       
         OC    TOTMKT,TOTMKT                                                    
         BZ    EXIT                                                             
         LA    R1,TOTMKT                                                        
         BAS   RE,UNFMT                                                         
         MVC   UNSTA(14),=CL14'MARKET  TOTALS'                                  
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
* EST LAST (PRODUCT TOTALS)                                                     
*                                                                               
UN220    LA    R4,P                                                             
         USING UNLINED,R4                                                       
         OC    TOTEST,TOTEST                                                    
         BZ    EXIT                                                             
         GOTO1 REPORT              SKIP A LINE                                  
         LA    R1,TOTEST                                                        
         BAS   RE,UNFMT                                                         
         MVC   UNSTA(14),=CL14'PRODUCT TOTALS'                                  
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
* CLT LAST                                                                      
*                                                                               
UN230    LA    R4,P                                                             
         USING UNLINED,R4                                                       
         OC    TOTCLT,TOTCLT                                                    
         BZ    EXIT                                                             
         GOTO1 REPORT              SKIP A LINE                                  
         LA    R1,TOTCLT                                                        
         BAS   RE,UNFMT                                                         
         MVC   UNMKT(14),=CL14'CLIENT  TOTALS'                                  
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
* RUNLAST                                                                       
* PRINT PRODUCT CHANGE REPORT IF REQUESTED                                      
*                                                                               
UN240    CLI   SORTOPEN,C'Y'                                                    
         BNE   EXIT                                                             
         MVI   MODE,STALAST                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,5                                                       
         MVC   QEST,=C'ALL'                                                     
         MVC   QESTEND,SPACES                                                   
         MVI   SPSUPMKT,C'N'                                                    
         MVI   SORTEND,C'N'                                                     
         XC    WRECSV,WRECSV                                                    
*                                                                               
UN242    DS    0H                  GET SORT RECORDS                             
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   RF,15,4(R1)                                                      
         BNZ   *+12                                                             
         MVI   SORTEND,C'Y'                                                     
         B     UN243                                                            
         MVC   WREC,0(RF)                                                       
         LA    R4,P                                                             
         USING SPLINED,R4                                                       
         CLC   WKEY,WKEYSV         TEST KEY CHANGE                              
         BNE   *+12                                                             
         LA    R2,1(R2)            NO-AUGMENT N'SPOTS                           
         B     UN242                  AND GET NEXT RECORD                       
         OC    WKEYSV,WKEYSV       YES-TEST FIRST TIME                          
         BZ    UN244                                                            
*                                                                               
UN243    EDIT  (R2),(4,SPSPT)      NO-EDIT N'SPOTS                              
         CLC   WKEY(WPRD2-WKEY),WKEYSV                                          
         BE    *+8                                                              
         MVI   SPACING,2              SPACE AFTER IF DATE CHANGE                
         GOTO1 REPORT                 AND PRINT A LINE                          
         CLI   SORTEND,C'Y'                                                     
         BE    UN260                                                            
*                                                                               
UN244    LA    R2,1                RESET SPOT COUNTER                           
         CLC   WMED,MED            TEST MEDIA CHANGE                            
         BE    UN246                                                            
         MVC   MED,WMED            YES-GET MEDIA NAME                           
         GOTO1 MEDGET,DMCB,(MED,AGY),DATAMGR,WORK                               
         CLI   8(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   BAGYMD,WORK                                                      
         MVC   MEDNM,WORK+1                                                     
*                                                                               
UN246    MVC   MKT,WMKT            SET MARKET                                   
         MVC   MKTNM,WMKTNM                                                     
         MVC   BIGSTA(8),WSTA      SET STATION                                  
         MVC   SVSYSNM,WSYSNAME                                                 
         MVC   CLT,WCLT            SET CLIENT AND PRODUCT                       
         MVC   CLTNM,SPACES                                                     
         MVC   CLTNM(L'WCLTNM),WCLTNM                                           
         MVC   PRODUCT,WPRD                                                     
         MVC   PRDNM,SPACES                                                     
         MVC   PRDNM(L'WPRDNM),WPRDNM                                           
         MVC   QSTART,WSTART       SET REQUEST DATES                            
         MVC   QEND,WEND                                                        
*                                                                               
         CLC   WKEY(WCLT-WKEY),WKEYSV   TEST NEW STATION                        
         BE    UN247                                                            
         MVI   FORCEHED,C'Y'            YES-FORCE BLANK PAGE                    
         MVI   P,0                                                              
         MVI   RCSUBPRG,6                                                       
         GOTO1 REPORT                                                           
         MVI   RCSUBPRG,5                                                       
         MVC   PAGE,=H'1'               AND START NEW REPORT                    
*                                                                               
UN247    CLC   WKEY(WEST-WKEY),WKEYSV   TEST NEW PERIOD                         
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'            YES-FORCE PAGE BREAK                    
*                                                                               
         MVI   NEWEST,C'N'                                                      
         CLC   WKEY(WDAYSEQ-WKEY),WKEYSV  TEST NEW ESTIMATE                     
         BE    *+8                                                              
         MVI   NEWEST,C'Y'         YES                                          
         MVI   HEADHK,C'N'                                                      
         BAS   RE,SPFMT            FORMAT BUYLINE DETAILS                       
         MVI   TRADEBUY,C'N'                                                    
         CLC   AGY,=C'DF'          TEST SAATCHI                                 
         BNE   UN248                                                            
         LA    R1,WPRG             AND IT'S A TRADE BUY                         
         LA    R0,L'WPRG-1                                                      
         CLC   0(2,R1),=C'-T'                                                   
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         B     UN248                                                            
         MVI   TRADEBUY,C'Y'       YES                                          
*                                                                               
UN248    CLI   WPRD2,0             NEW PRODUCT                                  
         BNE   *+14                                                             
         MVC   SPPRD(7),=C'DELETED'                                             
         B     UN250                                                            
         CLI   WPRD2,X'FF'                                                      
         BNE   *+14                                                             
         MVC   SPPRD(5),=C'UNALL'                                               
         B     UN250                                                            
         MVC   SPPRD,WPRDCD2                                                    
         MVC   SPPRDNM,WPRDNM2                                                  
*                                                                               
UN250    CLI   TRADEBUY,C'Y'       FORMAT THE COST                              
         BNE   *+14                                                             
         MVC   SPCOST(5),=C'TRADE'                                              
         B     UN252                                                            
         EDIT  (B3,WCOST),(10,SPCOST),2,ALIGN=LEFT,FLOAT=$                      
*                                                                               
UN252    MVC   WRECSV,WREC         SAVE THE RECORD                              
         B     UN242               GET NEXT RECORD                              
*                                                                               
UN260    MVI   MODE,RUNLAST                                                     
         B     EXIT                                                             
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
*                                                                               
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RE,RE               EXIT WITH CC EQ                              
         BR    RE                                                               
*                                                                               
NEXTELX  LTR   RE,RE               EXIT WITH CC NEQ                             
         BR    RE                                                               
*                                                                               
ELCDLO   DC    X'00'                                                            
ELCDHI   DC    X'00'                                                            
         EJECT                                                                  
* R4 HAS PRINT LINE ADDRESS                                                     
* R1 HAS ACCUM ADDRESS                                                          
*                                                                               
UNFMT    NTR1                                                                   
         USING UNLINED,R4                                                       
         LA    RF,5                                                             
         LA    R3,UNPRD1                                                        
UNFMT2   OC    0(8,R1),0(R1)                                                    
         BZ    UNFMT4                                                           
         L     R0,0(R1)                                                         
         EDIT  (R0),(5,(R3))       SPOTS                                        
         L     R0,4(R1)                                                         
         EDIT  (R0),(10,6(R3)),2   DOLLARS                                      
*                                                                               
UNFMT4   LA    R3,17(R3)                                                        
         LA    R1,8(R1)                                                         
         BCT   RF,UNFMT2                                                        
         B     EXIT                                                             
         SPACE 2                                                                
* FORMAT BUYLINE DETAILS FOR PRODUCT CHANGE REPORT                              
* R4=A(PRINT LINE)                                                              
*                                                                               
SPFMT    NTR1  ,                                                                
         USING SPLINED,R4                                                       
         CLI   HEADHK,C'Y'                                                      
         BE    *+12                                                             
         CLI   NEWEST,C'Y'                                                      
         BNE   SPFMT2                                                           
         ZIC   RE,WEST                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SPEST,DUB                                                        
         MVC   SPESTNM,WESTNM                                                   
*                                                                               
SPFMT2   CLI   HEADHK,C'Y'                                                      
         BE    *+14                                                             
         CLC   WKEY(WDAT-WKEY),WKEYSV                                           
         BE    SPFMT4                                                           
         GOTO1 CODAY,DMCB,WDAY,SPDAY     (REALLY DAYUNPK)                       
         GOTO1 UNTIME,DMCB,WTIM,SPTIM                                           
         MVC   SPPRG,WPRG                                                       
         EDIT  WLEN,(3,SPLEN)                                                   
*                                                                               
SPFMT4   CLI   HEADHK,C'Y'                                                      
         BE    *+14                                                             
         CLC   WKEY(WPRD2-WKEY),WKEYSV                                          
         BE    SPFMTX                                                           
         GOTO1 DATCON,DMCB,(2,WDAT),(4,SPDAT)                                   
*                                                                               
SPFMTX   B     EXIT                                                             
         EJECT                                                                  
UNLINED  DSECT                                                                  
*                                                                               
UNMKT    DS    CL4                                                              
         DS    CL2                                                              
UNSTA    DS    CL8                                                              
         DS    CL1                                                              
UNEST    DS    CL3                                                              
         DS    CL1                                                              
UNLIN    DS    CL3                                                              
         DS    CL2                                                              
UNPRD1   DS    CL16                99999 9999999.99                             
         DS    CL1            COL  2...3....3....4.                             
UNPRD2   DS    CL16                5   0    5    0                              
         DS    CL1                                                              
UNPRD3   DS    CL16                                                             
         DS    CL1                                                              
UNPRD4   DS    CL16                                                             
         DS    CL1                                                              
UNPRD5   DS    CL16                                                             
         SPACE 2                                                                
SPLINED  DSECT                     PRINT LINE FOR PRODUCT CHANGE RPT            
*                                                                               
SPEST    DS    CL3                                                              
         DS    CL1                                                              
SPESTNM  DS    CL20                                                             
         DS    CL1                                                              
SPDAY    DS    CL8                                                              
         DS    CL1                                                              
SPTIM    DS    CL11                                                             
         DS    CL1                                                              
SPPRG    DS    CL18                                                             
         DS    CL1                                                              
SPLEN    DS    CL3                                                              
         DS    CL2                                                              
SPDAT    DS    CL5                                                              
         DS    CL2                                                              
SPPRD    DS    CL3                                                              
         DS    CL1                                                              
SPPRDNM  DS    CL20                                                             
         DS    CL1                                                              
SPSPT    DS    CL4                                                              
         DS    CL2                                                              
SPCOST   DS    CL10                                                             
         EJECT                                                                  
SPK202   CSECT                                                                  
         CNOP  0,4                                                              
         USING *,RF                                                             
UNHDHK   NTR1                                                                   
         LM    R7,RC,HDHKR7                                                     
         B     HDHK2                                                            
HDHKR7   DC    6F'0'                                                            
         DROP  RF                                                               
*                                                                               
HDHK2    CLI   RCSUBPRG,4          TEST PRODUCT CHANGE REPORT                   
         BH    HDHK10              YES                                          
         CLI   MKTSW,C'Y'          NO-TEST MKT NAME PRINTED YET                 
         BNE   HDHK4               NO                                           
         CLI   MODE,MKTLAST                                                     
         BH    HDHK4                                                            
         MVC   MID1(24),MKTNM                                                   
         MVC   MID1+25(11),=C'(CONTINUED)'                                      
         CLI   MODE,MKTLAST                                                     
         BE    HDHK4                                                            
         LA    R4,P                                                             
         USING UNLINED,R4                                                       
         MVC   UNSTA,BIGSTA                                                     
*                                                                               
HDHK4    DS    0H                                                               
         CLC   QAGY,=C'DF'         IF NOT AGENCY DF JUST DO USUAL               
         BNE   HDHK5               ROUTINE                                      
*                                                                               
         CLI   Q2OPT1,C'T'         TRADE BUYLINES                               
         BNE   *+10                                                             
         MVC   H5+50(14),=C'TRADE BUYLINES'                                     
         CLI   Q2OPT1,C'C'           CASH  BUYLINES                             
         BNE   *+10                                                             
         MVC   H5+50(13),=C'CASH BUYLINES'                                      
*                                                                               
HDHK5    CLI   RCSUBPRG,0          TEST UNALL REQUEST                           
         BNE   HDHK6                                                            
         CLC   =C'DELETE',QAREA+49                                              
         BNE   HDHKX                                                            
         MVC   H8+26(11),=C'--DELETED--'                                        
         B     HDHKX                                                            
*                                                                               
HDHK6    ZIC   RF,RCSUBPRG                                                      
         LA    R5,QAREA+49                                                      
         LA    R6,H8+51                                                         
         MVC   0(3,R6),0(R5)                                                    
         LA    R5,4(R5)                                                         
         LA    R6,17(R6)                                                        
         BCT   RF,*-14                                                          
         B     HDHKX                                                            
*                                  PRODUCT CHANGE REPORT                        
HDHK10   CLI   P,0                 TEST BLANK SEPERATOR PAGE                    
         BE    HDHKX               YES                                          
         MVC   H6+38(6),=C'MARKET'                                              
         MVC   H6+46(4),MKT                                                     
         MVC   H6+53(24),MKTNM                                                  
         MVC   H7+38(7),=C'STATION'                                             
         MVC   H7+46(8),BIGSTA                                                  
         CLC   SVSYSNM,SPACES      CABLE SYSTEM NAME (IF ANY)                   
         BNH   *+10                                                             
         MVC   H7+55(L'SVSYSNM),SVSYSNM                                         
         CLI   NEWEST,C'Y'         TEST FIRST LINE FOR AN ESTIMATE              
         BE    HDHKX                                                            
         MVC   P2,P                NO-CONTINUE PREVIOUS ESTIMATE                
         MVC   P,SPACES                                                         
         MVC   P(11),=C'(CONTINUED)'                                            
         MVI   HEADHK,C'Y'                                                      
         LA    R4,P2                                                            
         MVC   WRECSV2,WREC                                                     
         MVC   WREC,WRECSV                                                      
         BAS   RE,SPFMT                                                         
         MVC   WREC,WRECSV2                                                     
*                                                                               
HDHKX    B     EXIT                                                             
         EJECT                                                                  
         DS    0D                                                               
TOTBUY   DS    XL40                                                             
TOTSTA   DS    XL40                                                             
TOTMKT   DS    XL40                                                             
TOTEST   DS    XL40                                                             
TOTCLT   DS    XL40                                                             
*                                                                               
BRDTAB   DS    XL128               BRAND SELECTION LIST                         
ADDLIST  DS    XL128               LIST OF ADDED PRD CODES FOR DATAMGR          
*                                                                               
RELO     DS    A                                                                
NEXTBRD  DS    A                   A(NEXT BRDTAB ENTRY)                         
*                                                                               
ERRFLAG  DC    X'00'                                                            
BRDLIST  DS    XL9                 CODE/WGT FOR EACH BRAND + X'00'              
*                                                                               
SVQPRD   DS    CL3                                                              
SVSYSNM  DS    CL(L'SSYSNAME)      CABLE SYSTEM NAME                            
WRECSV   DS    CL(L'WREC)                                                       
         ORG   WRECSV                                                           
WKEYSV   DS    CL(L'WKEY)                                                       
         ORG                                                                    
WRECSV2  DS    CL(L'WREC)                                                       
FIRSTPRD DS    XL1                                                              
NEWPRD   DS    XL1                                                              
NEWEST   DS    CL1                                                              
HEADHK   DS    CL1                                                              
TRADEBUY DS    CL1                                                              
*                                                                               
WREC     DS    0CL194              SORT RECORD                                  
WKEY     DS    0CL63                                                            
WMED     DS    CL1                                                              
WMKT     DS    CL4                                                              
WSTA     DS    CL8                                                              
WCLT     DS    CL3                                                              
WPRD     DS    CL3                                                              
WSTART   DS    CL6                                                              
WEND     DS    CL6                                                              
WEST     DS    XL1                                                              
WDAYSEQ  DS    XL1                                                              
WDAY     DS    XL1                                                              
WTIM     DS    XL4                                                              
WPRG     DS    CL18                                                             
WLEN     DS    XL1                                                              
WDAT     DS    XL2                                                              
WPRD2    DS    XL1                                                              
WCOST    DS    XL3                                                              
*                                                                               
WMKTNM   DS    CL24                                                             
WCLTNM   DS    CL20                                                             
WPRDNM   DS    CL20                                                             
WPRDNM2  DS    CL20                                                             
WPRDCD2  DS    CL3                                                              
WESTNM   DS    CL20                                                             
WSYSNAME DS    CL24                                                             
*                                                                               
*                                                                               
SORTOPEN DS    CL1                                                              
SORTEND  DS    CL1                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,63,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=194'                                   
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'*ESTTAB*'                                                    
ESTTAB   DS    1024C               TABLE FOR OPEN EST CHECKING                  
*                                                                               
ESTNAMES DS    255CL20             ESTIMATE NAME TABLE                          
         EJECT                                                                  
******************** S P E S T C H K ****************************               
*                                                               *               
* SUBROUTINE MAINTAINS TABLE OF ESTIMATE NUMBERS OPEN BY BRAND  *               
* FIRST 32 BYTES ARE LIST OF ESTIMATE NUMBERS IN TABLE          *               
* FOLLOWED BY 4  BYTE ENTRY FOR EACH BRAND (MAX 220)            *               
* IF MORE THAN 32 ESTIMATES OPEN, ABEND OCCURS                  *               
*                                                               *               
* PARAM 1     BYTE  0    PRD CODE                               *               
*                  1-3   BUYREC ADDRESS                         *               
*                                                               *               
* PARAM 2           4                                           *               
*                  5-7   TABLE ADDRESS (1024X'00' FIRST TIME)   *               
*                                                               *               
* PARAM 3           8    ON RETURN X'00' = EST FOUND            *               
*                                  X'FF' = NOT FOUND            *               
*                  9-11  SPWORK ADDRESS                         *               
*                                                               *               
*****************************************************************               
         SPACE 2                                                                
SPESTCHK CSECT                                                                  
         NMOD1 0,SPESTCHK                                                       
         L     RA,8(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LR    R2,R1               SAVE PARAM POINTER                           
         MVI   8(R2),0             CLEAR ERROR IND                              
         L     R3,0(R1)            GET A(REC)                                   
* TEST EST NUM IN TABLE                                                         
         LA    R5,X'80'            SET MASK                                     
         SLL   R5,24               GET IT IN LEFTMOST BIT                       
         L     R1,4(R2)            POINT TO TABLE                               
ESTCHK2  CLI   0(R1),0             TEST E-O-L                                   
         BE    ESTCHK4                                                          
         CLC   0(1,R1),9(R3)       TEST MATCH                                   
         BE    ESTCHK10                                                         
         LA    R1,1(R1)                                                         
         SRL   R5,1                SHIFT MASK                                   
         LTR   R5,R5                                                            
         BNZ   ESTCHK2                                                          
         DC    H'0'                TABLE FULL                                   
*                                                                               
ESTCHK4  MVC   0(1,R1),9(R3)       SET EST NUM IN TABLE                         
*                                                                               
ESTCHK10 DS    0H                                                               
         ZIC   R4,0(R2)            GET PRD CODE                                 
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),220                                                        
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  R4,0                                                             
         SLL   R4,2                X 4                                          
         A     R4,4(R2)            ADD TABLE ADDRESS                            
         LA    R4,32(R4)           AND ADD DSPL TO FIRST ENTRY                  
         ST    R5,ESTMASK          SAVE MASK BIT                                
         NC    ESTMASK,0(R4)       TEST EST FOUND PREVIOUSLY                    
         BNZ   ESTCHKX             YES - EXIT                                   
* MUST READ FOR EST KEY                                                         
         ST    R5,ESTMASK          SAVE MASK BIT AGAIN                          
         MVC   ESTCHKSV,KEY        SAVE CONTENTS OF KEY                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),0(R3)      A-M/CLT                                      
         MVC   KEY+7(1),9(R3)      EST                                          
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         LA    R6,CLIST                                                         
         DROP  R6                                                               
*                                                                               
ESTCHK12 CLC   3(1,R6),0(R2)                                                    
         BE    ESTCHK14                                                         
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BH    ESTCHK12                                                         
         DC    H'0'                                                             
*                                                                               
ESTCHK14 MVC   KEY+4(3),0(R6)      PRD CODE                                     
         MVI   8(R2),X'FF'         PRESET EST NOT FOUND                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   *+14                                                             
         OC    0(4,R4),ESTMASK     'OR' IN MASK                                 
         MVI   8(R2),0             AND RESET ERROR FLAG                         
* RESTORE KEY FOR SEQ READING                                                   
         MVC   KEY,ESTCHKSV                                                     
         GOTO1 HIGH                                                             
*                                                                               
ESTCHKX  XMOD1 1                                                                
*                                                                               
ESTCHKSV DS    XL20                                                             
ESTMASK  DS    F                                                                
         LTORG                                                                  
         EJECT                                                                  
* AFTER PRINT OFF INCLUDE SPGENAGY/SPGENCLT/SPGENMKT/SPGENBUY                   
*                         SPREPMODES/SPREPWORKD/SPMEDBLOCK/SPGENEST             
*                         SPGENSTA                                              
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPMEDBLOCK                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
SPWORKD  DSECT                                                                  
         ORG   Q2USER COL                                                       
Q2OPT1   DS    CL1    21                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037SPREPK202 04/04/11'                                      
         END                                                                    
