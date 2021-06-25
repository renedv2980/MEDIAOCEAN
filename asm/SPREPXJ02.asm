*          DATA SET SPREPXJ02  AT LEVEL 109 AS OF 03/31/04                      
*PHASE SPXJ02A                                                                  
*INCLUDE BRDMON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE NETNET                                                                 
         TITLE 'SPREPXJ02 - NEW SPOTPAK ESTIMATE INTERFACE TAPE'                
***********************************************************************         
*                                                                               
*          ****************   NOTE   *******************                        
*                                                                               
*  THIS PROGRAM DOES NOT ASSEMBLE CLEANLY. THE SOURCE WAS MODIFIED IN           
*  AUG/02 TO USE THE PL6 DOLLAR VALUES IN THE BILL HEADER RECORDS, BUT          
*  COULD NOT BE ASSEMBLED BECAUSE IT REFERS TO FIELDS NO LONGER IN              
*  EXISTENCE. THE LIVE LOAD MODULE WAS *ZAPPED* TO REFLECT THE                  
*  MODIFICATIONS AT LEVEL 25.                                                   
*                                                                               
*  PRESUMABLY, NONE OF THIS REALLY MATTER, SINCE WE DON'T BELIEVE THAT          
*  THIS PROGRAM IS IN USE ANY LONGER.                                           
*                                                                               
***********************************************************************         
                                                                                
***********************************************************************         
*                                                                               
*        OPPT1 N=ONE PRD PER PAGE                                               
*        QOPT4 Y=SHOW ONLY REQ PERIOD $                                         
*        QOPT7 Y=HEXOUT OUTPUT (QFILTER+1)                                      
*                                                                               
*        NOTE - FOR NETPAK, CALENDAR MONTHS ARE ASSUMED,                        
*               FOR SPOTPAK, BROADCAST MONTHS                                   
*                                                                               
***********************************************************************         
         PRINT NOGEN                                                            
SPXJ02   CSECT                                                                  
         NMOD1 0,SPXJ02,RR=R2                                                   
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    R7,4095(RB)                                                      
         LA    R7,1(R7)                                                         
         USING SPXJ02+4096,R7      ** NOTE USE OF SECOND BASE REG **            
         LA    RC,SPACEND                                                       
         USING XJWORKD,RC                                                       
         ST    R2,RELO                                                          
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,REQFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
         CLI   MODE,PRDFRST                                                     
         BE    PRDF                                                             
         CLI   MODE,ESTFRST                                                     
         BE    ESTF                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
RUNF     DS    0H                  RUNFIRST                                     
         B     EXIT                                                             
*                                                                               
RUNL     DS    0H                  RUN LAST                                     
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+2(14),=C'RECORDS OUTPUT'                                       
         EDIT  OUTCNT,(9,P+18),0,COMMAS=YES                                     
         GOTO1 REPORT                                                           
         CLOSE (OUTFILE)                                                        
         B     EXIT                                                             
         EJECT                                                                  
INITIAL  DS    0H                                                               
         CLI   INITSW,0                                                         
         BNE   INIT1                                                            
         ZAP   OUTCNT,=P'0'                                                     
         LA    R2,OUTFILE                                                       
         OPEN  ((R2),OUTPUT)                                                    
         MVI   INITSW,1                                                         
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         MVI   NETPAKSW,C'Y'                                                    
         CLI   MCNETPAK,C'Y'                                                    
         BE    *+8                                                              
         MVI   NETPAKSW,C'N'                                                    
         DROP  RF                                                               
*                                                                               
INIT1    DS    0H                                                               
         STM   R7,RC,HDHKR7                                                     
         LA    R0,ESTHDHK                                                       
         ST    R0,HEADHOOK                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   RQALLPOL,C'Y'                                                    
         MVI   RQSTDTLN,4          TO ALLOW YYMM-YYMM                           
         L     R8,ADAGY                                                         
         USING AGYHDR,R8                                                        
         MVC   SVAPROF,AGYPROF     SAVE AGYPROF                                 
         DROP  R8                                                               
         CLC   QSTART(12),SPACES                                                
         BNE   INIT2                                                            
         MVC   QSTAUTO(2),=C'ES'                                                
         MVC   BQSTART(6),=X'000000FFFFFF'                                      
         MVC   BQSTARTP(4),=X'0000FFFF'                                         
*                                                                               
INIT2    DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
         CLC   QEST(2),=C'NO'                                                   
         BNE   INIT5                                                            
         MVI   RCSUBPRG,4                                                       
         CLI   QOPT1,C'N'          SEE IF DOING ONE PRD PER PAGE                
         BNE   INITX                                                            
         MVI   RCSUBPRG,3                                                       
         B     INITX                                                            
*                                                                               
INIT5    DS    0H                                                               
         CLI   QOPT1,C'N'                                                       
         BNE   INITX                                                            
         MVI   RCSUBPRG,2                                                       
*                                                                               
INITX    DS    0H                  OPEN TAPE OUTPUT                             
         B     EXIT                                                             
*        EJECT                                                                  
*                                                                               
CLTF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         XC    SAVEPRD,SAVEPRD                                                  
         L     R8,ADCLT                                                         
         USING CLTHDR,R8                                                        
         MVC   SVCEXTRA,CEXTRA                                                  
*                                                                               
         CLI   NETPAKSW,C'Y'       TEST NETPAK                                  
         BNE   EXIT                                                             
*                                  SET BINSRCH PARS FOR PEPTAB                  
         SR    R0,R0                                                            
         L     R1,=A(PEPTAB)                                                    
         A     R1,RELO                                                          
         SR    R2,R2                                                            
         LA    R3,PEPTDL                                                        
         LA    R4,PEPTKL           KEY LENGTH                                   
         LH    R5,=Y(PEPTMAX)                                                   
         STM   R0,R5,PEPPARS                                                    
*                                  GET BN PROFILE                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0BN'                                                 
         MVC   WORK+4(2),QAGY                                                   
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),CLT                                                    
         L     RF,ADCLT                                                         
         CLI   COFFICE-CLTHDR(RF),C' '                                          
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE-CLTHDR(RF)                                    
         GOTO1 GETPROF,DMCB,WORK,PROFBN,DATAMGR                                 
*                                  GET NETIO                                    
         OC    ANETIO,ANETIO       ONCE ONLY                                    
         BNZ   NTU08                                                            
*                                                                               
         GOTO1 LOADER,DMCB,=C'T00A27  '    LOAD IN NETIO                        
         MVC   ANETIO,DMCB+4                                                    
*                                                                               
         L     R4,ADBUY            USE AS WORK AREA FOR OPEN                    
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',NUFLIST,(R4)                      
         B     NTU08                                                            
*                                                                               
NUFLIST  DC    CL8'UUNTFIL'        U= OPEN FOR UPDATE                           
         DC    CL8'UUNTDIR'                                                     
         DC    CL10'X'                                                          
*                                                                               
*                                                                               
NTU08    DS    0H                                                               
         L     R8,=A(NETBLK)                                                    
         A     R8,RELO                                                          
         ST    R8,ANETBLK                                                       
         USING NETBLOCK,R8                                                      
*                                                                               
         LA    RE,NETBLOCK                                                      
         LH    RF,=Y(NBBLKEND-NETBLOCK)                                         
         XCEF                                                                   
*                                  SET SELECT OPTIONS                           
         MVC   NBSELAGY(3),QAGY    AGY/MED                                      
         MVC   NBSELCLI,CLT        CLT                                          
         MVC   NBSELSTR(12),QSTART                                              
         CLI   NBSELSTR+4,C' '       IF NO START DAY                            
         BH    *+10                                                             
         MVC   NBSELSTR+4(2),=C'01'  SET TO START OF MONTH                      
         CLI   NBSELEND+4,C' '       IF NO END DAY                              
         BH    *+10                                                             
         MVC   NBSELEND+4(2),=C'31'  SET TO END OF MONTH                        
*                                  SET DATA OPTIONS                             
         MVI   NBDATA,C'U'         UNITS                                        
         MVI   NBSEQ,C'N'          DATE SEQ                                     
         MVC   NBTRCOPT,RCTRACE    TRACE                                        
         MVI   NBFUNCT,NBFNORM     NORMAL FUNCTION                              
*                                                                               
         MVC   NBAIO,ADBUY         USE BUY RECORD AREA                          
         MVC   NBPRINT,PRINT                                                    
         MVC   NBLOADER,LOADER                                                  
         MVC   NBACOM,ACOMFACS                                                  
*                                                                               
NTU10    DS    0H                                                               
         GOTO1 ANETIO,DMCB,NETBLOCK                                             
         CLI   NBERROR,NBINVPRD    NO PRODUCT SET UP?                           
         BE    EXIT                                                             
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   NBMODE,NBPROCUN                                                  
         BE    NTU12                                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    NTU30                                                            
         B     NTU10                                                            
*                                                                               
         SPACE 3                                                                
*        PROCESS UNIT                                                           
         SPACE 2                                                                
NTU12    DS    0H                                                               
         TM    NBUNITST,X'42'      SKIP PRE-EMPT OR MISSED                      
         BNZ   NTU10                                                            
         L     RF,NBAIO                                                         
         TM    NURSTAT-NURECD(RF),X'80'       SKIP DELETES                      
         BNZ   NTU10                                                            
*                                                                               
         LA    R2,WORK             SET PEPTAB ENTRY                             
         USING PEPTABD,R2                                                       
         XC    WORK,WORK                                                        
         MVC   PEPTEST,NBACTEST                                                 
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(3,DUB)                                 
         MVC   PEPTPER,DUB                                                      
*                                                                               
         GOTO1 NETCOST,DMCB,ANETBLK,X                                           
*                                                                               
         LA    R6,X                                                             
         USING NCOSTD,R6                                                        
         MVC   PEPTPRD,NBPRD       PRODUCT ONE                                  
         MVC   PEPTGRS,NCP1GRS     PRODUCT 1 GROSS                              
         MVC   PEPTNET,NCP1NET     AND NET                                      
*                                                                               
NTU14    DS    0H                                                               
         GOTO1 =V(BINSRCH),PEPPARS,(1,PEPTABD),RR=RELO                          
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                BINSRCH TABLE FULL                           
*                                                                               
         CLI   0(R1),1             TEST ENTRY ALREADY THERE                     
         BE    NTU20                                                            
         L     R1,0(R1)                                                         
*                                                                               
         ICM   RF,15,PEPTGRS-PEPTABD(R1)   YES, ADD THIS ENTRY TO IT            
         ICM   RE,15,PEPTGRS                                                    
         AR    RF,RE                                                            
         STCM  RF,15,PEPTGRS-PEPTABD(R1)                                        
*                                                                               
         ICM   RF,15,PEPTNET-PEPTABD(R1)                                        
         ICM   RE,15,PEPTNET                                                    
         AR    RF,RE                                                            
         STCM  RF,15,PEPTNET-PEPTABD(R1)                                        
*                                                                               
NTU20    DS    0H                                                               
         CLI   NBPRD2,0            IS THERE A PIGGY                             
         BE    NTU10               NO, DONE                                     
         CLC   PEPTPRD,NBPRD2      HAVE WE DONE IT?                             
         BE    NTU10               YES, ALL DONE                                
*                                                                               
         MVC   PEPTPRD,NBPRD2      ELSE DO IT NOW                               
         MVC   PEPTGRS,NCP2GRS                                                  
         MVC   PEPTNET,NCP2NET                                                  
         B     NTU14                                                            
*                                                                               
         DROP  R2                                                               
         DROP  R6                                                               
*                                                                               
NTU30    DS    0H                                                               
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
PRDF     DS    0H                                                               
         L     R8,ADPRD                                                         
         USING PRDHDR,R8                                                        
         CLC   SAVEPRD,PKEYPRD                                                  
         BE    EXIT                                                             
         CLI   QOPT1,C'N'          SEE IF DOING ONE PRD PER PAGE                
         BNE   PRDF5                                                            
         MVI   FORCEHED,C'Y'       YES                                          
         MVC   SAVEPRD,PKEYPRD                                                  
         MVC   SAVEPACC,PACCT                                                   
         MVI   PPNTSW,1                                                         
         B     EXIT                                                             
*                                                                               
PRDF5    DS    0H                                                               
         MVC   SAVEPRD,PKEYPRD                                                  
         MVC   SAVEPNAM,PNAME                                                   
         MVC   SAVEPACC,PACCT                                                   
         MVI   PPNTSW,0                                                         
         B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
ESTF     DS    0H                                                               
         CLC   QEST(2),=C'NO'      IF USING FILTERS MUST READ                   
         BNE   ESTF4               ESTS MYSELF - FROM ESTLST                    
         MVC   SAVESKEY,KEY         SAVE SPONSOR'S KEY                          
         MVI   DISP,0                                                           
*                                                                               
ESTF1    DS    0H                                                               
         LA    R8,ESTLST                                                        
         ZIC   R0,DISP                                                          
         AR    R8,R0                                                            
         LA    R8,1(R8)            BUMP PAST EST JUST DONE                      
ESTF2    CLI   0(R8),0                                                          
         BNE   ESTF3                                                            
         LA    R8,1(R8)                                                         
         LA    R5,ESTLST+255                                                    
         CR    R8,R5               SEE IF I'M AT END OF LIST                    
         BNH   ESTF2                                                            
         MVC   KEY,SAVESKEY        RESTORE SPONSOR'S KEY                        
         GOTO1 HIGH                RESTORE SEQ READ                             
         B     EXIT                                                             
*                                                                               
ESTF3    DS    0H                                                               
         MVC   DISP,0(R8)          SAVE THIS DISPLACEMENT INTO ESTLST           
*                                  AS STARTING POINT TO CONTINUE SEARCH         
*                                  FOR ESTS TO REPORT                           
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),PRD                                                     
         MVC   KEY+7(1),0(R8)                                                   
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY                                                   
         BNE   ESTF1               NOT FOUND - BYPASS                           
         CLI   KEY+8,0             MUST BE AN EST                               
         BNE   ESTF1                                                            
         GOTO1 GETEST                                                           
ESTF4    DS    0H                                                               
         L     R8,ADEST                                                         
         USING ESTHDR,R8                                                        
         MVI   DOLSW,0                                                          
         MVI   LINEED,0                                                         
         XC    SVNETYM,SVNETYM                                                  
*                                                                               
         LA    R6,GOTOTS           CLEAR ESTIMATE ACCUMS                        
         LA    R5,NUMROWS*NUMCOLS                                               
*                                                                               
ESTF5    ZAP   0(8,R6),=P'0'                                                    
         LA    R6,8(R6)                                                         
         BCT   R5,ESTF5                                                         
*                                                                               
         LA    R5,NUMROWS                                                       
         LA    R6,ESTTOTS                                                       
ESTF6    ZAP   0(8,R6),=P'0'                                                    
         LA    R6,8(R6)                                                         
         BCT   R5,ESTF6                                                         
*                                                                               
         BAS   RE,BLDMLST          BUILDS EST MTH LIST                          
*                                  AND DELETES EST $ OUT OF REQ PERIOD          
*                                  AND HANDLES NETPAK ORDERED                   
*                                                                               
         LA    R6,ENET                                                          
         LA    R5,12                                                            
         ZAP   0(6,R6),=P'0'       CLEAR SO NETPAK CAN POST                     
         LA    R6,6(R6)                                                         
         BCT   R0,*-10                                                          
* ADD ORDERED TODAY TO ORDERED YTD                                              
         LA    R6,EORD                                                          
         LA    R4,GOTOTS                                                        
         LA    R5,12                                                            
         BAS   RE,ESTROLL                                                       
         B     ESTF30                                                           
*                                                                               
ESTROLL  AP    0(8,R4),0(6,R6)                                                  
         AP    0(8,R4),78(6,R6)                                                 
         LA    R4,8(R4)                                                         
         LA    R6,6(R6)                                                         
         BCT   R5,ESTROLL                                                       
         BR    RE                                                               
*                                                                               
ESTF30   DS    0H                                                               
         MVC   SAVEKEY,KEY         READ BILLING RECORDS                         
         XC    KEY,KEY                                                          
         MVC   KEY(13),EKEY                                                     
         GOTO1 HIGH                                                             
*                                                                               
ESTF35   GOTO1 SEQ                                                              
         CLC   KEYSAVE(8),KEY      CHK SAME A/M CLT PRD EST                     
         BNE   ESTF60                                                           
         GOTO1 GETBILL                                                          
*                                                                               
         DROP  R8                                                               
*                                                                               
         L     R8,ADBILL                                                        
         USING BILLREC,R8                                                       
         CLI   BKEY+9,12                                                        
         BNH   *+8                                                              
         MVI   BKEY+9,12           FUNNY BILLING PERIOD - MAKE DEC              
         ZIC   R6,BKEY+9                                                        
         BCTR  R6,0                                                             
         LA    R1,DELMTHS                                                       
         AR    R1,R6                                                            
         CLI   0(R1),1                                                          
         BE    ESTF35              SKIP BILLS OUT OF REQ PERIOD                 
*                                                                               
ESTF40   DS    0H                                                               
         ZIC   R6,BKEY+9                                                        
         BCTR  R6,0                                                             
         SLL   R6,3                X 8                                          
         LA    R2,TOTBILL                                                       
         AR    R2,R6               GET RIGHT MTH                                
         AP    0(8,R2),BGRSP                                                    
         AP    EBILTOT,BGRSP                                                    
*                                  NOW DO NET BILLINGS                          
         LA    R2,NTOTBILL                                                      
         AR    R2,R6               DISPLACEMENT                                 
         L     R5,0(R2)                                                         
         AP    0(8,R2),BNETP                                                    
         AP    ENBILTOT,BNETP                                                   
         B     ESTF35                                                           
*                                                                               
         DROP  R8                                                               
*                                                                               
ESTF60   DS    0H                                                               
         L     R8,ADEST                                                         
         USING ESTHDR,R8                                                        
         MVC   KEY,SAVEKEY         RESTORE SEQ READ                             
         GOTO1 HIGH                                                             
*                                                                               
         SR    R6,R6                                                            
         LA    R4,GOTOTS                                                        
         LA    R5,ACCNUM                                                        
*                                                                               
ESTF62   LR    R4,R1                                                            
         BAS   RE,TSTZERO                                                       
         BZ    ESTF63                                                           
         LA    R6,1(R6)                                                         
         MVI   DOLSW,1             SET FOR ACTIVITY                             
*                                                                               
ESTF63   AHI   R4,NCOLS*8                                                       
         BCT   R5,ESTF62                                                        
*                                                                               
         LA    R6,3(R6)            BUMP FOR MTH HEADS                           
         STC   R6,LINEED                                                        
         CLI   DOLSW,0             SEE IF DOLLARS FOUND                         
         BE    ESTXX               SKIP                                         
*                                                                               
ESTF68   CLI   PPNTSW,1            SEE IF I NEED TO PRINT PRD                   
         BE    ESTF70              NO                                           
         MVC   MID1(7),=C'PRODUCT'                                              
         MVC   MID1+8(3),SAVEPRD                                                
         MVC   MID1+12(20),SAVEPNAM                                             
         LA    RF,MID1+31                                                       
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   2(RF),C'('                                                       
         MVC   3(4,RF),SAVEPACC                                                 
         MVI   7(RF),C')'                                                       
         CLI   SAVEPACC,X'FF'                                                   
         BNE   ESTF69                                                           
         OI    SAVEPACC+3,X'0F'                                                 
         UNPK  3(5,RF),SAVEPACC+1(3)                                            
         MVI   8(RF),C')'                                                       
ESTF69   DS    0H                                                               
         MVC   MID2(11),=11C'-'                                                 
         CLI   MID1+10,C' '                                                     
         BNE   *+8                                                              
         MVI   MID2+10,C' '                                                     
         MVI   FORCEMID,C'Y'                                                    
         AI    LINEED,3                                                         
*                                                                               
ESTF70   DS    0H                                                               
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1(3),DUB                                                        
         TM    ECNTRL,X'04'        CHK HELD                                     
         BZ    ESTF71                                                           
         MVI   P1+3,C'H'                                                        
         B     ESTF72                                                           
*                                                                               
ESTF71   TM    ECNTRL,X'08'        CHK LOCKED                                   
         BZ    ESTF72                                                           
         MVI   P1+3,C'L'                                                        
*                                                                               
ESTF72   DS    0H                                                               
         MVC   P1+5(20),EDESC                                                   
         GOTO1 DATCON,DMCB,(0,ESTART),(5,P1+26)                                 
         MVI   P1+34,C'-'                                                       
         GOTO1 DATCON,DMCB,(0,EEND),(5,P1+35)                                   
         AI    LINEED,5                                                         
         MVI   SPACING,2                                                        
         B     ESTDOLS                                                          
         EJECT                                                                  
ESTDOLS  DS    0H             PRINT DOLLARS                                     
*                             SEE IF EST WILL FIT ON THIS PAGE                  
         ZIC   R0,LINE                                                          
         ZIC   R1,LINEED                                                        
         AR    R0,R1                                                            
         STC   R0,WORK                                                          
         CLC   WORK(1),MAXLINES                                                 
         BNH   ESTD3                                                            
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT1,C'N'         ONE PRD PER PAGE                              
         BE    ESTD3               YES                                          
         MVC   MID1(7),=C'PRODUCT'                                              
         MVC   MID1+8(3),SAVEPRD                                                
         MVC   MID1+12(20),SAVEPNAM                                             
         LA    RF,MID1+31                                                       
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   2(RF),C'('                                                       
         MVC   3(4,RF),SAVEPACC                                                 
         MVI   7(RF),C')'                                                       
         CLI   SAVEPACC,X'FF'                                                   
         BNE   ESTD                                                             
         OI    SAVEPACC+3,X'0F'                                                 
         UNPK  3(5,RF),SAVEPACC+1(3)                                            
         MVI   8(RF),C')'                                                       
ESTD     DS    0H                                                               
         MVC   MID2(11),=11C'-'                                                 
         CLI   MID1+10,C' '                                                     
         BNE   *+8                                                              
         MVI   MID2+10,C' '                                                     
         CLI   PPNTSW,1                                                         
         BNE   ESTD3                                                            
         LA    R1,MID1+39                                                       
ESTD1    CLI   0(R1),C' '                                                       
         BH    ESTD2                                                            
         BCTR  R1,0                                                             
         B     ESTD1                                                            
*                                                                               
ESTD2    MVC   2(11,R1),=C'(CONTINUED)'                                         
*                                                                               
         MVI   FORCEMID,C'Y'                                                    
*                                                                               
ESTD3    DS    0H                                                               
         MVI   PPNTSW,1            SET PRD PRINTED                              
*                                                                               
ESTD5    DS    0H                                                               
         CLI   DOLSW,1             CHK FOR $S                                   
         BE    ESTD10              YES                                          
         LA    R6,P2+26                                                         
         B     ESTD7               PUT MSG IN P2                                
ESTD7    MVC   0(19,R6),=C'*** NO ACTIVITY ***'                                 
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     ESTXX                                                            
*                                                                               
ESTD10   DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P+26(103),MTHLN1                                                 
         MVC   P2+26(103),MTHLN2                                                
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,TITLES                                                        
         LA    R3,ACCNUM                                                        
         LA    R4,GOTOTS                                                        
         LA    R8,ESTTOTS                                                       
*                                                                               
ESTD40   LR    R1,R4                                                            
         BAS   RE,TSTZERO                                                       
         BZ    ESTD50                                                           
         MVC   P1+9(14),0(R2)                                                   
         LA    R5,12                                                            
         LR    RE,R4                                                            
         LA    R6,P1+25                                                         
*                                                                               
ESTD45   ZAP   DUB,0(8,RE)                                                      
         SRP   DUB,64-2,5                                                       
         EDIT  (P8,DUB),(7,0(R6)),0,FLOAT=-                                     
         LA    R6,8(R6)                                                         
         LA    RE,8(RE)                                                         
         BCT   R5,ESTD45                                                        
*                                                                               
         ZAP   DUB,0(8,R8)                                                      
         SRP   DUB,64-2,5                                                       
         EDIT  (P8,DUB),(7,P+122),0,FLOAT=-                                     
         GOTO1 REPORT                                                           
*                                                                               
ESTD50   LA    R2,14(R2)                                                        
         AHI   R4,NUMCOLS*8                                                     
         LA    R8,8(R8)                                                         
         BCT   R3,ESTD40                                                        
*                                                                               
         ZIC   R3,LINE                                                          
         LA    R3,1(R3)                                                         
         STC   R3,X                                                             
         CLC   X(1),MAXLINES                                                    
         BH    ESTDX                                                            
         GOTO1 REPORT              SKIP A LINE                                  
*                                  NOW GO ADD TAPE RECS                         
ESTDX    DS    0H                                                               
*                                  NOW GO ADD TAPE RECS                         
         XC    OUTREC,OUTREC                                                    
         MVI   TPSYS,C'S'          SPOT                                         
         MVC   TPMED,MED                                                        
         MVC   TPCLT,CLT                                                        
         MVC   TPCNUM,=4C'0'                                                    
         DROP  R8                                                               
         L     R8,ADCLT                                                         
         USING CLTHDR,R8                                                        
         OC    CCLTINTR,CCLTINTR                                                
         BZ    ESTTP2                                                           
         MVC   HALF,CCLTINTR                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TPCNUM,DUB                                                       
*                                                                               
         DROP  R8                                                               
ESTTP2   L     R8,ADEST            RESET R8 TO ESTREC                           
         USING ESTHDR,R8                                                        
         MVC   TPPNUM(4),SAVEPACC                                               
         CLI   SAVEPACC,X'FF'      SEE IF NUMERIC                               
         BNE   ESTTP3                                                           
         OI    SAVEPACC+3,X'0F'                                                 
         UNPK  TPPNUM,SAVEPACC+1(3)                                             
ESTTP3   OC    TPPNUM,SPACES                                                    
         MVC   TPPRD,PRD                                                        
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TPEST,DUB+6(2)                                                   
         DROP  R8                                                               
*                                                                               
         LA    R3,TOTBILL                                                       
         LA    R2,NTOTBILL         NET BILLED                                   
         LA    R4,GOTOTS                                                        
         LA    R5,NOTOTS                                                        
         LA    R6,ESTMLST                                                       
         LA    R8,12               FOR BCT                                      
*                                                                               
ESTTP5   CP    0(8,R4),=P'0'       GROSS - ORDERED                              
         BNZ   ESTTP10                                                          
         CP    0(8,R5),=P'0'       NET - ORDERED                                
         BNZ   ESTTP10                                                          
         CP    0(8,R3),=P'0'       GROSS - BILLED                               
         BNZ   ESTTP10                                                          
         CP    0(8,R2),=P'0'       NET - BILLED                                 
         BNZ   ESTTP10                                                          
         B     ESTTPNX                                                          
*                                                                               
ESTTP10  MVC   TPYEAR(2),0(R6)                                                  
         MVC   TPMTH(2),2(R6)                                                   
         UNPK  TPGROSS,0(8,R4)                                                  
         CP    TPGROSS,=P'0'                                                    
         BL    *+8                                                              
         OI    TPGROSS+11,X'F0'                                                 
*                                                                               
         UNPK  TPNET,0(8,R5)                                                    
         CP    TPNET,=P'0'                                                      
         BL    *+8                                                              
         OI    TPNET+11,X'F0'                                                   
*                                                                               
         UNPK  TPBILL,0(8,R3)                                                   
         CP    TPBILL,=P'0'                                                     
         BL    *+8                                                              
         OI    TPBILL+11,X'F0'                                                  
*                                                                               
         UNPK  TPNBILL,0(8,R2)                                                  
         CP    TPNBILL,=P'0'                                                    
         BL    *+8                                                              
         OI    TPNBILL+11,X'F0'                                                 
         BAS   RE,TWRITE                                                        
*                                                                               
ESTTPNX  LA    R4,8(R4)            NEXT BUCKET                                  
         LA    R5,8(R5)                                                         
         LA    R3,8(R3)                                                         
         LA    R2,8(R2)                                                         
         LA    R6,4(R6)            NEXT MTH                                     
         BCT   R8,ESTTP5                                                        
         B     ESTXX                                                            
*                                                                               
ESTXX    CLC   QEST(2),=C'NO'         SEE IF DOING FILTERED ESTS                
         BNE   EXIT                NO DONE                                      
         B     ESTF1               YES GO FIND NEXT EST IN ESTLST               
         EJECT                                                                  
BLDMLST  NTR1                                                                   
*                                  BUILD ESTMLST                                
*                                  AND DELETE $ OUT OF REQ PERIOD               
*                                  AND BUILD TABLE OF MTHS TO IGNORE            
*                                  AND HANDLE NETPAK ORDERED                    
*                                                                               
         L     R8,ADEST            RESET R8 TO ESTREC                           
         USING ESTHDR,R8                                                        
         XC    ESTMLST,ESTMLST                                                  
         XC    DELMTHS,DELMTHS                                                  
         CLI   NETPAKSW,C'Y'       IF NETPAK                                    
         BNE   ESTD20                                                           
         MVC   WORK(6),ESTART      USE ESTART                                   
         B     ESTD25                                                           
*                                                                               
ESTD20   DS    0H                  ELSE GET BROADCAST MONTH START               
         GOTO1 =V(BRDMON),DMCB,(0,ESTART),WORK                                  
*                                                                               
ESTD25   DS    0H                                                               
         LA    R0,12                                                            
         LA    R1,MTHLN1                                                        
         LA    R3,ESTMLST                                                       
         LA    R2,MONTHS                                                        
         ZAP   X(2),=P'1'                                                       
         PACK  DUB,WORK(2)                                                      
         CVB   R5,DUB                                                           
         STH   R5,HALF                                                          
         LA    R5,1(R5)                                                         
         STH   R5,HALF2            YEAR PLUS 1                                  
         PACK  X+4(2),WORK+2(2)                                                 
*                                                                               
ESTD30   MVC   0(3,R1),0(R2)                                                    
         MVI   3(R1),C'/'                                                       
         LH    R4,HALF                                                          
         CP    X(2),X+4(2)                                                      
         BNL   *+8                                                              
         LH    R4,HALF2                                                         
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(2,R1),DUB                                                      
         UNPK  2(2,R3),X(2)        MTH                                          
         OI    3(R3),X'F0'                                                      
         UNPK  0(2,R3),DUB         YESR                                         
         LA    R3,4(R3)                                                         
         AP    X(2),=P'1'                                                       
         LA    R2,3(R2)                                                         
         LA    R1,8(R1)                                                         
         BCT   R0,ESTD30                                                        
*                                                                               
         LA    R0,12                                                            
         LA    R1,MTHLN2                                                        
         MVC   0(6,R1),=6C'-'                                                   
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         MVC   MTHLN1+98(5),=C'TOTAL'                                           
         MVC   MTHLN2+98(5),=5C'-'                                              
*                                                                               
         CLI   QOPT4,C'Y'          SEE IF SHOWING REQ MTHS ONLY                 
         BNE   BLDM50                                                           
         CLC   QSTAUTO(2),=C'ES'                                                
         BE    BLDM50                                                           
*                                                                               
         MVC   WORK(12),QSTART                                                  
         LA    R1,DELMTHS                                                       
         LA    R2,ESTMLST                                                       
         LA    R5,12                                                            
*                                                                               
BLDM8    CLC   0(4,R2),WORK                                                     
         BL    BLDM10                                                           
         CLC   0(4,R2),WORK+6                                                   
         BH    BLDM10                                                           
         B     BLDM15                                                           
*                                                                               
BLDM10   MVI   0(R1),1                                                          
BLDM15   LA    R1,1(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R5,BLDM8                                                         
*                                                                               
BLDM20   DS    0H                  DELETE EST $ OUT OF REQ PERIOD               
         CLI   NETPAKSW,C'Y'       NO FOR NETPAK                                
         BE    BLDM50                                                           
         LA    R1,EAUTH                                                         
         LA    R2,DELMTHS                                                       
         LA    R5,12                                                            
BLDM25   CLI   0(R2),1                                                          
         BNE   *+10                                                             
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R5,BLDM25                                                        
*                                                                               
         LA    R1,ENET                                                          
         LA    R2,DELMTHS                                                       
         LA    R5,12                                                            
BLDM26   CLI   0(R2),1                                                          
         BNE   *+10                                                             
         ZAP   0(6,R1),0(R1)                                                    
         LA    R1,6(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R5,BLDM26                                                        
*                                                                               
         LA    R1,EORD                                                          
         LA    R2,DELMTHS                                                       
         LA    R5,12                                                            
*                                                                               
BLDM30   CLI   0(R2),1                                                          
         BNE   BLDM35                                                           
         ZAP   0(6,R1),=P'0'                                                    
         ZAP   78(6,R1),=P'0'                                                   
*                                                                               
BLDM35   LA    R1,6(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R5,BLDM30                                                        
*                                                                               
         LA    R1,EPAID                                                         
         LA    R2,DELMTHS                                                       
         LA    R5,12                                                            
*                                                                               
BLDM40   CLI   0(R2),1                                                          
         BNE   BLDM45                                                           
         ZAP   0(6,R1),=P'0'                                                    
         ZAP   78(6,R1),=P'0'                                                   
*                                                                               
BLDM45   LA    R1,6(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R5,BLDM40                                                        
         B     BLDMX                                                            
*                                                                               
BLDM50   DS    0H                  NETPAK ORDERED HADNLING                      
         LA    R2,EORD                                                          
         LA    R3,ENET             EXIST ONLY IN THIS PROGRAM                   
         LA    R4,DELMTHS                                                       
         LA    R5,ESTMLST                                                       
         LA    R6,12                                                            
*                                                                               
BLDM52   DS    0H                                                               
         CLI   0(R4),1             TEST MONTH OK                                
         BE    BLDM60              NO, SKIP                                     
         PACK  DUB,0(2,R5)         YEAR                                         
         CVB   R0,DUB                                                           
         STC   R0,HALF                                                          
         PACK  DUB,2(2,R5)         MONTH                                        
         CVB   R0,DUB                                                           
         STC   R0,HALF+1                                                        
*                                                                               
         LA    R8,WORK                                                          
         XC    WORK,WORK                                                        
         USING PEPTABD,R8                                                       
         MVC   PEPTPRD,BPRD                                                     
         MVC   PEPTEST,BEST                                                     
         MVC   PEPTPER,HALF                                                     
*                                                                               
         GOTO1 =V(BINSRCH),PEPPARS,(0,PEPTABD)                                  
         SR    R8,R8                                                            
         ICM   R8,7,1(R1)                                                       
         BZ    BLDM60              NOTHING FOR THIS MONTH                       
*                                                                               
         L     R0,PEPTGRS                                                       
         CVD   R0,DUB                                                           
         AP    0(6,R2),DUB         GROSS                                        
*                                                                               
         L     R0,PEPTNET                                                       
         CVD   R0,DUB                                                           
         AP    0(6,R3),DUB                                                      
         DROP  R8                                                               
*                                                                               
BLDM60   DS    0H                                                               
         LA    R2,6(R2)                                                         
         LA    R3,6(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R6,BLDM52                                                        
*                                                                               
BLDMX    XIT1                                                                   
         EJECT                                                                  
         DC    F'0'                                                             
TWRITE   ST    RE,TWRITE-4                                                      
         LA    R1,OUTFILE                                                       
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         AP    OUTCNT,=P'1'                                                     
         CLI   QFILTER+1,C'Y'      COL 68                                       
         BNE   TWRITE8                                                          
         GOTO1 HEXOUT,DMCB,OUTREC,P+1,64,0                                      
         GOTO1 REPORT                                                           
TWRITE8  L     RE,TWRITE-4                                                      
         BR    RE                                                               
*                                                                               
TSTZERO  LHI   R0,NUMCOLS                                                       
         CP    0(8,R1),=P'0'                                                    
         BNER  RE                                                               
         LA    R1,8(R1)                                                         
         BCT   R0,*-12                                                          
         BR    RE                                                               
         EJECT                                                                  
*============================================================                   
*        HEADHOOK                                                               
*============================================================                   
         SPACE 1                                                                
         CNOP  0,4                                                              
         USING *,RF                                                             
ESTHDHK  NTR1                                                                   
         LM    R7,RC,HDHKR7                                                     
         B     HDHK2                                                            
*                                                                               
HDHKR7   DC    6F'0'                                                            
         DROP  RF                                                               
*                                                                               
HDHK2    DS    0H                                                               
         CLI   MODE,RUNLAST                                                     
         BE    HDHKX                                                            
         CLI   QOPT1,C'N'          CHK ONE PRD PER PAGE                         
         BNE   HDHK5                                                            
         MVI   H5+34,C'('                                                       
         MVC   H5+35(4),SAVEPACC                                                
         MVI   H5+39,C')'                                                       
         CLI   SAVEPACC,X'FF'                                                   
         BNE   HDHK5                                                            
         OI    SAVEPACC+3,X'0F'                                                 
         UNPK  H5+35(5),SAVEPACC+1(3)                                           
         MVI   H5+40,C')'                                                       
*                                                                               
HDHK5    DS    0H                                                               
         CLC   QSTAUTO(2),=C'ES'                                                
         BE    HDHKX                                                            
         CLC   QSTART+4(2),SPACES                                               
         BNE   HDHK10                                                           
         MVC   H4+47(28),=C'PERIOD FROM MMM/YY TO MMM/YY'                       
         MVC   DUB(6),QSTART                                                    
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,(0,DUB),(6,H4+59)                                    
         MVC   DUB(6),QEND                                                      
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 (RF),(R1),(0,DUB),(6,H4+69)                                      
         B     HDHKX                                                            
*                                                                               
HDHK10   EQU   *                                                                
         MVC   H4+45(32),=C'PERIOD FROM MMMDD/YY TO MMMDD/YY'                   
         GOTO1 DATCON,DMCB,(0,QSTART),(5,H4+57)                                 
         GOTO1 (RF),(R1),(0,QEND),(5,H4+69)                                     
HDHKX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        NET COST - GET TOTAL GROSS/NET FOR UNIT                                
*                                                                               
*      PARM1        A(NETBLOCK)                                                 
*      PARM2        A(OUTPUT) - SEE NCOSTD                                      
*                                                                               
***********************************************************************         
         SPACE 1                                                                
NETCOST  NTR1                                                                   
         L     R8,0(R1)            A(NETBLOCK)                                  
         USING NETBLOCK,R8                                                      
         L     R6,4(R1)            A(OUTPUT) - NCOSTD                           
         USING NCOSTD,R6                                                        
         XC    NCOSTD(NCOSTDL),NCOSTD                                           
*                                  TIME COST                                    
         TM    NBUNITST,X'42'      IF PRE-EMPT OR MISSED                        
         BNZ   NCX                                                              
         BAS   RE,SETTIM           ELSE GET ORDERED TIME CHARGES                
*                                                                               
         MVC   FULL,NBINTEG        INTEGRATION                                  
         TM    NBUNITST,X'80'      TEST MINUS UNIT                              
         BZ    NC4B                                                             
         L     RF,FULL             MAKE AMOUNT NEGATIVE                         
         LNR   RF,RF                                                            
         ST    RF,FULL                                                          
*                                                                               
NC4B     DS    0H                  GET GROSS AND NET                            
         IC    R0,NBRTTYPE         RATE TYPE                                    
         TM    NBPACKST,X'04'      TEST NON-COM INTEGRATION                     
         BZ    *+12                                                             
         LA    R0,C'F'             IF SO, NET=GROSS                             
         B     NC4D                                                             
         CLI   NBSDRTCV,C'T'       IF RATE TYPE COVERAGE                        
         BNE   *+6                 IS FOR TIME ONLY                             
         SR    R0,R0               THEN INTEGRATION NET = 85%                   
*                                                                               
NC4D     DS    0H                                                               
         GOTO1 =V(NETNET),DMCB,((R0),FULL),DUB,RR=RELO                          
         L     R0,NCP1GRS                                                       
         A     R0,DUB                                                           
         ST    R0,NCP1GRS                                                       
         L     R0,NCP1NET                                                       
         A     R0,DUB+4                                                         
         ST    R0,NCP1NET                                                       
*                                                                               
*                                  OTHER CHARGES                                
NC6      DS    0H                                                               
         L     R4,NBAIO                                                         
         USING NURECD,R4                                                        
         LA    R2,NUDATA                                                        
*                                                                               
NC6D     DS    0H                                                               
         CLI   0(R2),X'03'         SPECIAL COST ELEM                            
         BE    NC7B                                                             
         CLI   0(R2),0                                                          
         BE    NC20                                                             
*                                                                               
NC7      DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     NC6D                                                             
*                                                                               
NC7B     DS    0H                                                               
         USING NUSPRD,R2                                                        
*                                                                               
         ICM   R0,15,NUSPRAMT      GROSS                                        
         TM    NBUNITST,X'80'      IF MINUS UNIT                                
         BZ    *+6                                                              
         LNR   R0,R0               MAKE AMOUNT NEGATIVE                         
         ST    R0,FULL                                                          
*                                                                               
         CLI   NUSPRCOM,C'C'       IF NOT COMMISSIONABLE                        
         BE    *+12                                                             
         LA    R0,C'F'             GROSS=NET                                    
         B     NC7F                                                             
         IC    R0,NBRTTYPE         ELSE USE RATE TYPE                           
         CLI   NBSDRTCV,C'A'       IF SPECIALS COVERED                          
         BE    NC7F                                                             
         CLI   NBSDRTCV,C' '                                                    
         BNH   NC7F                                                             
         SR    R0,R0               ELSE NET=85%                                 
*                                                                               
NC7F     DS    0H                                                               
         GOTO1 =V(NETNET),DMCB,((R0),FULL),DUB,RR=RELO                          
*                                                                               
         L     R0,NCP1GRS          ADD IN GROSS                                 
         A     R0,DUB                                                           
         ST    R0,NCP1GRS                                                       
         L     R0,NCP1NET          AND NET                                      
         A     R0,DUB+4                                                         
         ST    R0,NCP1NET                                                       
         B     NC7                 NEXT ELEMENT                                 
*                                                                               
NC20     DS    0H                                                               
         BAS   RE,SETSHR                                                        
*                                                                               
NCX      DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
*        GET ORDERED TIME CHARGES                                               
         SPACE 2                                                                
SETTIM   NTR1                                                                   
         XC    FULL,FULL                                                        
         TM    NBUNITST,X'42'      PRE-EMPT OR MISSED                           
         BNZ   SETTIMX                                                          
         TM    NURSTAT,X'80'       DELETED                                      
         BNZ   SETTIMX             ORDERED = ZERO                               
*                                                                               
         CLI   PROFBN+3,C'Y'       TEST TO BILL ACTUAL                          
         BE    SETTIM4                                                          
         OC    FULL,NBASSIGN       ELSE USE ASSIGNED                            
         BNZ   SETTIM6             IF PRESENT                                   
         TM    NBUNITST,X'08'      OR IF ENTERED AS ZERO                        
         BNZ   SETTIM6                                                          
*                                                                               
SETTIM4  DS    0H                  BILLING ACTUAL                               
         TM    NBUNITST,X'20'      MUST HAVE BEEN ENTERED (7/15/85)             
         BZ    SETTIMX             ELSE ZERO TIME ORDERED                       
         MVC   FULL,NBACTUAL       USE ACTUAL COST                              
*                                                                               
SETTIM6  DS    0H                                                               
         TM    NBUNITST,X'80'      MINUS UNIT                                   
         BZ    *+14                                                             
         L     RF,FULL             MAKE AMOUNT NEGATIVE                         
         LNR   RF,RF                                                            
         ST    RF,FULL                                                          
*                                                                               
         GOTO1 =V(NETNET),DMCB,(NBRTTYPE,FULL),DUB,RR=RELO                      
         MVC   NCP1GRS,DUB                                                      
         MVC   NCP1NET,DUB+4                                                    
*                                                                               
SETTIMX  DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
*                                                                               
SETSHR   NTR1                      GET PIGGY SHARE                              
         CLI   NUPRD2,0            ANY 2ND PRD                                  
         BE    SETSX                                                            
         MVC   DUB(8),NCP1GRS      SAVE TOTALS                                  
         SR    RF,RF                                                            
         ICM   RF,3,NUP1SHR        PRD 1 SHARE                                  
         BNZ   SETS1                                                            
         TM    NUUNST2,X'04'       IF ZERO, TEST TRULY ZERO                     
         BNZ   SETS1                                                            
         LH    RF,=H'5000'         DEFAULT IS 50 PCT                            
SETS1    DS    0H                                                               
         ST    RF,FULL                                                          
         L     RF,=F'10000'        SHARE IS N.NN PCT                            
*                                                                               
         L     R1,NCP1GRS                                                       
         M     R0,FULL             SHARE IS IN FULL                             
         BAS   RE,NCDIV                                                         
         ST    R1,NCP1GRS                                                       
*                                                                               
         L     R1,NCP1NET                                                       
         M     R0,FULL             SHARE IS IN FULL                             
         BAS   RE,NCDIV                                                         
         ST    R1,NCP1NET                                                       
*                                                                               
         L     RF,DUB              SET DIFFERENCE IN PRD 2 SLOTS                
         S     RF,NCP1GRS                                                       
         ST    RF,NCP2GRS                                                       
*                                                                               
         L     RF,DUB+4                                                         
         S     RF,NCP1NET                                                       
         ST    RF,NCP2NET                                                       
*                                                                               
SETSX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
NCDIV    DIV   (R0),(RF)                                                        
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
TITLES   DS    0C                                                               
         DC    CL14' GROSS ORDERED'                                             
         DC    CL14'  GROSS BILLED'                                             
         DC    CL14'   NET ORDERED'                                             
         DC    CL14'    NET BILLED'                                             
         SPACE 2                                                                
OUTFILE  DCB   DDNAME=OUTFILE,DSORG=PS,RECFM=FB,LRECL=80,              X        
               BLKSIZE=800,MACRF=PM                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*PEPTAB*'                                                    
PEPTAB   DS    XL(PEPTMAX*PEPTDL)                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'*NETBLK*'                                                    
NETBLK   DS    4096X                                                            
         EJECT                                                                  
XJWORKD  DSECT                                                                  
*                                                                               
NUMROWS  EQU   4                   NUMBER OF ACCUM  ROWS                        
NUMCOLS  EQU   12                                                               
         DS    0F                                                               
GOTOTS   DS    12PL8               GROSS ORDERED                                
TOTBILL  DS    12PL8               GROSS BILLED                                 
NOTOTS   DS    12PL8               NET ORDERED                                  
NTOTBILL DS    12PL8               NET BILLED                                   
*                                                                               
*                                                                               
ESTTOTS  DS    0D                                                               
EOTOTAL  DS    D                   GROSS TOTAL ORDERED                          
EBILTOT  DS    D                   TOTAL BILLED - GROSS                         
ENTOTAL  DS    D                   NET TOTAL ORDERED                            
ENBILTOT DS    D                   NET BILLED                                   
*                                                                               
OUTCNT   DS    PL5'0'                                                           
INITSW   DS    X                                                                
RELO     DS    F                                                                
*                                                                               
SAVESKEY DS    CL13                                                             
SAVEKEY  DS    CL13                                                             
SAVEPRD  DS    CL3                                                              
SAVEPNAM DS    CL20                                                             
SAVEPACC DS    CL4                 SAVED PRODUCT ACCOUNT                        
SVAPROF  DS    CL20                SAVED AGY PROFILE                            
SVCEXTRA DS    CL15                SAVED CEXTRA                                 
*                                                                               
DISP     DS    CL1                 DISP INTO ESTLST FOR FILTERED ESTS           
DOLSW    DS    CL1                                                              
PPNTSW   DS    CL1                                                              
SVNETYM  DS    CL2                 NONE ZERO IF NETPAK EST                      
LINEED   DS    CL1                                                              
X        DS    CL30                                                             
PROFBN   DS    XL16                                                             
*                                                                               
ESTMLST  DS    CL48                EST MTH LIST IN BUCKET ORDER                 
*                                                                               
DELMTHS  DS    CL12                12 MTHS X'01' MEANS BYPASS THIS MTH          
*                                                                               
MTHLN1   DS    CL106                                                            
MTHLN2   DS    CL106                                                            
*                                                                               
OUTREC   DS    0CL80                                                            
TPSYS    DS    CL1                 SYSTEM                                       
TPMED    DS    CL1                 MEDIA                                        
TPCLT    DS    CL3                 CLIENT                                       
TPCNUM   DS    CL4                 CLT INTERFACE NUMBER                         
TPPRD    DS    CL3                 PRODUCT                                      
TPPNUM   DS    CL5                 PRODUCT ACCOUNT NUMBER                       
TPEST    DS    CL3                 ESTIMATRE                                    
TPYEAR   DS    CL2                 YEAR OF SERVICE                              
TPMTH    DS    CL2                 MTH OF SERVICE                               
TPGROSS  DS    CL12                GROSS ORDERED                                
TPNET    DS    CL12                NET ORDERED                                  
TPBILL   DS    CL12                BILLED                                       
TPNBILL  DS    CL12                BILLED  - NET                                
         DS    CL20                SPARE                                        
*                                                                               
         DS    0F                                                               
PEPPARS  DS    0XL24                                                            
         DS    F                                                                
APEPTAB  DS    A                                                                
         DS    4F                                                               
*                                                                               
ANETIO   DS    A                                                                
ANETBLK  DS    A                                                                
NETPAKSW DS    C                                                                
*                                                                               
NCOSTD   DSECT                     DSECT FOR NETCOST RETURN                     
NCP1GRS  DS    F                   PRD 1 GROSS                                  
NCP1NET  DS    F                         NET                                    
NCP2GRS  DS    F                   PRD 2 GROSS                                  
NCP2NET  DS    F                         NET                                    
NCOSTDL  EQU   *-NCOSTD                                                         
*                                                                               
PEPTABD  DSECT                     DSECT FOR PRD/EST/PER TABLE (NETPAK)         
PEPTPRD  DS    XL1                                                              
PEPTEST  DS    XL1                                                              
PEPTPER  DS    XL2                                                              
PEPTKL   EQU   *-PEPTABD                                                        
PEPTGRS  DS    XL4                                                              
PEPTNET  DS    XL4                                                              
PEPTDL   EQU   *-PEPTABD                                                        
*                                                                               
PEPTMAX  EQU   5000                                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
ENET     DS      12PL6             SO NETPAK CAN REPORT NET                     
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
NETBLKD  DSECT                                                                  
       ++INCLUDE NETBLOCKD                                                      
*                                                                               
       ++INCLUDE NEGENUNIT                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'109SPREPXJ02 03/31/04'                                      
         END                                                                    
