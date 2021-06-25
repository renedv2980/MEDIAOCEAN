*          DATA SET PPINF14    AT LEVEL 030 AS OF 09/23/11                      
*PHASE T41A14A                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PPBVAL                                                                 
*INCLUDE PPFMTINO                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41A14  PRINTPAK INFO BILL RECS'                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*  BPLA  09/11    CHANGES FOR MIDAS BILLS                                       
*                                                                               
*  BPLA  09/08    SOON OR OVERNIGHT FILTERING                                   
*                                                                               
*  BPLA  08/07    DISPLAY Y/M FROM PBILIMO (IF THERE)                           
*                                                                               
*  BPLA  06/07    ADD POSTED FILTER                                             
*                                                                               
* KWAN 03/25/03 USE COMFACS' GETPROF INSTEAD OF INCLUDING IT                    
*                                                                               
*  BPLA 01/02     FIX INVNO EDIT-ALLOW FOR A,B,C AT START                       
*                                                                               
*  KWAN 10/00     DON'T DISP PENNY WHEN AMT IS MORE/LESS THAN 10 MILL           
*                                                                               
*  KWAN 07/00     BILL AMTS ARE NOW IN PL6 INSTEAD OF PL5                       
*                                                                               
*  SMYE 1/00      FIX FUNNY PBILLDAT FOR DISPLAY (IN BDOK)                      
*                                                                               
*  BPLA 9/99      ADD OPTION TO DISPLAY COS2 (OPEN) VALUES                      
*                                                                               
*  KWAN 5/99      CORRECT FILTER ERROR DISPLAY                                  
*                                                                               
*  KWAN 2/99      FIX FILTER FLD: INVNO=AANNNNNN                                
*                                                                               
*  BPLA 2/97      FIX BUG - INVALID BILLTYPE ERROR                              
*                                                                               
*  BPLA 4/96      IN MOS EDIT CHECK FOR C'/' AT 5(R4)                           
*                 (HAPPENS WHEN THEY ENTER MMMDD/YY BY MISTAKE)                 
*                 ALSO CHECK FOR C'-' AFTER MMM/YY OR MMMYY                     
*                 (HAPPENS WHEN THE TRY TO ENTER A RANGE BY MISTAKE)            
*                                                                               
*  SMYE 02/22/96  USE DATCON FOR FILTER DATES BINARY(YMD) FORMATTING            
*                                                                               
*  BPLA 12/21/94  NO LONGER OMIT AOR BILLS FROM TOTALS                          
*                 (REQUEST FROM DEBBIE FLORAY)                                  
*                                                                               
*  BPLA 6/8/94    INCLUDE PPFMTINO AND USE IT TO FORMAT INVOICES                
*              ** NOTE THAT INVOICE NUMBER FILTERING                            
*              ** DOES NOT WORK WITH THE NEW Y/M FORMATTING                     
*                                                                               
*  BPLA 4/27/94   INCLUDE GST AND PST IN ACTUAL COLUMN                          
*                                                                               
*  BPLA 10/29/92  CHANGE COM= TO UFC= + CHANGE DISPLAY                          
*                                                                               
*  BPLA 10/12/92  REDO SEP COM FILTERS/DISPLAY USING PPBVAL                     
*                                                                               
T41A14   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 44,T41A14,RR=R9                                                  
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         LA    R7,T41A14+4095                                                   
         LA    R7,1(R7)                                                         
         USING T41A14+4096,R7                                                   
         LR    R3,RC                                                            
         USING BILLWRKD,R3                                                      
*                       CLEAR BILL WORK AREA (NOT INFO WORK)                    
         LA    R5,BILLWRKX                                                      
         SR    R5,R3                                                            
         LR    R4,R3                                                            
         BAS   RE,CLEARWRK                                                      
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T41AFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         LA    R5,REC              SET RECORD ADDRESS                           
         ST    R5,AREC                                                          
*                                                                               
* MUST READ PB1X PROFILE FOR  INVOICE MONTH DISPLAY                             
*                                                                               
         MVC   PROGPRO(2),=C'**1'    CAN JUST SET DEFAULT                       
*                                    ME-MN                                      
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'PB1X'                                                 
         NI    WORK,X'BF'           LOWER CASE                                  
         MVC   WORK+4(2),SVAGY                                                  
         MVC   WORK+6(1),SVEBCMED                                               
         MVC   WORK+7(3),SVCLT                                                  
         CLI   SVCOFF,C' '                                                      
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCOFF                                                
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',WORK),PROGPROX,VDATAMGR                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'P0B9'                                                 
         MVC   WORK+4(2),SVAGY                                                  
         MVC   WORK+6(1),SVEBCMED                                               
         MVC   WORK+7(3),SVCLT                                                  
         CLI   SVCOFF,C' '                                                      
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCOFF                                                
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',WORK),PROFB9,VDATAMGR                           
*                                                                               
         MVI   FLTSW,0             INITIALIZE FILTER SWITCH                     
*                                                                               
* START VALIDATING FILTER FIELD                                                 
*                                                                               
         MVI   NOUSER,C'N'                                                      
*                                                                               
         CLI   PROFB9,0       IS THIS A RETAIL CLIENT?                          
         BE    *+8            IF SO DEFAULT TO Y                                
         MVI   NOUSER,C'Y'    SO I'LL DISPLAY ACCOUNT                           
*                                                                               
         GOTO1 GETFLTR,DUB,(64,SINIFLT),(6,=C'NOUSER')                          
         OC    4(4,R1),4(R1)                                                    
         BZ    *+12                                                             
         MVI   NOUSER,C'Y'         DON'T DISPLAY USER                           
         MVI   FLTSW,1             CONTAINS A VALID FIELD                       
*                                                                               
         GOTO1 GETFLTR,DUB,(64,SINIFLT),(4,=C'USER')                            
         OC    4(4,R1),4(R1)                                                    
         BZ    *+12                                                             
         MVI   NOUSER,C'N'         DISPLAY USER                                 
         MVI   FLTSW,1             CONTAINS A VALID FIELD                       
*                                                                               
*   NOTE - IF THEY HAPPEN TO ENTER BOTH, THE "USER" ONE WILL APPLY              
*                                                                               
         MVC   EST1(4),=X'0001FFFF'                                             
         GOTO1 GETFLTR,DUB,(64,SINIFLT),(4,=C'EST=')                            
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKMOS                                                           
         L     R4,4(R1)                                                         
         LA    R4,4(R4)                                                         
         CLC   0(2,R4),=C'NO'                                                   
         BNE   CE1                                                              
         B     CHKESTX              LEAVE AS X'0001FFFF'                        
*                                                                               
CE1      BAS   R9,GETNUM                                                        
         L     R6,4(R1)                                                         
         LA    RE,4                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(11),=C'EST=NNN-NNN'                                         
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         L     R4,4(R1)            CONVERT ESTIMATE 1                           
         LA    R4,4(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)         * EXECUTED *                                 
         CVB   RF,DUB                                                           
         STH   RF,EST1                                                          
         STH   RF,EST2                                                          
         LA    R4,1(R5,R4)                                                      
         CLI   0(R4),C'-'          ESTIMATE SERIES                              
         BNE   CHKESTX             NO - CHECK DATA FORMAT                       
         LA    R4,1(R4)                                                         
         LR    RF,R4                                                            
         BAS   R9,GETNUM                                                        
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)        * EXECUTED *                                  
         CVB   RF,DUB                                                           
         STH   RF,EST2                                                          
         CLC   EST1,EST2                                                        
         BH    FLTERR                                                           
CHKESTX  DS    0H                                                               
*                                                                               
         MVI   FLTSW,1             CONTAINS A VALID FIELD                       
*                                                                               
         EJECT                                                                  
CHKMOS   XC    MOS,MOS                                                          
         GOTO1 GETFLTR,DUB,(64,SINIFLT),(4,=C'MOS=')                            
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKCOS2                                                          
         L     R4,4(R1)                                                         
         LA    R4,4(R4)                                                         
         LR    R6,R4                                                            
         XC    WORK,WORK                                                        
         GOTO1 VDATVAL,DMCB,(2,(R4)),WORK2                                      
         MVC   WORK(10),=C'MOS=MMM/YY'                                          
         LA    RE,7                                                             
         CLI   DMCB+3,0                                                         
         BE    FLTERR                                                           
         CLI   5(R4),C'/'        IF THEY ENTER MMMDD/YY BY MISTAKE              
         BE    FLTERR                                                           
****                                                                            
**       CHECKS BELOW NEEDED SINCE RANGE NOT SUPPORTED YET                      
***                                                                             
         CLI   5(R4),C'-'        IF THEY ENTER MMMYY- BY MISTAKE                
         BE    FLTERR                                                           
         CLI   6(R4),C'-'        IF THEY ENTER MMM/YY- BY MISTAKE               
         BE    FLTERR                                                           
         GOTO1 VDATCON,DMCB,(0,WORK2),(3,WORK3)     WORK3=BINARY YMD            
         MVC   MOS(1),WORK3                                                     
         MVC   MOS+1(1),WORK3+1                                                 
*                                                                               
         MVI   FLTSW,1             CONTAINS A VALID FIELD                       
         EJECT                                                                  
CHKCOS2  DS    0H            CHECK FOR CARAT COS2 (CLIENT)                      
*                                                                               
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(5,=C'COS2=')                          
         OC    4(4,R1),4(R1)                                                    
         BZ    CKCS25                                                           
         L     R4,4(R1)                                                         
         LA    R4,5(R4)                                                         
         LR    R6,R4                                                            
         XC    WORK,WORK                                                        
         LA    RE,1                                                             
         MVC   WORK(8),=C'COS2=N,Y'                                             
*                                                                               
         CLI   C2SW,0                                                           
         BNE   FLTERR                                                           
*                                                                               
         MVI   C2SW,C'N'                                                        
         CLI   0(R4),C'N'                                                       
         BE    CKCS22                                                           
         MVI   C2SW,C'Y'           SET TO USE COST2                             
         CLI   0(R4),C'Y'                                                       
         BE    CKCS22                                                           
         B     FLTERR                                                           
*                                                                               
CKCS22   MVI   FLTSW,1       VALID FILTER ENTERED                               
*                                                                               
CKCS25   DS    0H                                                               
*                                                                               
         EJECT                                                                  
*                           TREAT OPEN= LIKE COST2                              
*                                                                               
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(5,=C'OPEN=')                          
         OC    4(4,R1),4(R1)                                                    
         BZ    CKCS29                                                           
         XC    WORK,WORK                                                        
         LA    RE,1                                                             
         MVC   WORK(8),=C'OPEN=N,Y'                                             
         L     R4,4(R1)                                                         
         LA    R4,5(R4)                                                         
         LR    R6,R4                                                            
*                                                                               
         CLI   C2SW,0           COST2 OR OPEN FILTER ALREADY GIVEN              
         BNE   FLTERR                                                           
*                                                                               
         MVI   C2SW,C'N'                                                        
         CLI   0(R4),C'N'                                                       
         BE    CKCS27                                                           
         MVI   C2SW,C'Y'           SET TO USE OPEN $                            
         CLI   0(R4),C'Y'                                                       
         BE    CKCS27                                                           
         B     FLTERR                                                           
*                                                                               
CKCS27   MVI   FLTSW,1       VALID FILTER ENTERED                               
*                                                                               
CKCS29   DS    0H                                                               
*                                                                               
         EJECT                                                                  
CHKBMO   XC    FBILMO,FBILMO                                                    
         GOTO1 GETFLTR,DUB,(64,SINIFLT),(7,=C'BILLMO=')                         
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKBNO                                                           
         XC    WORK,WORK                                                        
         LA    RE,7                                                             
         MVC   WORK(13),=C'BILLMO=MMM/YY'                                       
         L     R4,4(R1)                                                         
         LA    R4,7(R4)                                                         
         LR    R6,R4                                                            
         GOTO1 VDATVAL,DMCB,(2,(R4)),WORK2+8                                    
         LA    RE,7                                                             
         CLI   DMCB+3,0                                                         
         BE    FLTERR                                                           
         CLI   5(R4),C'/'                                                       
         BE    FLTERR                                                           
****                                                                            
**       CHECKS BELOW NEEDED SINCE RANGE NOT SUPPORTED YET                      
***                                                                             
         CLI   5(R4),C'-'        IF THEY ENTER MMMYY- BY MISTAKE                
         BE    FLTERR                                                           
         CLI   6(R4),C'-'        IF THEY ENTER MMM/YY- BY MISTAKE               
         BE    FLTERR                                                           
         GOTO1 VDATCON,DMCB,(0,WORK2+8),(3,WORK3)   WORK3=BINARY YMD            
         MVC   FBILMO(1),WORK3           GET DECADE                             
         MVC   FBILMO+1(1),WORK3+1                                              
*                                                                               
         MVI   FLTSW,1             CONTAINS A VALID FIELD                       
*                                                                               
* CHECK FOR BILL NUMBER FILTER                                                  
*                                                                               
CHKBNO   MVI   BNCNTRL,0                                                        
         MVI   BNCNTRL2,0                                                       
         XC    BNNOM,BNNOM                                                      
         XC    BNNOM2,BNNOM2                                                    
         MVI   BNCOUNT,0                                                        
         MVI   BNCOUNT2,0                                                       
*                                                                               
         GOTO1 GETFLTR,DUB,(64,SINIFLT),(6,=C'INVNO=')                          
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKBDATE                                                         
         L     R4,4(R1)                                                         
         LA    R4,6(R4)                                                         
         XC    WORK,WORK                                                        
         MVC   WORK(14),=C'INVNO=AANNNNNN'                                      
*                                                                               
         LA    RE,7                                                             
         LA    R2,BNCNTRL                                                       
         LA    R8,BNNOM                                                         
         MVI   BYTE,C'Y'                                                        
*                                                                               
CHKBNO5  DS    0H                                                               
         MVI   0(R2),X'70'         DEFAULT = EQ                                 
         CLC   0(2,R4),=C'LT'                                                   
         BNE   *+12                                                             
         MVI   0(R2),X'B0'    BYPASS GREATER THAN OR EQUAL                      
         LA    R4,2(R4)                                                         
         CLC   0(2,R4),=C'GT'                                                   
         BNE   *+12                                                             
         MVI   0(R2),X'D0'    BYPASS LESS THAN OR EQUAL                         
         LA    R4,2(R4)                                                         
         CLC   0(2,R4),=C'EQ'                                                   
         BNE   *+12                                                             
         MVI   0(R2),X'70'         BYPASS NOT EQUAL                             
         LA    R4,2(R4)                                                         
*                                                                               
         LR    R6,R4               SAVE START                                   
         SR    R5,R5                                                            
*                                                                               
CHKBNO6  CHI   R5,2                                                             
         BNL   CHKBNO8                                                          
         CLI   0(R6),C'A'                                                       
         BL    FLTERR                                                           
         CLI   0(R6),C'C'                                                       
         BNH   CHKBNO7                                                          
         CLI   0(R6),C'0'                                                       
         BL    FLTERR                                                           
         CLI   0(R6),C'9'                                                       
         BH    FLTERR                                                           
*                                                                               
CHKBNO7  LA    R6,1(R6)                                                         
         AHI   R5,1                                                             
         CLI   0(R6),0             END OF INPUT?                                
         BNE   CHKBNO7A                                                         
         CHI   R5,1                                                             
         BE    CHKBNO7B                                                         
         BCTR  R6,0                                                             
         BCTR  R6,0                                                             
         MVC   0(2,R8),0(R6)       INPUT IS 2 CHARS                             
         B     CHKBNO20                                                         
*                                                                               
CHKBNO7A CLI   0(R6),C'-'          CHK FOR RANGE OF INVNOS                      
         BE    CHKBNO7C                                                         
         CLI   0(R6),C','          CHK FOR RANGE OF INVNOS                      
         BE    CHKBNO7C                                                         
         CLI   0(R6),C' '          CHK FOR RANGE OF INVNOS                      
         BE    CHKBNO7C                                                         
         B     CHKBNO6                                                          
*                                                                               
CHKBNO7B MVI   0(R8),C'0'          INPUT IS 1 CHAR (MOVE IN A FAKE 0)           
         BCTR  R6,0                                                             
         MVC   1(1,R8),0(R6)                                                    
         B     CHKBNO20                                                         
*                                                                               
CHKBNO7C CHI   R5,1                                                             
         BNE   CHKBNO7D                                                         
         MVI   0(R8),C'0'          INPUT IS 1 CHAR (MOVE IN A FAKE 0)           
         BCTR  R6,0                                                             
         MVC   1(1,R8),0(R6)                                                    
         AHI   R6,1                                                             
         B     CHKBNO7X                                                         
CHKBNO7D BCTR  R6,0                INPUT IS 2 CHARS                             
         BCTR  R6,0                                                             
         MVC   0(2,R8),0(R6)                                                    
         AHI   R6,2                                                             
*                                                                               
CHKBNO7X LR    R4,R6                                                            
         B     CHKBNO10                                                         
*                                                                               
*                                                                               
*                                                                               
CHKBNO8  MVC   0(2,R8),0(R4)       FIRST 2 ALPHAS OR DIGITS                     
         AHI   R4,2                                                             
         LR    R6,R4               POSITION TO BE EX-ED                         
         BAS   R9,GETNUM                                                        
         LTR   R5,R5                                                            
         BZ    FLTERR              AFTER 2 ALPHAS, MUST BE DIGITS               
         CHI   R5,4                                                             
         BH    FLTERR              TOO MANY DIGITS                              
*                                                                               
         CLI   BYTE,C'Y'           SEE IF DOING 1ST PART                        
         BNE   CHKBNO8X                                                         
         LA    RF,2                                                             
         AR    RF,R5                                                            
         STC   RF,BNCOUNT          NUMBER OF DIGITS INPUT                       
         B     CHKBNO9                                                          
*                                                                               
CHKBNO8X DS    0H                                                               
         LA    RF,2                                                             
         AR    RF,R5                                                            
         STC   RF,BNCOUNT2         NUMBER OF DIGITS INPUT - 2ND PART            
*                                                                               
CHKBNO9  BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R6)         * EXECUTED *                                 
         CVB   RF,DUB                                                           
*                                                                               
         CHI   R5,3                                                             
         BNL   CHKBNO9H                                                         
         LA    RE,3                                                             
         SR    RE,R5                                                            
         MHI   RF,10                                                            
         BCT   RE,*-4                                                           
         LA    RE,7                RESTORE RE FOR ERROR MSG                     
*                                                                               
CHKBNO9H STCM  RF,3,2(R8)          4 DIGITS IN BINARY HALF WORD                 
*                                                                               
         CLI   0(R4),C'-'          CHK FOR RANGE OF INVNOS                      
         BE    CHKBNO10                                                         
         CLI   0(R4),C','          CHK FOR RANGE OF INVNOS                      
         BE    CHKBNO10                                                         
         CLI   0(R4),C' '          CHK FOR RANGE OF INVNOS                      
         BNE   CHKBNO20                                                         
CHKBNO10 CLI   BNCNTRL2,0          SEE IF SECOND ENTRY USED ALREADY             
         BNE   FLTERR              YES - ERROR                                  
         LA    R4,1(R4)                                                         
         LA    R2,BNCNTRL2                                                      
         LA    R8,BNNOM2                                                        
         MVI   BYTE,C'N'                                                        
         B     CHKBNO5                                                          
*                                                                               
*                                                                               
*                                                                               
CHKBNO20 DS    0H                                                               
         CLC   BNCNTRL,BNCNTRL2                                                 
         BNE   CHKBNO25                                                         
         CLI   BNCNTRL,X'70'                                                    
         BNE   FLTERR                                                           
*                                  2 EQUALS - MUST BE RANGE                     
*                                                                               
         MVI   BNCNTRL,X'40'       SET FIRST TO BYPASS LESS THAN                
         MVI   BNCNTRL2,X'20'      SET SECOND TO BYPASS GT                      
*                                  EQUALS WILL BE DISPLAYED                     
         B     CHKBNO30                                                         
*                                                                               
CHKBNO25 CLI   BNCNTRL2,0                                                       
         BE    CHKBNO30                                                         
         MVC   BYTE4(1),BNCNTRL                                                 
         OC    BYTE4(1),BNCNTRL2                                                
         CLI   BYTE4,X'F0'         CHECKS FOR LT + GT COMBINATION               
         BNE   FLTERR              NO - ERROR                                   
*                                                                               
CHKBNO30 B     CHKBNO50            SKIP THIS NONSENSE                           
*                                                                               
CHKBNO50 MVI   FLTSW,1             CONTAINS A VALID FIELD                       
*                                                                               
* CHECK BILL DATE                                                               
*                                                                               
CHKBDATE XC    FBILDATE,FBILDATE                                                
         MVI   FBILMO2,0                                                        
         XC    FBILDAT2,FBILDAT2                                                
         GOTO1 GETFLTR,DUB,(64,SINIFLT),(9,=C'BILLDATE=')                       
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKBTYP                                                          
         LA    RE,9                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(17),=C'BILLDATE=MMMDD/YY'                                   
         L     R4,4(R1)                                                         
         LA    R4,9(R4)                                                         
         LR    R6,R4                                                            
         GOTO1 VDATVAL,DMCB,(R4),FBILDATE                                       
         LA    RE,9                                                             
         CLI   DMCB+3,0                                                         
         BE    FLTERR                                                           
         MVC   SAVBYTE,DMCB+3      SAVE LENGTH FOR USE BELOW                    
*                                                                               
         GOTO1 VDATCON,DMCB,(0,FBILDATE),(3,WORK3)   WORK3=BINARY YMD           
         MVC   FBILMO(1),WORK3                                                  
         MVC   FBILMO+1(1),WORK3+1                                              
         SR    RE,RE                                                            
         IC    RE,SAVBYTE          LENGTH FROM DATVAL ABOVE                     
         LA    R4,0(RE,R4)                                                      
         CLI   0(R4),C'-'                                                       
         BNE   CHKBDTX                                                          
         LA    R4,1(R4)                                                         
         GOTO1 VDATVAL,DMCB,(R4),FBILDAT2                                       
         LA    RE,9                                                             
         CLI   DMCB+3,0                                                         
         BE    FLTERR                                                           
         GOTO1 VDATCON,DMCB,(0,FBILDAT2),(3,WORK3)   WORK3=BINARY YMD           
         MVC   FBILMO2(1),WORK3                                                 
         MVC   FBILMO2+1(1),WORK3+1                                             
CHKBDTX  DS    0H                                                               
*                                                                               
         MVI   FLTSW,1             CONTAINS A VALID FIELD                       
*                                                                               
         EJECT                                                                  
*                                                                               
* CHECK BILL TYPE FILTER                                                        
*                                                                               
CHKBTYP  XC    FBILTYP,FBILTYP                                                  
         XC    FBILNEG,FBILNEG     CLEAR NEGATIVE FILTER INDICATOR              
*                                  MEANS 'ALL BUT'                              
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(9,=C'BILLTYPE=')                      
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKCD                                                            
         LA    RE,9                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(14),=C' B4-7,M4-7,AOR'                                      
         LA    RF,TYPETAB                                                       
         L     R4,4(R1)                                                         
         LA    R4,9(R4)                                                         
         LR    R6,R4                                                            
         CLI   0(R4),C'-'       SEE IF 'ALL BUT'                                
         BNE   CHKBTYP0                                                         
         OI    FBILNEG,X'01'    SET ON NEGATIVE FILTER BYTE                     
         LA    R4,1(R4)         BUMP PAST -                                     
CHKBTYP0 OC    0(2,R4),=C'  '                                                   
CHKBTYP1 CLI   0(RF),0                                                          
         BE    FLTERR                                                           
         CLC   0(2,R4),2(RF)                                                    
         BE    *+12                                                             
         LA    RF,5(RF)                                                         
         B     CHKBTYP1                                                         
         MVC   FBILTYP,0(RF)                                                    
*                                                                               
         MVI   FLTSW,1             CONTAINS A VALID FIELD                       
*                                                                               
         EJECT                                                                  
CHKCD    MVI   CDSW,0              DEFAULT IS SUBTRACT CASH DISC                
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(3,=C'CD=')                            
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKDIST                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(9),=C'YES OR NO'                                            
         LA    RE,3                                                             
         L     R4,4(R1)                                                         
         LA    R4,3(R4)                                                         
         LR    R6,R4                                                            
         CLI   0(R4),C'N'                                                       
         BE    CHKCDX                                                           
         CLI   0(R4),C'Y'                                                       
         BNE   FLTERR                                                           
         MVI   CDSW,1              INCLUDE CD                                   
CHKCDX   DS    0H                                                               
*                                                                               
         MVI   FLTSW,1             CONTAINS A VALID FIELD                       
*                                                                               
CHKDIST  DS    0H                                                               
         XC    FDIST,FDIST                                                      
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(5,=C'DIST=')                          
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKCOM                                                           
         L     R4,4(R1)                                                         
         LA    R4,5(R4)                                                         
         SR    R6,R6                                                            
         CLI   0(R4),C'*'          BYPASS LEADING STARS                         
         BNE   *+16                                                             
         LA    R4,1(R4)                                                         
         LA    R6,1(R6)                                                         
         B     *-16                                                             
         STC   R6,FDISTDSP         LENGTH TO SKIP ON COMPARE                    
*                                                                               
         LR    R6,R4                                                            
         CLI   0(R4),C'A'          TERMINATE ON NON-ALPHA NUM                   
         BL    *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
         SR    R4,R6                                                            
         STC   R4,FDISTLEN         LENGTH OF INPUT                              
         MVI   FDIST,C' '                                                       
         MVC   FDIST+1(L'FDIST-1),FDIST                                         
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   FDIST(0),0(R6)                                                   
*                                                                               
         MVI   FLTSW,1             CONTAINS A VALID FIELD                       
*                                                                               
         EJECT                                                                  
CHKCOM   DS    0H                  OPTION FOR SEP COMMISSION BILLS              
         MVI   SCOMSW,C'B'         SET TO BOTH COM AND REG                      
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(3,=C'UFC')                            
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKNET                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(9),=C'YES OR NO'                                            
         LA    RE,4                                                             
         MVI   SCOMSW,C'N'                                                      
         L     R4,4(R1)                                                         
         CLI   4(R4),C'N'                                                       
         BE    CHKCOMX                                                          
         MVI   SCOMSW,C'Y'                                                      
         CLI   4(R4),C'Y'                                                       
         BE    CHKCOMX                                                          
         CLI   4(R4),X'00'                                                      
         BE    CHKCOMX                                                          
         CLI   4(R4),C','                                                       
         BE    CHKCOMX                                                          
         B     FLTERR                                                           
*                                                                               
CHKCOMX  DS    0H                                                               
*                                                                               
         MVI   FLTSW,1             CONTAINS A VALID FIELD                       
*                                                                               
         EJECT                                                                  
CHKNET   DS    0H                  OPTION FOR SEP COMMISSION BILLS              
         MVI   SNETSW,C'B'         SET TO BOTH COM AND REG                      
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(3,=C'NET')                            
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKPOST                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(9),=C'YES OR NO'                                            
         LA    RE,4                                                             
         MVI   SNETSW,C'N'                                                      
         L     R4,4(R1)                                                         
         CLI   4(R4),C'N'                                                       
         BE    CHKNETX                                                          
         MVI   SNETSW,C'Y'                                                      
         CLI   4(R4),C'Y'                                                       
         BE    CHKNETX                                                          
         CLI   4(R4),X'00'                                                      
         BE    CHKNETX                                                          
         CLI   4(R4),C','                                                       
         BE    CHKNETX                                                          
         B     FLTERR                                                           
*                                                                               
CHKNETX  DS    0H                                                               
*                                                                               
         MVI   FLTSW,1             CONTAINS A VALID FIELD                       
*                                                                               
         EJECT                                                                  
CHKPOST  DS    0H                  POSTED OPTION                                
         MVI   POSTSW,C'B'         SET TO BOTH POSTED AND UNPOSTED              
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(6,=C'POSTED')                         
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKPOSTX                                                         
         MVI   POSTSW,C'N'                                                      
         L     R4,4(R1)                                                         
         CLI   7(R4),C'N'                                                       
         BE    *+8                                                              
         MVI   POSTSW,C'Y'                                                      
*                                                                               
         MVI   FLTSW,1             CONTAINS A VALID FIELD                       
*                                                                               
*                                                                               
CHKPOSTX DS    0H                                                               
         EJECT                                                                  
CHKRUN   DS    0H                  CHECK FOR RUN TYPE FILTER                    
         MVI   FBILRUN,0                                                        
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(3,=C'RUN')                            
         OC    4(4,R1),4(R1)                                                    
         BZ    CHKRUNX                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(22),=C'SOON OR OV (OVERNIGHT)'                              
         LA    RE,4               NEEDED FOR ERROR PROCESSING                   
*                                 (LENGTH OF RUN=)                              
         MVI   FBILRUN,C'S'                                                     
         L     R4,4(R1)                                                         
         CLC   4(4,R4),=C'SOON'                                                 
         BE    CHKRUN5                                                          
         MVI   FBILRUN,C'O'       OVERNIGHT                                     
         CLC   4(2,R4),=C'OV'     JUST CHECK FIRST 2 CHARS                      
         BE    CHKRUN5                                                          
         MVI   FBILRUN,0           NO VALID FILTER ENTERED                      
         B     FLTERR                                                           
*                                                                               
CHKRUN5  MVI   FLTSW,1             CONTAINS A VALID FIELD                       
*                                                                               
*                                                                               
CHKRUNX  DS    0H                                                               
*                                                                               
CHKTOT   DS    0H                                                               
         MVI   TOTSW,0             DEFAULT IS NORMAL DISPLAY                    
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(6,=C'TOTAL=')                         
         OC    4(4,R1),4(R1)                                                    
         BZ    SETSCRN                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(9),=C'YES OR NO'                                            
         LA    RE,6                                                             
         L     R4,4(R1)                                                         
         LA    R4,6(R4)                                                         
         LR    R6,R4                                                            
         CLI   0(R4),C'N'                                                       
         BE    CHKTOTX                                                          
         CLI   0(R4),C'Y'                                                       
         BNE   FLTERR                                                           
         MVI   TOTSW,1             SHOW TOTAL LINE ONLY                         
CHKTOTX  DS    0H                                                               
*                                                                               
         MVI   FLTSW,1             CONTAINS A VALID FIELD                       
*                                                                               
         EJECT                                                                  
SETSCRN  DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
         CLI   SINIFLTH+5,0        ANY BAD INPUTS?                              
         BE    SETSCR20                                                         
         CLI   FLTSW,1                                                          
         BE    SETSCR20                                                         
         LA    R2,SINIFLTH         POINT TO FILTER FIELD                        
         LA    R3,2                FIELD INVALID ERR MSG                        
         B     ERROR                                                            
*                                                                               
*                                                                               
*                                                                               
SETSCR20 LA    R4,SINHDR                                                        
         USING BILSCRND,R4                                                      
         LA    R2,SINHDRH                                                       
         MVC   BSMOS(4),=C'MNTH'                                                
         MVC   BSBDATE+1(4),=C'BILL'                                            
         MVC   BSINV-1(7),=C'INVOICE'                                           
         MVC   BSBTYPE(4),=C'BILL'                                              
         MVC   BSBAMT+5(5),=C'GROSS'                                            
         MVC   BSNAMT+6(3),=C'NET'                                              
         CLI   CDSW,1                                                           
         BE    SETS5                                                            
         MVC   BSBAMT+2(8),=C'GROSS-CD'                                         
         MVC   BSNAMT+3(7),=C'NET/NET'                                          
*                                                                               
SETS5    DS    0H                                                               
         MVC   BSAAMT+4(6),=C'ACTUAL'                                           
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R4,LINLEN(R4)                                                    
         MVC   BSPRD,=C'PRD'                                                    
         MVC   BSEST,=C'EST'                                                    
         MVC   BSMOS,=C'SERV'                                                   
         MVC   BSBDATE+1(4),=C'DATE'                                            
         MVC   BSINV(6),=C'NUMBER'                                              
         MVC   BSBTYPE(4),=C'TYPE'                                              
         MVC   BSBAMT+4(6),=C'AMOUNT'                                           
         MVC   BSNAMT+4(6),=C'AMOUNT'                                           
         MVC   BSAAMT+4(6),=C'AMOUNT'                                           
*                                                                               
         CLI   NOUSER,C'Y'              SUPPRESSING USER ID?                    
         BE    SETS7                                                            
         MVC   BSRINO(7),=C'USER ID'                                            
*                                                                               
SETS7    FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R4,LINLEN(R4)                                                    
         MVC   BSPRD,DASH                                                       
         MVC   BSEST,DASH                                                       
         MVC   BSMOS,DASH                                                       
         MVC   BSBDATE,DASH                                                     
         MVC   BSINV(6),DASH                                                    
         MVC   BSBTYPE(4),DASH                                                  
         MVC   BSBAMT+4(6),DASH                                                 
         MVC   BSNAMT+4(6),DASH                                                 
         MVC   BSAAMT+4(6),DASH                                                 
*                                                                               
         CLI   NOUSER,C'Y'              SUPPRESSING USER ID?                    
         BE    SETS8                                                            
         MVC   BSRINO(7),DASH           UNDERLINE USER ID                       
*                                                                               
SETS8    FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
         LA    R4,LINLEN(R4)                                                    
         LA    R4,LINLEN(R4)                                                    
         EJECT                                                                  
         LA    R9,12               SO I'LL HAVE ROOM FOR TOTAL LINE             
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING BILLRECD,R5                                                      
         MVC   PBILKAGY,SVAGY                                                   
         MVC   PBILKMED,SVEBCMED                                                
         MVI   PBILKRCD,X'08'                                                   
         MVC   PBILKCLT,SVCLT                                                   
         CLC   SVPRD,=C'ALL'                                                    
         BE    *+10                                                             
         MVC   PBILKPRD,SVPRD                                                   
         OC    PREVKEY,PREVKEY                                                  
         BNZ   SETS10                                                           
         ZAP   TBILL,=P'0'                                                      
         ZAP   TBAMT,=P'0'                                                      
         ZAP   TNET,=P'0'                                                       
         ZAP   TACT,=P'0'                                                       
SETS10   OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY,PREVKEY                                                      
         XC    PREVKEY,PREVKEY                                                  
GBHIGH   BAS   RE,HIGH                                                          
         B     HAVREC                                                           
*                                                                               
GBSEQ    BAS   RE,SEQ                                                           
HAVREC   LA    R5,KEY                                                           
         USING BILLRECD,R5                                                      
         CLC   KEY(7),KEYSAVE                                                   
         BNE   BEND                                                             
         CLC   SVPRD,=C'ALL'    CHECK PRODUCT                                   
         BE    PRDOK                                                            
         CLC   SVPRD,=C'POL'                                                    
         BE    PRDOK                                                            
         CLC   PBILKPRD,SVPRD                                                   
         BNE   BEND                                                             
PRDOK    DS    0H                                                               
         CLC   EST1,=X'0000'       ONLY SHOW EST 0 IF LOOKING FOR IT            
         BE    PRDOK5                                                           
         CLC   PBILKEST,=X'0000'     ELSE BYPASS EST 0                          
         BE    GBSEQ                                                            
PRDOK5   CLC   PBILKEST,EST1                                                    
         BNL   CEEND                                                            
         MVC   PBILKEST,EST1                                                    
         XC    PBILKMOS(6),PBILKMOS                                             
         B     GBHIGH                                                           
CEEND    CLC   PBILKEST,EST2                                                    
         BNH   ESTOK                                                            
         MVC   PBILKEST(6),HIGHK                                                
         B     GBHIGH                                                           
ESTOK    CLI   MOS,0               CHECK MONTH OF SERVICE                       
         BE    MOSOK                                                            
         CLC   MOS,PBILKMOS                                                     
         BNE   GBSEQ                                                            
*                                                                               
*                                                                               
*                                                                               
MOSOK    CLI   BNCNTRL,0           CHECK BILL NUMBER                            
         BE    BNOOK                                                            
*                                                                               
         L     R5,AREC                                                          
         BAS   RE,GETREC           GET A RECORD                                 
*                                                                               
         CLC   PBILIMO,=X'0000'                                                 
         BE    MSOK3               NO ALPHA IN INVOICE MONTH                    
         CLC   PBILIMO,=C'  '                                                   
         BE    MSOK3               NO ALPHA IN INVOICE MONTH                    
*                                                                               
         MVC   FULL,PBILIMO                                                     
         LA    R5,KEY                                                           
         MVC   FULL+2(2),PBILKBNO                                               
*                                                                               
         B     MOSOK6M             GO COMPARE                                   
*                                                                               
MSOK3    LA    R5,KEY              POINT TO KEY                                 
*                                                                               
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),PBILKBMN+1                                             
***                                                                             
         SR    RF,RF                                                            
         ICM   RF,1,PROGPROX+4     INV MTH BASE YEAR                            
         BZ    MOSOK5                                                           
         ZIC   RE,PBILKBMN                                                      
         SR    RE,RF                                                            
         BNP   MOSOK5                                                           
         MHI   RE,12                                                            
         LH    RF,HALF                                                          
         AR    RE,RF                                                            
         STH   RE,HALF                                                          
***                                                                             
MOSOK5   DS    0H                                                               
         CLI   PROGPROX+5,0                                                     
         BE    MOSOK6                                                           
         ZIC   RE,PROGPROX+5                                                    
         LH    RF,HALF                                                          
         AR    RF,RE                                                            
         STH   RF,HALF                                                          
         CLI   PROGPROX+4,0                                                     
         BNE   MOSOK6                                                           
         CHI   RF,12                                                            
         BNH   MOSOK6                                                           
         AHI   RF,-12                                                           
         STH   RF,HALF                                                          
*                                                                               
MOSOK6   SR    RF,RF                                                            
         LH    RF,HALF                                                          
         CVD   RF,DUB                                                           
         UNPK  FULL(3),DUB+6(2)                                                 
         MVC   HALF,FULL+1         THERE ARE ONLY 2 DIGITS                      
         OI    HALF+1,X'F0'                                                     
*                                                                               
         MVC   FULL(2),HALF                                                     
         MVC   FULL+2(2),PBILKBNO                                               
*                                                                               
MOSOK6M  B     MOSOK7M                                                          
**                                                                              
**       SKIP THE NONSENSE                                                      
**                                                                              
MOSOK7M  DS    0H                                                               
         CLI   BNCOUNT,0          SEE IF ONLY 2 CHAR FILTER                     
         BNE   MOSOK7Q                                                          
         CLC   FULL(2),BNNOM      ONLY CHECK MONTH PART                         
         B     MOSOK7R                                                          
*                                                                               
MOSOK7Q  CLC   FULL,BNNOM                                                       
MOSOK7R  IC    R8,BNCNTRL                                                       
         STC   R8,*+5              SET COMPARE CONDITION                        
         BC    0,GBSEQ                                                          
         CLI   BNCNTRL2,0          CHK FOR RANGES                               
         BE    BNOOK                                                            
         CLI   BNCOUNT2,0         SEE IF ONLY 2 CHAR FILTER                     
         BNE   MOSOK7S                                                          
         CLC   FULL(2),BNNOM2     ONLY CHECK MONTH PART                         
         B     MOSOK7T                                                          
*                                                                               
MOSOK7S  CLC   FULL,BNNOM2                                                      
MOSOK7T  IC    R8,BNCNTRL2         SET COMPARE CONDITION CODE                   
         STC   R8,*+5                                                           
         BC    0,GBSEQ                                                          
*                                                                               
*                                                                               
*                                                                               
BNOOK    CLI   FBILMO,0                                                         
         BE    BMOOK                                                            
         CLI   FBILMO2,0                                                        
         BE    BMOEQ                                                            
         CLC   PBILKBMN,FBILMO                                                  
         BL    GBSEQ                                                            
         CLC   PBILKBMN,FBILMO2                                                 
         BH    GBSEQ                                                            
         B     BMOOK                                                            
BMOEQ    CLC   PBILKBMN,FBILMO                                                  
         BNE   GBSEQ                                                            
BMOOK    DS    0H                                                               
         MVC   FBDSK,KEY+27                                                     
         L     R5,AREC                                                          
         BAS   RE,GETREC              GET A RECORD                              
         CLI   FBILTYP,0           CHECK BILL TYPE                              
         BE    BTYPOK                                                           
**AOR                                                                           
BMOOK1   CLI   FBILTYP,X'FE'       SPECIAL FOR AOR                              
         BNE   BMOOK1M                                                          
         TM    FBILNEG,X'01'                                                    
         BNO   BMOOK1C                                                          
         TM    PBILCMSW,BSTTAORQ                                                
         BO    GBSEQ              SKIP AOR                                      
         B     BTYPOK                                                           
*                                                                               
BMOOK1C  TM    PBILCMSW,BSTTAORQ                                                
         BZ    GBSEQ                                                            
         B     BTYPOK                                                           
**AOR                                                                           
**MIDAS                                                                         
BMOOK1M  CLI   FBILTYP,X'FB'       MIDAS FILTER                                 
         BNE   BMOOK2                                                           
         TM    FBILNEG,X'01'                                                    
         BNO   BMOOK1P                                                          
         TM    PBILSTAT,X'20'                                                   
         BO    GBSEQ              SKIP MIDAS                                    
         B     BTYPOK                                                           
*                                                                               
BMOOK1P  TM    PBILSTAT,X'20'                                                   
         BZ    GBSEQ                                                            
         B     BTYPOK                                                           
**MIDAS                                                                         
BMOOK2   CLI   FBILTYP,X'FF'       SPECIAL FOR RETAIL                           
         BNE   BMOOK3                                                           
         TM    FBILNEG,X'01'       SEE IF 'ALL BUT'                             
         BNO   BMOOK2C                                                          
         CLC   FBILTYP+1(1),PBRETAIL                                            
         BE    GBSEQ                  SKIP                                      
         B     BTYPOK                                                           
*                                                                               
BMOOK2C  CLC   FBILTYP+1(1),PBRETAIL                                            
         BNE   GBSEQ                                                            
         B     BTYPOK                                                           
*                                                                               
BMOOK3   CLI   FBILTYP,C'0'        CHK FOR OLD BILL TYPES                       
         BH    BMOOK5                                                           
         TM    FBILNEG,X'01'       SEE IF DOING 'ALL BUT'                       
         BNO   BMOOK3C                                                          
         CLC   FBILTYP,PBILLTYP                                                 
         BE    GBSEQ                                                            
         B     BTYPOK                                                           
*                                                                               
BMOOK3C  CLC   FBILTYP,PBILLTYP                                                 
         BNE   GBSEQ                                                            
         B     BTYPOK                                                           
*                                                                               
BMOOK5   TM    FBILNEG,X'01'        SEE IF DOING 'ALL BUT'                      
         BNO   BMOOK5C                                                          
         CLC   FBILTYP(1),PBILLTYP OLD - ONLY CHK 1 BYTE                        
         BE    GBSEQ                                                            
         B     BTYPOK                                                           
*                                                                               
BMOOK5C  CLC   FBILTYP(1),PBILLTYP OLD - ONLY CHK 1 BYTE                        
         BNE   GBSEQ                                                            
*                                                                               
BTYPOK   DS    0H                                                               
*                                                                               
CKRUN    CLI   FBILRUN,0          RUN TYPE ENTERED?                             
         BE    RUNTOK                                                           
         CLI   FBILRUN,C'S'       SOON BILLS ONLY?                              
         BNE   CKRUN5                                                           
         TM    PBILSTAT,PSTSOONQ                                                
         BO    RUNTOK                                                           
         B     GBSEQ                                                            
*                                                                               
CKRUN5   CLI   FBILRUN,C'O'       OVERNIGHT ONLY?                               
         BNE   RUNTOK                                                           
         TM    PBILSTAT,PSTSOONQ  SKIP SOON BILLS                               
         BO    GBSEQ                                                            
*                                                                               
RUNTOK   DS    0H                                                               
*                                                                               
CKCOM    DS    0H                                                               
         CLI   SCOMSW,C'N'        SEP COMMISSION FILTER                         
         BNE   CKCOM2                                                           
         TM    PBILCMSW,BSTSCOMQ                                                
         BZ    CKCOMOK                                                          
         B     GBSEQ                                                            
CKCOM2   DS    0H                                                               
         CLI   SCOMSW,C'Y'                                                      
         BNE   CKCOMOK                                                          
         TM    PBILCMSW,BSTSCOMQ                                                
         BZ    GBSEQ                                                            
         TM    PBILCMSW,BSTTAORQ   SKIP TRUE AOR BILLS EVEN                     
         BZ    CKCOMOK             SEPARATE COMMISSION                          
         B     GBSEQ                                                            
CKCOMOK  DS    0H                                                               
         EJECT                                                                  
CKNET    DS    0H                                                               
         CLI   SNETSW,C'N'        SEP COMMISSION NET FILTER                     
         BNE   CKNET2                                                           
         TM    PBILCMSW,BSTSNETQ                                                
         BZ    CKNETOK                                                          
         B     GBSEQ                                                            
CKNET2   DS    0H                                                               
         CLI   SNETSW,C'Y'                                                      
         BNE   CKNETOK                                                          
         TM    PBILCMSW,BSTSNETQ                                                
         BZ    GBSEQ                                                            
         TM    PBILCMSW,BSTTAORQ   SKIP TRUE AOR BILLS EVEN                     
         BZ    CKNETOK             SEPARATE COMMISSION                          
         B     GBSEQ                                                            
CKNETOK  DS    0H                                                               
         EJECT                                                                  
CKBDATE  CLI   FBILDATE,0          CHECK BILL DATE                              
         BE    BDDIST                                                           
         CLI   FBILDAT2,0                                                       
         BNE   BDRANGE                                                          
         CLC   FBILDATE,PBILLDAT                                                
         BNE   GBSEQ                                                            
         B     BDDIST                                                           
BDRANGE  CLC   PBILLDAT,FBILDATE                                                
         BL    GBSEQ                                                            
         CLC   PBILLDAT,FBILDAT2                                                
         BH    GBSEQ                                                            
         EJECT                                                                  
*                                                                               
BDDIST   DS    0H                                                               
         CLI   FDIST,0             RETAIL DISTRIBUTOR                           
         BE    BDISTOK                                                          
         ZIC   RE,FDISTDSP                                                      
         LA    RE,PBRACCT(RE)     START OF COMPARE                              
         ZIC   RF,FDISTLEN         LENGTH TO CHECK                              
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FDIST(0),0(RE)                                                   
         BNE   GBSEQ                                                            
         EJECT                                                                  
*                                                                               
BDISTOK  DS    0H                                                               
         CLI   POSTSW,C'N'         POSTED FILTER                                
         BNE   BPST2                                                            
         OC    PBILPOST,PBILPOST                                                
         BZ    BPSTOK                                                           
         B     GBSEQ                                                            
BPST2    DS    0H                                                               
         CLI   POSTSW,C'Y'                                                      
         BNE   BPSTOK                                                           
         OC    PBILPOST,PBILPOST                                                
         BNZ   BPSTOK                                                           
         B     GBSEQ                                                            
*                                                                               
BPSTOK   DS    0H                                                               
* END OF FILTERS SO DISPLAY RECORD                                              
BDOK     MVC   BSPRD,PBILKPRD                                                   
         MVC   HALF,PBILKEST                                                    
         LH    RF,HALF                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BSEST,DUB+6(2)                                                   
         ZIC   R0,PBILKMOS                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BSMOS(2),DUB+6(2)                                                
         ZIC   R0,PBILKMOS+1       MTH                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BSMOS+2(2),DUB+6(2)                                              
*NOP*    MVC   BSBDATE,PBILLDAT        PBILLDAT IS "FUNNY DATE" SO FIX          
         GOTO1 VDATCON,DMCB,(0,PBILLDAT),(X'20',BSBDATE)   FOR DISPLAY          
***                                                                             
         GOTO1 =V(PPFMTINO),DMCB,PBILLDAT,(2,PBILKBNO),                X        
               (PBILKMED,PROGPRO),PROGPROX,RR=RELO                              
*                                                                               
         L     RF,DMCB+8           ADDRESS OF YEAR/MTH                          
         MVC   BSINV(2),0(RF)                                                   
         L     RF,DMCB+4           ADDRESS OF SHORT FORMAT                      
         MVC   BSINV+2(4),3(RF)    JUST MOVE NUMBER PART                        
*                                                                               
         CLI   PBILIMO,X'40'       WAS INV Y/M SAVED HERE?                      
         BNH   *+10                                                             
         MVC   BSINV(2),PBILIMO    IF SO, USE IT HERE                           
*                                                                               
         LA    RF,TYPETAB                                                       
BDOK5    CLI   0(RF),0                                                          
         BE    BDOK10              NOT FOUND                                    
         CLI   PBILLTYP,C'0'       SEE IF OLD TYPE                              
         BH    BDOK8                                                            
         CLC   0(2,RF),PBILLTYP    FOR NEW MATCH 2 CHARS                        
         BE    BDOK10                                                           
         B     BDOK9                                                            
*                                                                               
BDOK8    CLC   0(1,RF),PBILLTYP                                                 
         BE    BDOK10                                                           
BDOK9    LA    RF,5(RF)                                                         
         B     BDOK5                                                            
*                                                                               
BDOK10   MVC   BSBTYPE,2(RF)                                                    
**AOR                                                                           
         ZAP   WORK2+00(6),=P'0'                                                
         ZAP   WORK2+06(6),=P'0'                                                
         ZAP   WORK2+12(6),PBILLRCV                                             
*                                                                               
BDOK15   DS    0H                                                               
         GOTO1 =V(PPBVAL),DMCB,(C'B',PBILLREC),PPBVALD,RR=RELO                  
*                                                                               
         MVC   PBILLGRS,PPBVEBG  SET "EFFECTIVE" VALUES INTO PBILLREC           
         MVC   PBILLBIL,PPBVEBB                                                 
         MVC   PBILLNET,PPBVEBN                                                 
*                                                                               
         CLI   C2SW,C'Y'      DOING COST 2?                                     
         BNE   BDOK15X                                                          
***                                                                             
***      SET COST2 (OPEN) VALUES INTO PBILLREC                                  
***      LEAVE ACTUAL ALONE - ALWAYS WHAT APPEARED ON BILL                      
***                                                                             
         CLI   PBILOTH,X'09'         SEE IF OTHERS ELEMENT THERE                
         BNE   BDOK15X               NO - JUST LEAVE VALUES ALONE               
***                                  (IF PRESENT IT WILL ALWAYS BE              
***                                   NEXT ELEMENT)                             
         TM    PBILLIND,X'83'        CHECK IF FINANCIAL (ANY TYPE)              
         BZ    BDOK15X                                                          
         ZAP   PBILLGRS,PBILLOPG     OPEN GROSS                                 
         ZAP   PBILLBIL,PBILLOPG     OPEN GROSS                                 
         SP    PBILLBIL,PPBVEBC      SUBTRACT CD                                
         ZAP   PBILLNET,PBILLOPN     OPEN NET                                   
         SP    PBILLNET,PPBVEBC      SUBTRACT CD                                
***                                                                             
*                                                                               
BDOK15X  DS    0H                                                               
         L     R0,PPBVGST            ADD GST AND PST TO ACTUAL                  
         CVD   R0,DUB                                                           
         AP    WORK2+12(6),DUB                                                  
         L     R0,PPBVPST                                                       
         CVD   R0,DUB                                                           
         AP    WORK2+12(6),DUB                                                  
*                             NOTE - USE PPBVEBC FOR "EFFECTIVE" CD             
*                                                                               
         TM    PBILCMSW,BSTTAORQ       SEE IF AOR BILL                          
         BO    BDOK20                                                           
**AOR                                                                           
*                                                                               
         MVC   WORK2+00(6),PBILLBIL  G-CD                                       
         TM    PBILCMSW,BSTTAORQ     FOR AOR BILLS SKIP NET BECAUSE             
         BNZ   *+10                  FIELD IS USED FOR ORIG BILL AGYCOM         
         MVC   WORK2+06(6),PBILLNET                                             
         CLI   CDSW,0                                                           
         BE    BDOK20                                                           
         MVC   WORK2+00(6),PBILLGRS  GROSS                                      
         AP    PBILLNET,PPBVEBC      ADD CD                                     
         MVC   WORK2+06(6),PBILLNET                                             
*                                                                               
BDOK20   DS    0H                                                               
         CP    WORK2+00(6),=P'999999999'                                        
         BH    BDOK23H                                                          
         CP    WORK2+00(6),=P'-999999999'                                       
         BL    BDOK23H                                                          
         EDIT  (P6,WORK2+00),(11,BSBAMT),2,MINUS=YES                            
         B     BDOK23J                                                          
BDOK23H  ZAP   DUB,WORK2+00(6)                                                  
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'50'     REMAINDER IS GREAT THAN 50?                  
         BL    *+10                                                             
         AP    DUB(6),=P'1'        ROUND UP BY 1                                
         EDIT  (P6,DUB),(11,BSBAMT),0,MINUS=YES                                 
BDOK23J  CP    WORK2+06(6),=P'999999999'                                        
         BH    BDOK23M                                                          
         CP    WORK2+06(6),=P'-999999999'                                       
         BL    BDOK23M                                                          
         EDIT  (P6,WORK2+06),(11,BSNAMT),2,MINUS=YES                            
         B     BDOK23P                                                          
BDOK23M  ZAP   DUB,WORK2+06(6)                                                  
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'50'     REMAINDER IS GREAT THAN 50?                  
         BL    *+10                                                             
         AP    DUB(6),=P'1'        ROUND UP BY 1                                
         EDIT  (P6,DUB),(11,BSNAMT),0,MINUS=YES                                 
BDOK23P  CP    WORK2+12(6),=P'999999999'                                        
         BH    BDOK23U                                                          
         CP    WORK2+12(6),=P'-999999999'                                       
         BL    BDOK23U                                                          
         EDIT  (P6,WORK2+12),(11,BSAAMT),2,MINUS=YES                            
         B     BDOK23X                                                          
BDOK23U  ZAP   DUB,WORK2+12(6)                                                  
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'50'     REMAINDER IS GREAT THAN 50?                  
         BL    *+10                                                             
         AP    DUB(6),=P'1'        ROUND UP BY 1                                
         EDIT  (P6,DUB),(11,BSAAMT),0,MINUS=YES                                 
*                                                                               
BDOK23X  DS    0H                                                               
*                                                                               
**AOR                                                                           
         CLI   FBILTYP,X'FE'      SEE IF REQUESTING ONLY AOR BILLS              
         BE    BDOK25             YES THEN SHOW IN TOTALS                       
****     TM    PBILCMSW,BSTTAORQ  SEE IF AOR BILL                               
****     BO    BDOK30             OMIT FROM TOTALS                              
**AOR                                                                           
         CLI   PBRETAIL,X'81'     SKIP CORP CONTROL                             
         BNE   BDOK25                                                           
         CLI   FBILTYP+1,X'81'    UNLESS LISTING ONLY CONTROL BILLS             
         BNE   BDOK30                                                           
*                                                                               
BDOK25   AP    TBAMT,WORK2+00(6)                                                
         AP    TNET,WORK2+06(6)                                                 
         AP    TACT,WORK2+12(6)                                                 
         AP    TBILL,=P'1'            ADD TO BILLS                              
*                                                                               
BDOK30   CLI   PBRETAIL,0                                                       
         BE    BDOK30B                                                          
         MVI   BSBTYPE+2,C'S'                                                   
         CLI   PBRETAIL,X'41'          SUMMARY BILLL                            
         BE    BDOK31                                                           
         MVI   BSBTYPE+2,C'C'                                                   
         CLI   PBRETAIL,X'81'         CORP CONTROL BILL                         
         BE    BDOK31                                                           
         MVI   BSBTYPE+2,C' '                                                   
         B     BDOK31                                                           
*                                                                               
BDOK30B  DS    0H                                                               
         TM    PBILSTAT,X'20'         CHECK FOR MIDAS BILL                      
         BZ    BDOK31                                                           
         MVI   BSBTYPE+2,C'T'                                                   
*                                                                               
BDOK31   DS    0H    THIS IS WHERE I USED TO LIMIT TO SOON BILLS ONLY           
         CLI   NOUSER,C'Y'            SUPPRESSING USER?                         
         BE    BDOK32                                                           
         OC    PBILLUID,PBILLUID      SEE IF I HAVE A USERID                    
         BZ    BDOK32                                                           
*                          MUST READ CONTROL FILE TO DISPLAY USERID             
         LA    RE,REC2     READ INTO REC2                                       
         LA    RF,400*5                                                         
         XCEF                                                                   
*                                                                               
         LA    R6,REC2                                                          
         USING CTIREC,R6                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,PBILLUID    ID NUMBER                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R6),(R6)                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA                                                       
BDOK31A  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
*                                                                               
         DROP  R6                                                               
*                                                                               
         CLI   0(RE),X'02'         USERID ALPHA ELEMENT (X'02')                 
         BE    BDOK31C                                                          
         ZIC   R1,1(RE)            NEXT ELEMENT                                 
         AR    RE,R1                                                            
         B     BDOK31A                                                          
*                                                                               
BDOK31C  MVC   BSRINO(10),2(RE)                                                 
         B     BDOK34                                                           
*                                                                               
BDOK32   CLI   PBRETAIL,0          SEE IF RETAIL                                
         BE    BDOK34                                                           
         MVC   BSRINO,PBRACCT          DISPLAY RETAIL ACCOUNT                   
         B     SENDB                                                            
*                                                                               
**AOR                                                                           
BDOK34   TM    PBILCMSW,X'20'          AOR BILL                                 
         BZ    BDOK36                                                           
         MVC   BSBTYPE(3),=C'AOR'                                               
         MVC   BSBTYPE+3(1),PBILLTYP+1                                          
         B     BDOK38                                                           
*                                                                               
BDOK36   TM    PBILCMSW,BSTSCOMQ       SEP COM BILL                             
         BZ    BDOK37                                                           
         MVC   BSBTYPE(3),=C'UFC'                                               
         MVC   BSBTYPE+3(1),PBILLTYP+1                                          
         B     BDOK38                                                           
*                                                                               
BDOK37   TM    PBILCMSW,BSTSNETQ       SEP NET BILL                             
         BZ    BDOK38                                                           
         MVC   BSBTYPE(3),=C'NET'                                               
         MVC   BSBTYPE+3(1),PBILLTYP+1                                          
         B     BDOK38                                                           
*                                                                               
BDOK38   CLC   PBILLCAN,=C'000000'                                              
         BE    SENDB                                                            
         CLI   PBILLTYP,C'0'       SEE IF NEW BILL                              
         BL    SENDB               YES                                          
         MVC   BSRINO(6),PBILLCAN                                               
         B     SENDB                                                            
*                                                                               
* CAN BE PATCHED TO DISPLAY DISK ADDRS                                          
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,FBDSK,BSAAMT,4,0,8,RR=RELO                       
SENDB    CLI   TOTSW,0                                                          
         BE    SENDB5                                                           
         XC    0(80,R4),0(R4)      SHOWING TOTAL LINE ONLY                      
         B     GBSEQ               CONTINUE READING BILLS                       
SENDB5   FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R4,LINLEN(R4)                                                    
         BCT   R9,GBSEQ                                                         
         BAS   RE,SEQ                                                           
         CLC   KEY(7),KEYSAVE                                                   
         BNE   BEND                END OF CLIENT - WILL HAVE ROOM               
*                                  FOR TOTALS                                   
         MVC   PREVKEY,KEY                                                      
         B     MODEXIT                                                          
*                                                                               
BEND     CP    TBILL,=P'0'           SEE IF BILLS FOUND                         
         BNE   BEND5                                                            
         B     MODEXIT                                                          
*                                                                               
BEND5    MVC   BSMOS(6),=C'*TOTAL'                                              
         LA    R5,BSBAMT-1                                                      
         LA    RF,TBAMT                                                         
         BAS   RE,DSPNUMB                                                       
         LA    R5,BSNAMT-1                                                      
         LA    RF,TNET                                                          
         BAS   RE,DSPNUMB                                                       
         LA    R5,BSAAMT-1                                                      
         LA    RF,TACT                                                          
         BAS   RE,DSPNUMB                                                       
         FOUT  (R2)                                                             
*                                                                               
         ZAP   TBAMT,=P'0'                                                      
         ZAP   TNET,=P'0'                                                       
         ZAP   TACT,=P'0'                                                       
         ZAP   TBILL,=P'0'                                                      
         XC    PREVKEY,PREVKEY                                                  
         B     MODEXIT                                                          
*                                                                               
*                                                                               
*                                                                               
DSPNUMB  DS    0H                                                               
         CP    0(8,RF),=P'999999999'                                            
         BH    DSPN50                                                           
         CP    0(8,RF),=P'-999999999'                                           
         BL    DSPN50                                                           
         EDIT  (P8,0(RF)),(12,0(R5)),2,MINUS=YES                                
         BR    RE                                                               
DSPN50   ZAP   DUB,0(8,RF)                                                      
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'50'     REMAINDER IS GREAT THAN 50?                  
         BL    *+10                                                             
         AP    DUB(6),=P'1'        ROUND UP BY 1                                
         EDIT  (P6,DUB),(12,0(R5)),0,MINUS=YES                                  
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
FLTERR   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SINMSG(0),0(R6)                                                  
         LA    RE,3(RE)                                                         
         LA    RF,SINMSG(RE)                                                    
         MVC   0(20,RF),=C'INVALID FILTER FIELD'                                
         LA    RF,21(RF)                                                        
         MVC   0(22,RF),WORK                                                    
         LA    R2,SINIFLTH                                                      
         MVI   ERRCD,X'FF'                                                      
         MVI   ERRAREA,X'FF'                                                    
         B     MODEXIT2                                                         
*                                                                               
GETNUM   LA    R5,0                                                             
GN1      CLI   0(R4),C'-'                                                       
         BER   R9                                                               
         CLI   0(R4),C','                                                       
         BER   R9                                                               
         CLI   0(R4),X'00'                                                      
         BER   R9                                                               
         CLI   0(R4),C' '                                                       
         BER   R9                                                               
         CLI   0(R4),C'0'                                                       
         BL    GNERR                                                            
         CLI   0(R4),C'9'                                                       
         BH    GNERR                                                            
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     GN1                                                              
GNERR    SR    R5,R5                                                            
         BR    R9                                                               
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NEXTEL2                                                          
         CLC   0(1,R6),ELCODE                                                   
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
MODEXIT  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
MODEXIT2 OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
       ++INCLUDE PPGENEROL                                                      
TYPETAB  DC    C'1 ',C'MAN'                                                     
         DC    C'3 ',C'ORG'                                                     
         DC    C'4 ',C'DET'                                                     
         DC    C'B4',C'B4 '                                                     
         DC    C'B5',C'B5 '                                                     
         DC    C'B6',C'B6 '                                                     
         DC    C'B7',C'B7 '                                                     
         DC    C'M4',C'M4 '                                                     
         DC    C'M5',C'M5 '                                                     
         DC    C'M6',C'M6 '                                                     
         DC    C'M7',C'M7 '                                                     
         DC    C'R4',C'R4 '        FINANCIAL REBATE BILLS                       
         DC    C'R5',C'R5 '                                                     
         DC    C'R6',C'R6 '                                                     
         DC    C'R7',C'R7 '                                                     
         DC    X'FB20',C'T  '      GRP M MIDAS                                  
         DC    X'FF41',C'S  '      RETAIL SUMMARY BILLS                         
         DC    X'FF81',C'C  '      RETAIL CONTROL BILLS                         
         DC    X'FE00',C'AOR'      AOR BILLS                                    
         DC    X'0000',C'UNK'                                                   
DASH     DC    40C'-'                                                           
HIGHK    DC    10X'FF'                                                          
LINLEN   EQU   88                                                               
         LTORG                                                                  
         EJECT                                                                  
BILLWRKD DSECT                                                                  
C2SW     DS    CL1                 'Y' IF DISPLAYING COST2 $                    
EST1     DS    H                   ESTIMATE FILTER 1                            
EST2     DS    H                   ESTIMATE FILTER 2                            
MOS      DS    CL2                 MONTH OF SERVICE FILTER                      
FBILMO   DS    CL2                 BILLING MONTH FILTER                         
FBILMO2  DS    CL2                                                              
BNCNTRL  DS    CL1                 COMPARE CONTROL                              
BNNOM    DS    F                   BILL INV NO (XL2) + INV NO (XL2)             
BNCNTRL2 DS    CL1                 SECOND COMPARE CONTROL                       
BNNOM2   DS    F                   BILL INV NO (XL2) + INV NO (XL2)             
BNCOUNT  DS    XL1                 CHARS INPUT IN INVNO FILTER                  
BNCOUNT2 DS    XL1                 CHARS FOR SECOND INVNO PART                  
FBILNEG  DS    CL1                 X'01' IF NEGATIVE BILL TYPE FILTER           
FBILTYP  DS    CL2                 BILL TYPE FILTER                             
SCOMSW   DS    CL1                 SEP COM FILTER                               
SNETSW   DS    CL1                 SEP COM NET FILTER                           
CDSW     DS    CL1                                                              
FBILRUN  DS    CL1                 RUN TYPE FILTER                              
NOUSER   DS    CL1                 Y= DON'T DISPLAY USER ID                     
TOTSW    DS    CL1                 SET TO 1 IF DOING ONLY TOTAL LINE            
POSTSW   DS    CL1                 B=ALL,Y= POSTED ONLY,N=UNPOSTED ONLY         
FBILDATE DS    CL6                 BILL DATE FILTER                             
FBILDAT2 DS    CL6                                                              
PROGPRO  DS    CL16                PB1 PROFILE                                  
PROGPROX DS    CL16                PB1X PROFILE FOR INV MTH FORMATTING          
PROFB9   DS    CL16                PB9  TO SEE IF A RETAIL                      
FBDSK    DS    CL4                                                              
WORK2    DS    CL40                                                             
WORK3    DS    XL3                                                              
SAVBYTE  DS    XL1                                                              
FDIST    DS    CL12                                                             
FDISTLEN DS    X                                                                
FDISTDSP DS    X                                                                
ELCODE   DS    CL1                                                              
*                                                                               
FLTSW    DS    XL1                 VALID FILTER FIELD HAS BEEN ENTERED          
*                                                                               
       ++INCLUDE PPBVALD           NEW DSECT FOR PPBVAL ROUTINE                 
*                                                                               
BILLWRKX EQU   *                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*                                                                               
BILSCRND DSECT                                                                  
BSPRD    DS    CL3                                                              
         DS    CL1                                                              
BSEST    DS    CL3                                                              
         DS    CL1                                                              
BSMOS    DS    CL4                                                              
         DS    CL1                                                              
BSBDATE  DS    CL6                                                              
         DS    CL1                                                              
BSINV    DS    CL6                                                              
         DS    CL1                                                              
BSBTYPE  DS    CL3                                                              
         DS    CL1                                                              
BSBAMT   DS    CL11                                                             
         DS    CL1                                                              
BSNAMT   DS    CL11                                                             
         DS    CL1                                                              
BSAAMT   DS    CL11                                                             
         DS    CL1                                                              
BSRINO   DS    CL12                                                             
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE PBILLREC                                                       
*                                                                               
BSTSCOMQ EQU   X'01'                                                            
BSTTAORQ EQU   X'20'                                                            
BSTSNETQ EQU   X'08'                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
       ++INCLUDE PPSINFOWRK                                                     
*                                                                               
