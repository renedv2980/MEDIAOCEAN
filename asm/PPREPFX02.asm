*          DATA SET PPREPFX02  AT LEVEL 110 AS OF 12/09/03                      
*PHASE PPFX02A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*  KWAN  08/00     NEW PBILLREC DSECT                                           
*                                                                               
*  SMYE  12/12/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPFX02 - PRINTPAK BILL FIX'                                     
*                                                                               
* QOPT1        'F' = PRINT FORMULAS                                             
* QOPT2        'R' = PRINT REVERSAL INFO                                        
* QOPT3        'N' = NO PRINTING                                                
* QOPT5        'C' = COMMISION ONLY BILLS                                       
*                                                                               
*               TOTALING LOGIC FROM PP10                                        
*        THIS CODE USED TO INSERT JOBS FOR DOREMUS REBATE BILLS                 
*        ERRONEOUSLY RUN ADS TOGETHER NOT SEPERATE                              
*        GETS JOB FROM FIRST BILL FOR THE ESTIMATE (ASSUMED TO BE               
*        REGULAR BILL)                                                          
*                                                                               
PPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PPFX02                                                       
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP05WRKD,R8                                                      
         LA    R7,P                                                             
         USING BILLINED,R7                                                      
*                                                                               
         RELOC RELO                                                             
         SPACE 2                                                                
         CLI   MODE,PROCBIL                                                     
         BE    PRBIL                                                            
         CLI   MODE,CLIFRST           FIRST BILL FOR CLIENT                     
         BE    CLTF                                                             
         CLI   MODE,FBUYREQ                                                     
         BE    FBLR                                                             
         CLI   MODE,LBUYREQ                                                     
         BE    LBLR                                                             
***OFF                                                                          
         CLI   MODE,OFCFRST                                                     
         BE    FBOFF                                                            
         CLI   MODE,OFCLAST                                                     
         BE    LBOFF                                                            
***OFF                                                                          
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         SPACE 3                                                                
FBLR     DS    0H                  FIRST FOR REQUEST                            
         CLI   FIRSTSW,0           FIRST TIME TEST                              
         BNE   FBLR1                                                            
         MVI   FIRSTSW,1                                                        
*                                                                               
         LA    R3,RUNTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         LA    R3,TRUNTOTS         TOTAL FOR TAPE                               
         BAS   RE,CLRTOTS                                                       
         XC    START(12),START                                                  
         XC    AOUTP,AOUTP                                                      
         XC    LSTBLKY,LSTBLKY                                                  
         XC    EATOTS,EATOTS       CLEAR ESTIMATE AMTS                          
         MVC   SAVMAX,MAXLINES                                                  
*                                                                               
FBLR1    DS    0H                                                               
*                                                                               
FBLR3    DS    0H                                                               
         LA    R3,PRDTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         LA    R3,CLTTOTS                                                       
         BAS   RE,CLRTOTS                                                       
***OFF                                                                          
         LA    R3,OFFTOTS                                                       
         BAS   RE,CLRTOTS                                                       
***OFF                                                                          
         LA    R3,REQTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         XC    LASTKEY,LASTKEY                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   TOTSW,0                                                          
         OC    START(12),START                                                  
         BNZ   *+10                                                             
         MVC   START(12),QSTART    SET START-END FROM FRST REQ                  
         CLC   START(12),SPACES                                                 
         BNE   FBLR4                                                            
         MVC   START+00(2),RCDATE+06   YY                                       
         MVC   START+02(2),RCDATE+00   MM                                       
         MVC   START+04(2),RCDATE+03   DD                                       
         MVC   END,START                                                        
FBLR4    DS    0H                                                               
         CLC   QSTART(12),SPACES                                                
         BNE   *+10                                                             
         MVC   QSTART(12),START                                                 
*                                                                               
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'PB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         MVC   WORK+4(2),QAGENCY                                                
         MVC   WORK+6(1),QMEDIA                                                 
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
         EJECT                                                                  
         SPACE 3                                                                
LBLR     DS    0H                  LAST FOR REQUEST                             
         CLC   CLTTOTS(24),=4PL6'0'                                             
         BNE   PRB2B                                                            
LBLR2    DS    0H                                                               
         CLC   REQTOTS(24),=4PL6'0'                                             
         BE    EXIT                                                             
         LA    R3,REQTOTS                                                       
         LA    R4,RUNTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         CLI   QOPT4,C'N'                                                       
         BE    LBLR4                                                            
         LA    R3,REQTOTS                                                       
         LA    R4,TRUNTOTS         ALSO POST TO TAPE TOTALS                     
         BAS   RE,ROLTOTS                                                       
LBLR4    BAS   RE,PRNT                                                          
         MVC   BLINE(18),=C'**REQUEST TOTALS**'                                 
         LA    R3,REQTOTS                                                       
         MVI   TOTSW,2                                                          
         BAS   RE,FMTAMTS                                                       
         BAS   RE,PRNT                                                          
         B     EXIT                                                             
         SPACE 3                                                                
LAST     DS    0H                  RUN LAST                                     
         BAS   RE,PRNT                                                          
         MVC   BLINE(14),=C'**RUN TOTALS**'                                     
         LA    R3,RUNTOTS                                                       
         MVI   TOTSW,2                                                          
         BAS   RE,FMTAMTS                                                       
         MVI   SPACING,2                                                        
         BAS   RE,PRNT                                                          
         MVC   BLINE(15),=C'**TAPE TOTALS**'                                    
         LA    R3,TRUNTOTS                                                      
         MVI   TOTSW,2                                                          
         BAS   RE,FMTAMTS                                                       
         BAS   RE,PRNT                                                          
         L     RF,AOUTP                                                         
         LTR   RF,RF                                                            
         BZ    EXIT                                                             
         BASR  RE,RF                                                            
         B     EXIT                                                             
         EJECT                                                                  
CLTF     DS    0H                  FIRST BILL FOR CLIENT                        
         XC    ESTJOB,ESTJOB                                                    
         XC    OLDCPE,OLDCPE                                                    
         MVI   FINANSW,0                                                        
         CLI   PCLTFIN,C'Y'                                                     
         BNE   *+8                                                              
         MVI   FINANSW,X'01'                                                    
         B     EXIT                                                             
*                                                                               
*                                  PROCESS BILL                                 
         SPACE 2                                                                
PRBIL    DS    0H                                                               
         MVI   FLAG,0                                                           
         TM    PBILCMSW,X'02'      IS IT A COMMISION ONLY BILL                  
         BNZ   PRB09               YES, OK                                      
         CLI   QOPT5,C'C'          NO, ARE WE SKIPPING OTHERS                   
         BE    EXIT                                                             
*                                                                               
PRB09    DS    0H                                                               
         CLI   FINANSW,0                                                        
         BE    EXIT                                                             
         CLC   PBILKCLT(8),OLDCPE                                               
         BE    PRB09A                                                           
         CLI   PBILOTH,X'09'       YES - SAVE PBILLJOB                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ESTJOB,PBILLJOB                                                  
         MVC   OLDCPE,PBILKCLT      SAVE CLIENT/PRD/EST                         
         B     PRB09X                                                           
*                                                                               
PRB09A   OC    ESTJOB,ESTJOB                                                    
         BZ    EXIT                                                             
         CLI   PBILOTH,X'09'                                                    
         BE    *+6                                                              
         DC    H'0'                  MUST HAVE 09 ELEM                          
         OC    PBILLJOB,PBILLJOB                                                
         BZ    PRB09C                                                           
         CLC   PBILLJOB,ESTJOB                                                  
         BE    PRB09X                                                           
         MVI   FLAG,C'X'                                                        
         B     PRB09X                                                           
*                                                                               
PRB09C   MVC   PBILLJOB,ESTJOB                                                  
         MVI   FLAG,C'F'                                                        
         CLI   RCWRITE,C'Y'                                                     
         BNE   PRB09X                                                           
         LA    R0,PBILLREC                                                      
         ST    R0,IOAREA                                                        
         BAS   RE,SPUT                                                          
*                                                                               
PRB09X   CLC   PBILLKEY(14),LSTBLKY  IF FIRST FOR EST/MOS                       
         BE    PRB1                                                             
         MVI   REVSW,C' '          SET NOT A REVISION                           
         MVC   LSTBLKY,PBILLKEY                                                 
         XC    EATOTS,EATOTS                                                    
         B     PRB1D                                                            
*                                                                               
PRB1     DS    0H                                                               
         MVI   REVSW,C'R'          SET IS A REVISION                            
*                                                                               
PRB1D    DS    0H                  ADD TO ESTIMATE AMTS                         
         LA    R2,PBILLGRS         4 FIELDS STARTING AT GROSS                   
         LA    R3,EATOTS                                                        
         LA    R4,4                                                             
*                                                                               
PRB1F    DS    0H                                                               
         ZAP   DUB,0(BPLEQ,R2)                                                  
         CVB   R0,DUB                                                           
         A     R0,0(R3)                                                         
         ST    R0,0(R3)                                                         
*                                                                               
         LA    R2,BPLEQ(R2)                                                     
         LA    R3,4(R3)                                                         
         BCT   R4,PRB1F                                                         
*                                                                               
         CLC   PBILLDAT,QSTART     DATES FILTER                                 
         BL    EXIT                                                             
         CLI   QEND,C' '                                                        
         BE    PRB2                                                             
         CLC   PBILLDAT,QEND                                                    
         BH    EXIT                                                             
*                                                                               
PRB2     DS    0H                                                               
         CLI   FLAG,0              ONLY PROCESS BILLS WITH NO JOB               
         BE    EXIT                OR JOB OTHER THAN THAT OF FIRST              
*                                  BILL FOR THE ESTIMATE                        
*                                                                               
         MVI   RETAIL,C'N'                                                      
         CLI   PBRETAIL,0                                                       
         BE    PRB2C                                                            
*                                  RETAIL BILL                                  
         CLI   PBRETAIL,X'81'      IGNORE CORP BILLS                            
         BE    EXIT                                                             
         MVI   RETAIL,C'Y'                                                      
*                                                                               
*                                                                               
PRB2C    CLI   PBILLMOD,C'P'       BYPASS PRD MODE BILLS                        
         BE    EXIT                                                             
         CLI   PBILLMOD,C'S'       AND SERIES MODE                              
         BE    EXIT                                                             
*                                                                               
         CLI   QOPT3,C'N'          NO PRINTING                                  
         BE    PRB26                                                            
         CLC   PBILKCLT(6),LASTKCLT CHK FOR SAME      CLT + PRD                 
         BE    PRB3                YES                                          
         CLC   PBILKCLT,LASTKCLT   CHK FOR CHANGE IN CLT                        
         BNE   PRB2A               FORCE CHG IN PRD ALSO                        
         CLC   PBILKPRD,LASTKPRD                                                
         BE    PRB2D                                                            
PRB2A    CLI   LASTKPRD,0                                                       
         BE    PRB2D                                                            
PRB2B    DS    0H                                                               
         MVC   BLPRD,LASTKPRD                                                   
         MVC   BLINE+8(18),=C'**PRODUCT TOTALS**'                               
         LA    R3,PRDTOTS                                                       
         MVI   TOTSW,1                                                          
         BAS   RE,FMTAMTS                                                       
         MVI   SPACING,3                                                        
         MVI   MAXLINES,99                                                      
         BAS   RE,PRNT                                                          
         LA    R3,PRDTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         CLI   MODE,LBUYREQ        REQUEST LAST                                 
         BE    PRB2F               MUST ALSO DO CLIENT TOTALS                   
         CLI   MODE,OFCLAST        OR LAST FOR OFFICE                           
         BE    PRB2F               MUST ALSO DO CLIENT TOTALS                   
*                                                                               
PRB2D    CLC   PBILKCLT,LASTKCLT                                                
         BE    PRB3                                                             
         CLI   LASTKCLT,0                                                       
         BE    PRB3                                                             
PRB2F    MVC   BLCLT,LASTKCLT                                                   
         XC    LASTKPRD(25),LASTKPRD                                            
         MVC   BLINE+5(17),=C'**CLIENT TOTALS**'                                
         LA    R3,CLTTOTS                                                       
         MVI   TOTSW,1                                                          
         BAS   RE,FMTAMTS                                                       
         MVI   SPACING,3                                                        
         MVI   MAXLINES,99                                                      
         BAS   RE,PRNT                                                          
***OFF                                                                          
         CLI   QCLIENT,C'$'          SEE IF DOING OFFILCE LIST                  
         BNE   PRB2M                 ROLL CLTTOTS TO OFFTOTS                    
         LA    R3,CLTTOTS                                                       
         LA    R4,OFFTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         B     PRB2P                                                            
***OFF                                                                          
PRB2M    LA    R3,CLTTOTS                                                       
         LA    R4,REQTOTS                                                       
         BAS   RE,ROLTOTS                                                       
PRB2P    LA    R3,CLTTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         CLI   MODE,LBUYREQ            RETURN TO REQTOTALS                      
         BE    LBLR2                                                            
         CLI   MODE,OFCLAST            RETURN TO OFFICE TOTALS                  
         BE    LBOFF5                                                           
PRB3     DS    0H                                                               
         MVC   LASTKEY,PBILLKEY                                                 
         SPACE 2                                                                
*                                  CREATE PRINT LINE(S)                         
         MVC   BLCLT,PBILKCLT           CLT                                     
         MVC   BLPRD,PBILKPRD           PRD                                     
         OC    PBILKEST,PBILKEST                                                
         BZ    PRB7                                                             
         LA    R3,PBILKEST                                                      
         BAS   RE,CVD                                                           
         MVC   BLEST(3),WORK+2        ONE EST                                   
         B     PRB8                                                             
*                                                                               
PRB7     DS    0H                                                               
         MVC   BLEST+2(2),=C'NO'                                                
         OC    PBILESTS(4),PBILESTS                                             
         BZ    PRB8                                                             
         LA    R3,PBILESTS                                                      
         BAS   RE,CVD                                                           
         MVC   BLEST(3),WORK+2                                                  
         MVI   BLEST+3,C'-'             RANGE OF ESTS                           
         LA    R3,PBILESTE                                                      
         BAS   RE,CVD                                                           
         MVC   BLEST+4(3),WORK+2                                                
*                                                                               
PRB8     DS    0H                                                               
         MVC   BLPER(6),PBILLJOB                                                
         B     PRB12                                                            
***OLD                                                                          
         CLI   PBILLPER,C' '       NOT USED FOR NEW BILLING                     
         BNH   PRB9                                                             
         CLI   PBILLPER,C'M'       TEST MONTH BILL                              
         BNE   PRB10                                                            
PRB9     GOTO1 DATCON,DMCB,(3,PBILKMOS),(9,BLPER)                               
*                                                                               
         B     PRB12                                                            
*                                                                               
PRB10    DS    0H                  SPECIAL BILL PERIOD                          
         MVC   FULL(2),PBILKMOS                                                 
         MVC   FULL+2(1),PBILLSTA                                               
         GOTO1 DATCON,DMCB,(3,FULL),(7,BLPER)                                   
*                                                                               
         MVI   BLPER+5,C'-'                                                     
         MVC   FULL+2(1),PBILLEND                                               
         GOTO1 (RF),(R1),,(3,WORK)                                              
*                                                                               
         MVC   BLPER+6(5),WORK+3                                                
*                                                                               
PRB12    DS    0H                                                               
         LA    R3,PBILKBNO                                                      
         BAS   RE,CVD                                                           
         MVC   BLINO+2(4),WORK+1                                                
         MVC   DINVNO+2(4),WORK+1  SAVE INVOICE NUMBER                          
*                                                                               
         MVC   DINVNO(2),PBILLDAT+2 INVOICE MONTH                               
         CLI   B1XPROF+4,0         TEST ANY SPECIAL BASE YEAR                   
         BE    PRB13                                                            
         PACK  DUB,PBILLDAT(2)     YEAR OF BILL                                 
         CVB   R0,DUB                                                           
         ZIC   RF,B1XPROF+4        INVOICE BASE YEAR                            
         SR    R0,RF               DIFFERENCE                                   
         BNP   PRB13                                                            
         MH    R0,=H'12'                                                        
         PACK  DUB,PBILLDAT+2(2)   MONTH                                        
         CVB   RF,DUB                                                           
         AR    R0,RF                                                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DINVNO(2),DUB       SET NEW INVOICE 'MONTH'                      
*                                                                               
PRB13    DS    0H                                                               
         MVC   BLINO(2),DINVNO                                                  
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(5,BLRDAT)     RUN DATE                 
*                                                                               
         GOTO1 DATCON,(R1),(3,PBILINVD),(7,BLBDAT)     BILL DATE                
*                                                      TYPE OF BILL             
         MVC   BLTYP+1(2),PBILLTYP                                              
         CLI   PBILLTYP,C'0'       SEE IF NEW BILL                              
         BNH   PRB14               YES                                          
*                                                                               
         MVC   BLTYP(3),=C'ORI'                                                 
         CLI   PBILLTYP,C'3'                                                    
         BE    PRB14                                                            
         MVC   BLTYP(3),=C'DET'                                                 
         CLI   PBILLTYP,C'4'                                                    
         BE    PRB14                                                            
         MVC   BLTYP(3),=C'MAN'                                                 
PRB14    DS    0H                                                               
         MVC   BLTYP+0(1),FLAG                                                  
         MVI   BLTYP+3,C'-'                                                     
*                                                                               
         TM    PBILCMSW,X'02'          FOR COMMISSION ONLY BILL                 
         BZ    PRB15                                                            
         MVC   BLTYP+4(3),=C'COM'                                               
         CLI   QOPT5,C'C'          TEST TO LEAVE NET                            
         BE    *+10                                                             
         ZAP   PBILLNET,=P'0'      NO, SET NET = 0 (AC = RCVBL)                 
         B     PRB16                                                            
*                                                                               
PRB15    DS    0H                                                               
         MVC   BLTYP+4(3),=C'ADJ'                                               
         CLI   PBILSEP,C'A'                                                     
         BE    PRB16                                                            
         MVC   BLTYP+4(3),=C'CD '                                               
         CLI   PBILSEP,C'C'                                                     
         BE    PRB16                                                            
         MVC   BLTYP+4(3),=C'REG'                                               
*                                                                               
PRB16    DS    0H                                                               
*                                  SET MATS IN 5 FIELDS FOR ACCUMES             
*                                  (MATCH 5 PRINT COLUMNS)                      
         ZAP   BILTOTS(6),PBILLGRS     GROSS                                    
         ZAP   BILTOTS+6(6),PBILLNET   NET                                      
         ZAP   BILTOTS+12(6),=P'0'                                              
         SP    BILTOTS+12(6),PBILLRCV  CD IS -(RCVBL)                           
         CLI   PBILSEP,C'C'            FOR SEP CD BILL                          
         BE    PRB16B                                                           
*                                                                               
         ZAP   BILTOTS+12(6),=P'0'     CD IS 0                                  
         CLI   PBILCDSW,C'S'           IF SEP (ON OTHER BILL) BILL              
         BE    PRB16B                                                           
*                                                                               
         ZAP   BILTOTS+12(6),PBILLGRS    GROSS                                  
         SP    BILTOTS+12(6),PBILLBIL    -(G - CD) = CD                         
*                                                                               
PRB16B   DS    0H                                                               
         ZAP   BILTOTS+24(6),PBILLRCV     RCVBL                                 
         ZAP   DUB,PBILLRCV                                                     
         TM    PBILCMSW,X'02'         UNLESS COMMISSION ONLY BILL               
         BNZ   PRB16D                                                           
         TM    PBILBASA,X'04'         IS CD ALREADY TAKEN OUT                   
         BNZ   PRB16D                 YES                                       
         SP    DUB,BILTOTS+12(6)      NO, TAKE IT OUT NOW                       
*                                                                               
PRB16D   DS    0H                                                               
         ZAP   BILTOTS+18(6),DUB      FOR AC CALC                               
         SP    BILTOTS+18(6),PBILLNET    -NET = TRUE AC                         
*                                                                               
         LA    R3,BILTOTS                                                       
         BAS   RE,FMTAMTS               $ AMOUNTS                               
*                                                                               
         BAS   RE,PRNT                                                          
*                                                                               
         CLI   QOPT1,C'F'               FORMULA OPTION                          
         BNE   PRB22                    NO                                      
         OC    PBILBASA(5),PBILBASA                                             
         BZ    PRB22                    NO FORMULA                              
         CLI   PBILBASA,5          DONT PRINT IF G-CD +0 PCT                    
         BNE   *+14                                                             
         OC    PBILADJ,PBILADJ                                                  
         BZ    PRB22                                                            
         LA    R3,BLINO                                                         
         MVC   0(8,R3),=C'FORMULA='                                             
         SR    RF,RF                                                            
         IC    RF,PBILBASA                                                      
         SLL   RF,2                                                             
         LA    RF,ADJWRDS-4(RF)                                                 
         MVC   9(4,R3),0(RF)                                                    
         LA    R3,11(R3)                                                        
         CLI   1(RF),C' '                                                       
         BE    *+8                                                              
         LA    R3,3(R3)                                                         
         MVI   1(R3),C'0'                                                       
         LA    R0,1                                                             
         OC    PBILADJ,PBILADJ                                                  
         BZ    PRB18                                                            
         EDIT  (B3,PBILADJ),(8,1(R3)),5,ALIGN=LEFT                              
         LR    R4,R3                                                            
         AR    R4,R0               POINT TO END                                 
PRB17    DS    0H                                                               
         LR    R5,R4                                                            
         CLI   0(R4),C'0'                                                       
         BE    PRB17D                                                           
         CLI   0(R4),C'.'                                                       
         BE    PRB17B                                                           
         B     PRB18                                                            
PRB17B   LA    R4,1                END SCAN ON .                                
PRB17D   BCTR  R0,R0               SHORTEN LENGTH                               
         MVI   0(R5),C' '                                                       
PRB17F   BCT   R4,PRB17                                                         
*                                                                               
PRB18    DS    0H                                                               
         MVI   0(R3),C'+'                                                       
         TM    PBILADJ,X'80'                                                    
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
*                                                                               
         AR    R3,R0                                                            
         MVC   2(6,R3),=C'PCT OF'                                               
         SR    RF,RF                                                            
         IC    RF,PBILBASB                                                      
         SLL   RF,2                                                             
         LA    RF,ADJWRDS-4(RF)                                                 
         MVC   9(4,R3),0(RF)                                                    
         MVI   MAXLINES,99                                                      
         BAS   RE,PRNT                                                          
*                                                                               
PRB22    DS    0H                                                               
         CLI   QOPT2,C'R'               REVERSALS OPTION                        
         BNE   PRB26                    NO                                      
         CLI   PBILLCDT,C'0'                                                    
         BE    PRB26                    NOT REVERSED                            
         LA    R3,BLINO                                                         
         MVC   0(11,R3),=C'REVERSED BY'                                         
         MVC   12(4,R3),PBILLCAN+2                                              
         MVC   17(2,R3),=C'ON'                                                  
         GOTO1 DATCON,DMCB,(0,PBILLCDT),(5,20(R3))                              
*                                                                               
         MVI   MAXLINES,99                                                      
         BAS   RE,PRNT                                                          
*                                                                               
PRB26    DS    0H                                                               
         LA    R3,BILTOTS                                                       
         LA    R4,PRDTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         LA    R3,BILTOTS                                                       
         LA    R4,CLTTOTS                                                       
         BAS   RE,ROLTOTS                                                       
*                                                                               
PRB28    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***OFF                                                                          
LBOFF    DS    0H                                                               
         CLC   CLTTOTS(24),=4PL6'0'                                             
         BNE   PRB2B      MUST DO PRD/CLT TOTALS FIRST                          
*                         WILL RETURN TO LBOFF5                                 
LBOFF5   MVI   BLCLT,C'*'                                                       
         MVC   BLCLT+1(1),RCSVOFC                                               
         MVI   BLCLT+2,C' '                                                     
         MVC   BLINE+5(17),=C'**OFFICE TOTALS**'                                
         CLC   OFFTOTS(24),=4PL6'0'                                             
         BNE   LBOFF8                                                           
         MVC   BLINE+25(11),=C'NO ACTIVITY'                                     
         BAS   RE,PRNT                                                          
         B     EXIT                                                             
*                                                                               
LBOFF8   LA    R3,OFFTOTS                                                       
         MVI   TOTSW,2                                                          
         BAS   RE,FMTAMTS                                                       
         MVI   SPACING,3                                                        
         MVI   MAXLINES,99                                                      
         BAS   RE,PRNT                                                          
         LA    R3,OFFTOTS                                                       
         LA    R4,REQTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         LA    R3,OFFTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         B     EXIT                                                             
         SPACE 3                                                                
FBOFF    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         XC    LASTKEY,LASTKEY                                                  
         LA    R3,OFFTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         B     EXIT                                                             
***OFF                                                                          
*                                  FORMAT $ AMTS                                
         SPACE 2                                                                
FMTAMTS  NTR1                                                                   
         SPACE 2                                                                
         LA    R5,0(R3)            GROSS                                        
         LA    R4,BLGRS                                                         
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,6(R3)           NET                                           
         LA    R4,BLNET                                                         
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,12(R3)           CD                                           
         LA    R4,BLCD                                                          
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,18(R3)           AC                                           
         LA    R4,BLAC                                                          
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,24(R3)           ACTUAL                                       
         LA    R4,BLBIL                                                         
         BAS   RE,FMTEDT                                                        
*                                                                               
         MVI   TOTSW,0                                                          
         B     EXIT                                                             
         SPACE 2                                                                
FMTEDT   DS    0H                                                               
         CP    0(6,R5),=P'0'                                                    
         BER   RE                                                               
FMTEDT5  EDIT  (P6,0(R5)),(15,0(R4)),2,COMMAS=YES,CR=YES                        
*                                                                               
         CLI   TOTSW,0                                                          
         BER   RE                                                               
         CLI   13(R4),C'C'                                                      
         BER   RE                                                               
         MVI   13(R4),C'*'                                                      
         CLI   TOTSW,1                                                          
         BER   RE                                                               
         MVI   14(R4),C'*'                                                      
         BR    RE                                                               
         SPACE 3                                                                
PRNT     NTR1                                                                   
         SPACE 2                                                                
         CLI   FORCEHED,C'Y'                                                    
         BE    PRNT2                                                            
         CLC   LINE,MAXLINES                                                    
         BL    PRNT90                                                           
PRNT2    DS    0H                                                               
         CLI   MODE,LBUYREQ          REQUEST TOTALS                             
         BE    PRNT90                                                           
         CLI   QCLIENT,C'$'          SEE IF DOING OFFICE LIST REQ               
         BNE   PRNT90                                                           
         MVC   HEAD5(8),=C'OFFICE X'                                            
         MVC   HEAD5+7(1),RCSVOFC                                               
*                                                                               
*                                                                               
         SPACE 2                                                                
PRNT90   DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   MAXLINES,SAVMAX                                                  
         B     EXIT                                                             
         SPACE 3                                                                
ROLTOTS  DS    0H                                                               
         LA    R0,5                                                             
ROLTOTS2 DS    0H                                                               
         AP    0(6,R4),0(6,R3)                                                  
         LA    R4,6(R4)                                                         
         LA    R3,6(R3)                                                         
         BCT   R0,ROLTOTS2                                                      
         BR    RE                                                               
         SPACE 3                                                                
CLRTOTS  DS    0H                                                               
         LA    R0,5                                                             
         ZAP   0(6,R3),=PL6'0'                                                  
         LA    R3,6(R3)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
         SPACE 3                                                                
CVD      DS    0H                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),0(R3)                                                  
*                                                                               
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB                                                      
         BR    RE                                                               
         SPACE 3                                                                
ADJWRDS  DS    0C                                                               
         DC    C'G   '                                                          
         DC    C'N   '                                                          
         DC    C'    '                                                          
         DC    C'    '                                                          
         DC    C'G-CD'                                                          
         DC    C'N-CD'                                                          
         SPACE 2                                                                
         LTORG                                                                  
SPUT     NTR1                                                                   
         GOTO1 DATAMGR,DMCB,PUTREC,PRTFILE,KEY+27,IOAREA,DMWORK                 
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
PP05WRKD DSECT                                                                  
PP05WRK  DS    0C                                                               
AOUTP    DS    A                                                                
RELO     DS    A                                                                
IOAREA   DS    A                                                                
ESTJOB   DS    CL6                                                              
FINANSW  DS    CL1                                                              
NEWEST   DS    CL1                                                              
FLAG     DS    CL1                                                              
OLDCPE   DS    CL8                 OLD CLT/PRD/EST                              
*                                                                               
START    DS    CL6                                                              
END      DS    CL6                                                              
FIRSTSW  DS    X                                                                
REVSW    DS    CL1                 REVISION STATUS                              
RETAIL   DS    CL1                 'Y' IF RETAIL BILL                           
LSTBLKY  DS    XL25                                                             
         DS    0F                                                               
EATOTS   DS    XL16                                                             
TAPTYP   DS    C                                                                
LASTKEY  DS    0CL32                                                            
         DS    CL4                                                              
LASTKCLT DS    CL3                                                              
LASTKPRD DS    CL3                                                              
         DS    CL22                                                             
*                                                                               
B1XPROF  DS    XL16                                                             
DINVNO   DS    CL6                                                              
*                                                                               
DYNDDN   DS    CL8                                                              
DYNDSN   DS    CL20                                                             
*                                                                               
BILTOTS  DS    5PL6                                                             
PRDTOTS  DS    5PL6                                                             
CLTTOTS  DS    5PL6                                                             
OFFTOTS  DS    5PL6                                                             
REQTOTS  DS    5PL6                                                             
RUNTOTS  DS    5PL6                                                             
TRUNTOTS DS    5PL6                FOR TAPE REQS                                
*                                                                               
TOTSW    DS    X                                                                
SAVMAX   DS    X                                                                
SVCD     DS    PL8                                                              
*                                                                               
PBIREC   DS    CL256                                                            
BILLINED DSECT                                                                  
BLINE    DS    0C                                                               
BLCLT    DS    CL3                                                              
         DS    CL1                                                              
BLPRD    DS    CL3                                                              
         DS    CL1                                                              
BLEST    DS    CL7                                                              
         DS    CL1                                                              
BLPER    DS    CL11                                                             
         DS    CL1                                                              
BLINO    DS    CL6                                                              
         DS    CL1                                                              
BLRDAT   DS    CL8                                                              
         DS    CL1                                                              
BLBDAT   DS    CL5                                                              
         DS    CL1                                                              
BLTYP    DS    CL7                                                              
BLGRS    DS    CL15                                                             
BLNET    DS    CL15                                                             
BLCD     DS    CL15                                                             
BLAC     DS    CL15                                                             
BLBIL    DS    CL15                                                             
         SPACE 3                                                                
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPREPWORK                                                      
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
BPLEQ    EQU   6                   LENGTH OF PBILLREC PACKS (PL6)               
*                                                                               
       ++INCLUDE PPMODEQU                                                       
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'110PPREPFX02 12/09/03'                                      
         END                                                                    
