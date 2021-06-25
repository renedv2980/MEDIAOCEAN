*          DATA SET SPREPC002  AT LEVEL 015 AS OF 02/08/07                      
*PHASE SPC002A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPREPC002 - COLGATE INTERFACE'                                  
***********************************************************************         
*                                                                     *         
*   QOPT1 -  Y = PRODUCE OUTPUT TAPE                                  *         
*   QOPT2 -  Y = PRINT SORTER TRACE                                   *         
*                                                                     *         
***********************************************************************         
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*   BPLA  2/7/07  SKIP CREDIT INVOICES AND THEIR DETAILS                        
*                                                                               
***********************************************************************         
         SPACE 2                                                                
SPC002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPC002                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING BILWRKD,RC                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
         CLI   MODE,CLTLAST                                                     
         BE    CLTL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
RUNF     DS    0H                                                               
         MVI   TAPEOPEN,C'N'                                                    
         ZAP   GTDNET,=P'0'                                                     
         ZAP   GTHNET,=P'0'                                                     
         XC    TDCOUNT,TDCOUNT     TOTAL DETAIL RECORD COUNTER                  
         XC    THCOUNT,THCOUNT     TOTAL HEADER RECORD COUNTER                  
         XC    TRCOUNT,TRCOUNT     TOTAL DETAIL & HEADER REC COUNTER            
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,XX,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=XX'                                    
         EJECT                                                                  
REQF     DS    0H                                                               
         LHI   R0,INVKEYLQ         SOFT SORT KEY LENGTH                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(2),DUB                                               
         LHI   R0,INVRECLQ         SOFT SORT RECORD LENGTH                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(2),DUB                                                
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
*                                  SET VALUES FOR RUN-DATE FILTERING            
         GOTO1 DATCON,DMCB,TODAY,(3,THREE)                                      
         ZIC   R1,THREE                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
         STC   R1,DECADE           80,90,00,ETC.                                
         MVC   YEARDIG,TODAY+1     GET YEAR WITHIN DECADE                       
         NI    YEARDIG,X'FF'-X'F0' ISOLATE YEAR DIGIT                           
*                                                                               
         GOTO1 DATCON,DMCB,QSTART,(2,BQSTARTP)                                  
         GOTO1 DATCON,DMCB,QSTART,(3,BQSTART)                                   
         GOTO1 DATCON,DMCB,QEND,(2,BQENDP)                                      
         GOTO1 DATCON,DMCB,QEND,(3,BQEND)                                       
*                                                                               
*        GET QSTART AND QEND INTO Y2K FORMAT                                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,BQSTARTP),(0,QSTART)                              
         GOTO1 DATCON,DMCB,(2,BQENDP),(0,QEND)                                  
*                                                                               
         MVC   QMKT(3),=C'ALL'                                                  
         MVC   QSTA(3),=C'ALL'                                                  
*                                                                               
         XC    STRTMOS,STRTMOS     SET START MOS FILTER LOW                     
         MVC   ENDMOS,=X'FFFF'     SET END MOS FILTER HIGH                      
         CLI   QSTRTMOS,C' '       MONTH OF SERVICE START FILTER GIVEN?         
         BE    REQF10                                                           
         MVC   DUB(4),QSTRTMOS                                                  
         MVC   DUB+4(2),=C'01'     ARBITRARY FOR DATCON: FIRST OF MONTH         
         GOTO1 DATCON,DMCB,DUB,(3,FULL)                                         
         MVC   STRTMOS,FULL        KEEP Y/M (TOSS DAY)                          
         MVC   ENDMOS,FULL         SET END EQUAL TO START                       
*                                                                               
         CLI   QENDMOS,C' '        MONTH OF SERVICE END FILTER GIVEN?           
         BE    REQF10                                                           
         MVC   DUB(4),QENDMOS                                                   
         MVC   DUB+4(2),=C'01'     ARBITRARY FOR DATCON: FIRST OF MONTH         
         GOTO1 DATCON,DMCB,DUB,(3,FULL)                                         
         MVC   ENDMOS,FULL         KEEP Y/M (TOSS DAY)                          
*                                                                               
REQF10   DS    0H                                                               
         CLC   QEST,=C'ALL'                                                     
         BNE   *+10                                                             
         MVC   QEST,=C'NO '                                                     
*                                                                               
         CLI   TAPEOPEN,C'Y'       OUTPUT DATASET ALREADY OPEN?                 
         BE    REQFX                                                            
         CLI   QOPT1,C'Y'          WRITE TAPE DATA FOR THIS REQUEST?            
         BNE   REQFX                                                            
*                                                                               
         MVC   DSNAME+13(2),QAGY                                                
         GOTO1 DYNALLOC,DMCB,(0,=C'OUTFILE '),DSNAME                            
         OPEN  (OUTFILE,OUTPUT)                                                 
         MVI   TAPEOPEN,C'Y'                                                    
*                                                                               
REQFX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
CLTF     DS    0H                                                               
*                                                                               
         XC    WORK,WORK           BUILD PROFILE KEY IN WORK                    
         LA    R2,WORK                                                          
         USING PROFKD,R2                                                        
*                                                                               
         MVI   PROFKSYS,C'S'                                                    
         MVC   PROFKPGM,=C'0B1'    READ B1 PROFILE                              
         MVC   PROFKAGN,AGY                                                     
         MVC   PROFKMED,MED                                                     
         MVC   PROFKCLI,CLT                                                     
         L     RF,ADCLT                                                         
         USING CLTHDRD,RF                                                       
         MVI   PROFKOI2,C'*'       SET OFFICE CODE                              
         MVC   PROFKOCD,COFFICE                                                 
         DROP  RF                                                               
         GOTO1 GETPROF,DMCB,WORK,PROFB1,DATAMGR                                 
*                                                                               
         MVI   PROFKSYS,C'S'-X'40' LOWER-CASE 'S'                               
         MVC   PROFKPGM,=C'B1X'    READ B1X PROFILE                             
         GOTO1 GETPROF,DMCB,WORK,PROFB1X,DATAMGR                                
         DROP  R2                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
CLTL     DS    0H                                                               
*                                                                               
         BRAS  RE,RNBILL                                                        
         BRAS  RE,ROBILL                                                        
         BRAS  RE,REPRT                                                         
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
RUNL     DS    0H                                                               
         L     R1,THCOUNT                                                       
         L     R0,TDCOUNT                                                       
         AR    R1,R0                                                            
         C     R1,TRCOUNT                                                       
         BE    RUNL10                                                           
         MVC   P(45),=C'ERROR: TOTAL NUMBER OF RECORDS DOES NOT MATCH'          
         GOTO1 REPORT                                                           
*                                                                               
RUNL10   DS    0H                                                               
         CP    GTHNET,GTDNET                                                    
         BE    RUNL20                                                           
         MVC   P(58),=C'ERROR: GRAND TOTAL HEADER AMOUNT AND DETAIL AMO+        
               UNT UNEQUAL'                                                     
         GOTO1 REPORT                                                           
         MVC   P(9),=C'HEADER = '                                               
         EDIT  GTHNET,(16,P+9),ZERO=NOBLANK                                     
         GOTO1 REPORT                                                           
         MVC   P(9),=C'DETAIL = '                                               
         EDIT  GTDNET,(16,P+9),ZERO=NOBLANK                                     
         GOTO1 REPORT                                                           
*                                                                               
RUNL20   DS    0H                                                               
         LA    R8,OUTREC                                                        
         USING CGDATA,R8                                                        
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    RE,CGDATA           CLEAR RECORD                                 
         LHI   RF,L'CGDATA                                                      
         SR    R0,R0                                                            
         L     R1,=X'40000000'     PAD CHAR IS SPACE                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   CGTREID,SUMMARY_RECORD                                           
         MVC   CGTREID+1(L'CGTREID-1),CGTREID                                   
         GOTO1 DATCON,DMCB,(5,0),(20,WORK)                                      
         MVC   CGTCRDT(4),WORK+4   MMDD                                         
         MVC   CGTCRDT+4(4),WORK   YYYY                                         
         TIME  DEC                                                              
         ST    R0,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,DUB,4,=C'TOG'                                   
         MVC   CGTCRTM,DUB                                                      
*                                                                               
         EDIT  TRCOUNT,CGTTRCT,FILL=0                                           
         EDIT  THCOUNT,CGTHRCT,FILL=0                                           
         EDIT  TDCOUNT,CGTDRCT,FILL=0                                           
         EDIT  GTHNET,(17,CGTGRAM+2),FILL=0                                     
         EDIT  GTDNET,(17,CGTDIAM+2),FILL=0                                     
         MVC   CGTGRAM(2),=C'00'                                                
         MVC   CGTDIAM(2),=C'00'                                                
         MVI   CGTRETY,SUMMARY_RECORD                                           
*                                                                               
         CLI   TAPEOPEN,C'Y'       WAS OUTPUT DATASET EVER OPENED?              
         BNE   RUNL30                                                           
         L     R1,=A(OUTFILE)                                                   
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
RUNL30   DS    0H                                                               
         MVC   P(43),=C'SUMMARY RECORD - TOTAL OF HEADER AND DETAIL'            
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P,SPACES                                                         
         LA    R2,P                                                             
         USING PLINED,R2                                                        
*                                                                               
         MVC   PTREID,CGTREID                                                   
         MVC   PTCRDT,CGTCRDT                                                   
         MVC   PTCRTM,CGTCRTM                                                   
         MVC   PTTRCT,CGTTRCT                                                   
         MVC   PTHRCT,CGTHRCT                                                   
         MVC   PTGRAM,CGTGRAM                                                   
         MVC   PTDRCT,CGTDRCT                                                   
         MVC   PTDIAM,CGTDIAM                                                   
         MVC   PTRETY,CGTRETY                                                   
*                                                                               
         GOTO1 REPORT                                                           
         DROP  R2,R8                                                            
*                                                                               
         CLI   ERROR,0                                                          
         BE    RUNL40                                                           
         MVC   P(45),=C'***** ERRORS - NO OUTPUT FILE GENERATED *****'          
         GOTO1 REPORT                                                           
         B     RUNLX                                                            
*                                                                               
RUNL40   DS    0H                                                               
         CLI   TAPEOPEN,C'Y'       WAS OUTPUT DATASET EVER OPENED?              
         BNE   RUNLX                                                            
         CLOSE (OUTFILE)                                                        
*                                                                               
RUNLX    DS    0H                                                               
         B     EXIT                                                             
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 2                                                                
DSNAME   DC    CL20'SPTTAPE.SP0CLXX'                                            
         SPACE 2                                                                
OUTFILE  DCB   DDNAME=OUTFILE,DSORG=PS,RECFM=FB,LRECL=256,             X        
               BLKSIZE=2560,MACRF=PM                                            
         TITLE 'RNBILL - READ NEW BILL RECORDS'                                 
RNBILL   NMOD1 0,RNBILL                                                         
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING STABUCKD,R2                                                      
*                                                                               
         MVC   STABKCOD,=X'0E01'   RECORD TYPE                                  
         MVC   STABKAM,BAGYMD                                                   
         MVC   STABKCLT,BCLT                                                    
         CLI   KPRD,0                                                           
         BE    RNB30               SKIP REST OF KEY NOW IF MULTI-PRD            
         MVC   STABKPRD,KPRD                                                    
*                                                                               
RNB10    DS    0H                                                               
         MVC   STABKEST,BEST                                                    
         XC    STABKMKT,STABKMKT                                                
         XC    STABKSTA,STABKSTA                                                
         MVI   STABKCUR,0                                                       
         CLI   BEST,0                                                           
         BE    RNB30               SKIP REST OF KEY NOW IF MULTI-EST            
*                                                                               
RNB20    DS    0H                                                               
         XC    STABKMKT,STABKMKT                                                
         XC    STABKSTA,STABKSTA                                                
         MVI   STABKCUR,0                                                       
*                                                                               
RNB30    DS    0H                                                               
         GOTO1 HIGH                                                             
         B     RNB50                                                            
*                                                                               
RNB40    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
RNB50    DS    0H                                                               
         CLC   KEY(5),KEYSAVE      AGM/CLT                                      
         BNE   RNBX                                                             
         CLI   KPRD,0                                                           
         BE    *+14                                                             
         CLC   KPRD,STABKPRD       ONE PROD MUST BE EQUAL                       
         BNE   RNBX                                                             
*                                                                               
         CLI   BEST,0                                                           
         BE    RNB60                                                            
*                                  ONE EST OR SERIES                            
         CLC   STABKEST,BEST                                                    
         BL    RNB10                                                            
         BE    RNB60                                                            
         CLI   BESTEND,0                                                        
         BE    *+14                                                             
         CLC   STABKEST,BESTEND                                                 
         BNH   RNB60                                                            
*                                  EST NOT OK                                   
         CLI   KPRD,0                                                           
         BNE   RNBX                DONE IF ONE PRD                              
         ZIC   RF,STABKPRD         ELSE NEXT PROD                               
         AHI   RF,1                                                             
         STC   RF,STABKPRD                                                      
         B     RNB10                                                            
*                                  BUMP TO NEXT EST                             
         ZIC   RF,STABKEST                                                      
         AHI   RF,1                                                             
         STC   RF,STABKEST                                                      
         B     RNB20                                                            
         DROP  R2                                                               
*                                  HAVE GOOD KEY                                
RNB60    DS    0H                                                               
         LA    R7,STABREC                                                       
         ST    R7,AREC                                                          
         USING STABUCKD,R7                                                      
         GOTO1 GET                                                              
*                                                                               
         MVI   ELCODE,X'0E'                                                     
         LR    R2,R7                                                            
         BAS   RE,GETEL                                                         
         BE    RNB80                                                            
         B     RNB40                                                            
*                                                                               
RNB70    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   RNB40                                                            
*                                                                               
RNB80    DS    0H                                                               
         USING STABELEM,R2                                                      
*                                  TEST IN REQ PERIOD                           
         CLC   STABBDT,BQSTARTP                                                 
         BL    RNB70                                                            
         CLC   STABBDT,BQENDP                                                   
         BH    RNB70                                                            
*                                  MOS FILTER                                   
         CLC   STABPER,STRTMOS                                                  
         BL    RNB70                                                            
         CLC   STABPER,ENDMOS                                                   
         BH    RNB70                                                            
*                                                                               
         XC    SORTREC,SORTREC                                                  
         LA    R4,SORTREC                                                       
         USING INVD,R4                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,STABBDT),(0,WORK)                                 
         MVC   HALF,STABINV                                                     
         NI    HALF,X'FF'-(STABINV_REVERSAL+STABINV_REVERSED)                   
         L     RF,ADCONLST                                                      
         L     RF,VSPFMINO-SPADCONS(,RF)   A(SPFMTINO)                          
         GOTO1 (RF),DMCB,WORK,(2,HALF),(QMED,PROFB1),PROFB1X                    
         L     RF,DMCB+4           A(MN-NNNN) INVOICE NUMBER                    
         MVC   INVINVNO(2),0(RF)   FIRST TWO DIGITS                             
         MVC   INVINVNO+2(4),3(RF) LAST FOUR DIGITS                             
*                                                                               
         L     RF,ADCLT            FIND PRD CODE                                
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
RNB90    DS    0H                                                               
         CLC   STABKPRD,3(RF)                                                   
         BE    *+12                                                             
         LA    RF,4(RF)                                                         
         B     RNB90                                                            
*                                                                               
         MVC   INVPRD,0(RF)                                                     
         MVC   INVEST,STABKEST                                                  
         MVC   INVAGMD,STABKAM                                                  
         MVC   INVCLT,STABKCLT                                                  
         MVC   INVMKSTA,STABKMKT                                                
         MVC   INVSPTS,STABSPTS                                                 
         MVI   INVTYPE,DETAIL_RECORD                                            
         ICM   RF,15,STABNET                                                    
         CVD   RF,DUB                                                           
         ZAP   INVDNET,DUB                                                      
         GOTO1 DATCON,DMCB,(3,STABPER),(20,INVMOS)                              
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         CLI   QOPT2,C'Y'                                                       
         BNE   RNB70                                                            
         GOTO1 PRNTBL,DMCB,=C'PUT STABUCK DETAILS',SORTREC,C'DUMP',    +        
               INVRECLQ,=C'1D'                                                  
         B     RNB70                                                            
*                                                                               
RNBX     DS    0H                                                               
         J     EXIT                                                             
         SPACE 2                                                                
         GETEL R2,24,ELCODE                                                     
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'ROBILL  - READ OLD BILL RECORDS'                                
ROBILL   NMOD1 0,ROBILL            HEADER RECORDS                               
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         LA    R2,KEY                                                           
         USING BKEY,R2                                                          
         XC    BKEY,BKEY                                                        
*                                                                               
         MVC   BKEYAM,BAGYMD                                                    
         MVC   BKEYCLT,BCLT                                                     
         CLI   KPRD,0                                                           
         BE    ROB20               SKIP REST OF KEY IF MULTI-PRD                
         MVC   BKEYPRD,PRD                                                      
*                                                                               
ROB10    DS    0H                                                               
         MVC   BKEYEST,BEST                                                     
         XC    KEY+8(5),KEY+8                                                   
         CLI   BEST,0                                                           
         BE    ROB20               SKIP REST OF KEY IF MULTI-EST                
*                                                                               
         XC    KEY+8(5),KEY+8                                                   
*                                                                               
ROB20    DS    0H                                                               
         GOTO1 HIGH                                                             
         B     ROB40                                                            
*                                                                               
ROB30    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
ROB40    DS    0H                                                               
         CLC   KEY(4),KEYSAVE      AGM/CLT                                      
         BNE   ROBX                                                             
         CLI   KPRD,0                                                           
         BE    *+14                                                             
         CLC   PRD,BKEYPRD         ONE PRD MUST BE EQUAL                        
         BNE   ROBX                                                             
*                                  MULTI-PROD SITUATION                         
         OC    KEY+8(5),KEY+8                                                   
         BZ    ROB30               BYPASS IF NOT A BILL                         
*                                                                               
         CLI   BEST,0                                                           
         BE    ROB50                                                            
*                                  ONE EST OR SERIES                            
         CLC   BKEYEST,BEST                                                     
         BL    ROB10                                                            
         BE    ROB50                                                            
*                                                                               
         CLI   BESTEND,0                                                        
         BE    *+14                                                             
         CLC   BKEYEST,BESTEND                                                  
         BNH   ROB50                                                            
*                                                                               
*                                  EST NOT OK                                   
         CLI   KPRD,0                                                           
         BNE   ROBX                DONE IF ONE PROD                             
         ZIC   RF,BKEYPRD+2        BUMP TO NEXT PROD                            
         AHI   RF,1                                                             
         STC   RF,BKEYPRD+2                                                     
         B     ROB10               SET KEY FROM EST DOWN                        
*                                                                               
ROB50    DS    0H                  BILL DATE                                    
         ZIC   R0,BKEYMBIL                                                      
         SRL   R0,4                YEAR DIGIT OF BILL                           
         ZIC   RE,DECADE                                                        
         CLM   R0,1,YEARDIG        COMPARE TO YEAR OF TODAY                     
         BNH   *+8                 IF NOT HIGH, OK                              
         SHI   RE,10               ELSE BACK UP TO PREVIOUS DECADE              
         AR    RE,R0                                                            
         STC   RE,HALF             CALCULATED YEAR OF BILL                      
*                                                                               
         MVC   HALF+1(1),BKEYMBIL                                               
         NI    HALF+1,X'0F'                                                     
*                                                                               
         CLC   HALF,BQSTART                                                     
         BL    ROB30                                                            
         CLC   HALF,BQEND                                                       
         BH    ROB30                                                            
*                                                                               
         CLC   BKEYYSRV(2),STRTMOS MOS FILTER(S)                                
         BL    ROB30                                                            
         CLC   BKEYYSRV(2),ENDMOS                                               
         BH    ROB30                                                            
         DROP  R2                                                               
*                                  HAVE GOOD KEY                                
*                                  PASS DATA TO SORT                            
         GOTO1 GETBILL                                                          
         L     R7,ADBILL                                                        
         USING BILLRECD,R7                                                      
*                                                                               
         CLC   BDATE,QSTART                                                     
         BL    ROB30                                                            
         CLC   BDATE,QEND                                                       
         BH    ROB30                                                            
         TM    BILSTAT,X'20'       SKIP AOR BILLS                               
         BNZ   ROB30               SKIP NET (USED FOR SOMETHING ELSE)           
*                                                                               
         XC    SORTREC,SORTREC                                                  
         LA    R4,SORTREC                                                       
         USING INVD,R4                                                          
*                                                                               
         MVC   INVINVNO,BINVNO     GET 6 DIGIT INVOICE NUMBER                   
*                                                                               
         MVC   INVAGMD,BKEYAM                                                   
         MVC   INVCLT,BKEYCLT                                                   
         MVC   INVPRD,BKEYPRD                                                   
         MVC   INVEST,BKEYEST                                                   
         ZAP   INVHNET,BNETP                                                    
*                                                                               
         MVI   INVTYPE,HEADER_RECORD                                            
         GOTO1 DATCON,DMCB,BQDATE,(20,INVINVDT)                                 
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(20,INVDUEDT)                           
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         CLI   QOPT2,C'Y'                                                       
         BNE   ROB30                                                            
         GOTO1 PRNTBL,DMCB,=C'PUT HEADER DETAILS',SORTREC,C'DUMP',     +        
               INVRECLQ,=C'1D'                                                  
*                                                                               
         B     ROB30                                                            
*                                                                               
ROBX     DS    0H                                                               
         J     EXIT                                                             
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'REPRT - CREATE TAPE RECS AND PRINT REPORT'                      
REPRT    NMOD1 0,REPRT                                                          
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         ZAP   CURHAMT,=P'0'       CURRENT HEADER AMOUNT                        
         ZAP   DSUBTOT,=P'0'       DETAIL SUBTOTAL                              
         XC    SVSORTR,SVSORTR                                                  
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         XC    SORTREC,SORTREC                                                  
         LA    R4,SORTREC                                                       
         USING INVD,R4                                                          
*                                                                               
REPRT10  GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R3,15,4(R1)         A(SORTREC)                                   
         BZ    REPRT50                                                          
*                                                                               
         LHI   R1,L'SORTREC                                                     
         MOVE  (SORTREC,(R1)),0(R3)                                             
*                                                                               
         CLI   QOPT2,C'Y'                                                       
         BNE   REPRT20                                                          
         GOTO1 PRNTBL,DMCB,=C'GET SORT RECORD',SORTREC,C'DUMP',        +        
               INVRECLQ,=C'1D'                                                  
*                                                                               
REPRT20  DS    0H                                                               
         CLI   INVTYPE,HEADER_RECORD                                            
         BNE   REPRT40                                                          
*                                  INVOICE HEADER                               
         OC    SVSORTR,SVSORTR     ANYTHING IN SAVED BUFF?                      
         BZ    REPRT30                                                          
         BAS   RE,ENDINV                                                        
*                                                                               
         CLI   SVSORTR+18,DETAIL_RECORD                                         
         BE    REPRT30                                                          
         MVC   P,SPACES                                                         
         MVC   P(50),=C'ERROR: DETAIL RECORD DOES NOT FOLLOW HEADER REC+        
               ORD'                                                             
         GOTO1 REPORT                                                           
*                                                                               
REPRT30  DS    0H                                                               
*                                                                               
         XC    TSTINVC,TSTINVC     CLEAR INV NUMBER SAVE FIELD                  
         CP    INVHNET,=P'0'       SEE IF CREDIT INVOICE                        
         BNL   REPRT30P                                                         
         MVC   TSTINVC,INVINVNO    TELLS ME TO IGNORE DETAILS                   
         MVC   SVSORTR,INVD        SAVE HEADER                                  
         B     REPRT10             SKIP THIS INVOICE                            
*                                                                               
REPRT30P DS    0H                                                               
         LA    R8,OUTREC                                                        
         USING CGDATA,R8                                                        
         BAS   RE,HDROUT           DO HEADER OUTPUT                             
*                                                                               
         MVC   P,SPACES                                                         
*                                                                               
         MVC   PHVENN,CGHVENN                                                   
         MVC   PHINUM,CGHINUM                                                   
         MVC   PHOPID,CGHOPID                                                   
         MVC   PHINCL,CGHINCL                                                   
         MVC   PHCOMM,CGHCOMM                                                   
         MVC   PHINDT,CGHINDT                                                   
         MVC   PHPYTM,CGHPYTM                                                   
         MVC   PHSDTY,CGHSDTY                                                   
         MVC   PHDUDT,CGHDUDT                                                   
         MVC   PHSIGN,CGHSIGN                                                   
         MVC   PHGRAM,CGHGRAM                                                   
         MVC   PHCUCD,CGHCUCD                                                   
         MVC   PHLGID,CGHLGID                                                   
         MVC   PHENTY,CGHENTY                                                   
         MVC   PHRETY,CGHRETY                                                   
         DROP  R8                                                               
*                                                                               
         MVC   SVSORTR,INVD        SAVE HEADER                                  
         L     RE,THCOUNT          COUNT TOTAL NUMB OF HEADER RECS              
         AHI   RE,1                                                             
         ST    RE,THCOUNT                                                       
         L     RE,TRCOUNT          COUNT TOTAL NUMB OF RECS                     
         AHI   RE,1                                                             
         ST    RE,TRCOUNT                                                       
         GOTO1 REPORT              PRINT OUT HEADER RECORD                      
         B     REPRT10                                                          
*                                                                               
REPRT40  DS    0X                  DETAIL LINE                                  
         OC    TSTINVC,TSTINVC     NON-ZERO MEANS SKIP THESE DETAILS            
         BZ    REPRT40P                                                         
         MVC   SVSORTR,INVD                                                     
         B     REPRT10             SKIP                                         
*                                                                               
REPRT40P BAS   RE,DETOUT           DO DETAIL OUTPUT                             
         LA    R8,OUTREC                                                        
         USING CGDATA,R8                                                        
*                                                                               
         MVC   PDVENN,CGDVENN                                                   
         MVC   PDINUM,CGDINUM                                                   
         MVC   PDSBAR,CGDSBAR                                                   
         MVC   PDSEP1,CGDSEP1                                                   
         MVC   PDSGAC,CGDSGAC                                                   
         MVC   PDSCCT,CGDSCCT                                                   
         MVC   PDSCCD,CGDSCCD                                                   
         MVC   PDDESP,CGDDESP                                                   
         MVC   PDSIGN,CGDSIGN                                                   
         MVC   PDDISL,CGDDISL                                                   
         MVC   PDSRIN,CGDSRIN                                                   
         MVC   PDMRKT,CGDSPMKT                                                  
         MVC   PDSTAT,CGDSPSTA                                                  
         MVC   PDSPTS,CGD#SPTS                                                  
         MVC   PDRETY,CGDRETY                                                   
         DROP  R8                                                               
*                                                                               
         MVC   SVSORTR,INVD                                                     
         L     RE,TDCOUNT          COUNT TOTAL NUMB OF DETAIL RECS              
         AHI   RE,1                                                             
         ST    RE,TDCOUNT                                                       
         L     RE,TRCOUNT          COUNT TOTAL NUMB OF RECS                     
         AHI   RE,1                                                             
         ST    RE,TRCOUNT                                                       
         GOTO1 REPORT                                                           
*                                                                               
         B     REPRT10                                                          
*                                  EOF SORTER - NO MORE RECORDS                 
REPRT50  DS    0X                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
         BAS   RE,ENDINV                                                        
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
ENDINV   NTR1                                                                   
         CP    CURHAMT,DSUBTOT                                                  
         BNE   EINV10                                                           
*                                                                               
         OC    TSTINVC,TSTINVC              SKIPPED INVOICE?                    
         BNZ   EINV05                                                           
         MVC   P,SPACES                                                         
         GOTO1 REPORT                                                           
         MVC   P(23),=C'*** DETAIL SUBTOTAL ***'                                
         EDIT  DSUBTOT,(13,P+30),MINUS=YES                                      
         GOTO1 REPORT                                                           
*                                                                               
EINV05   DS    0H                                                               
         XC    TSTINVC,TSTINVC      CLEAR SKIPPED INVOICE INDICATOR             
         ZAP   DSUBTOT,=P'0'                                                    
         ZAP   CURHAMT,=P'0'                                                    
         B     EINVXX               JUST SET FORCEHED FOR NEXT                  
*                                                                               
EINV10   DS    0H                                                               
         MVC   P(52),=C'ERROR: HEADER AMOUNT AND TOTAL DETAIL AMOUNT UN+        
               EQUAL'                                                           
         GOTO1 REPORT                                                           
         MVC   P(9),=C'HEADER = '                                               
         EDIT  CURHAMT,(16,P+9),ZERO=NOBLANK                                    
         GOTO1 REPORT                                                           
         MVC   P(9),=C'DETAIL = '                                               
         EDIT  DSUBTOT,(16,P+9),ZERO=NOBLANK                                    
         GOTO1 REPORT                                                           
         MVC   P(18),=C'**OUT OF BALANCE**'                                     
         MVI   ERROR,C'Y'                                                       
*                                                                               
EINVX    DS    0H                                                               
         GOTO1 REPORT                                                           
EINVXX   MVI   FORCEHED,C'Y'                                                    
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
HDROUT   NTR1                                                                   
*                                                                               
         LA    R3,OUTREC                                                        
         USING CGDATA,R3                                                        
*                                                                               
         LA    RE,CGDATA           CLEAR RECORD                                 
         LHI   RF,L'CGDATA                                                      
         SR    R0,R0                                                            
         L     R1,=X'40000000'     PAD CHAR IS SPACE                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   CGHRETY,HEADER_RECORD                                            
         MVC   CGHVENN,=C'1252071'                                              
         MVC   CGHINUM(1),QMED                                                  
         MVI   CGHINUM+1,C'-'                                                   
         MVI   CGHINUM+4,C'-'                                                   
         MVC   CGHINUM+2(2),INVINVNO                                            
         MVC   CGHINUM+5(4),INVINVNO+2                                          
         MVC   CGHOPID,=C'Y&&R'                                                 
         MVI   CGHINCL,C'I'                                                     
         MVC   CGHINDT(4),INVINVDT+4                                            
         MVC   CGHINDT+4(4),INVINVDT                                            
         MVC   CGHPYTM,=C'0030'                                                 
         MVC   CGHSDTY,=C'RN'                                                   
         MVC   CGHDUDT(4),INVDUEDT+4                                            
         MVC   CGHDUDT+4(4),INVDUEDT                                            
         MVI   CGHSIGN,C'+'                                                     
         MVI   CGHGRAM,C'0'                                                     
         EDIT  INVHNET,(17,CGHGRAM+1),FILL=0,ZERO=NOBLANK                       
         MVC   CGHCUCD(3),=C'USD'                                               
         MVC   CGHLGID,=C'US01'                                                 
         MVC   CGHENTY(3),=C'101'                                               
*                                                                               
         ZAP   CURHAMT,INVHNET     CURRENT HEADER AMOUNT                        
         BNM   *+14                CHECK FOR NEGATIVE GROSS AMOUNT              
         MVI   CGHINCL,C'C'                                                     
         MVC   CGHSDTY,=C'KG'                                                   
*                                                                               
         CLI   CGHINCL,C'C'        CREDIT INVOICE?                              
         BE    *+10                DON'T ADD TO TOTALS                          
*                                                                               
         AP    GTHNET,INVHNET      SUMMING UP HEADER NET AMOUNT                 
*                                                                               
*                                  GET EST DESC FIELDS                          
*                                                                               
         XC    KEY,KEY             NOTE- WONT HAVE TO RESET SEQ                 
         LA    R5,KEY                                                           
         USING ESTHDRD,R5                                                       
         L     RF,ADCLT                                                         
         MVC   EKEY,0(RF)                                                       
         MVC   EKEYPRD,INVPRD                                                   
         MVC   EKEYEST,INVEST                                                   
*                                                                               
         L     RF,ADEST                                                         
         CLC   KEY(13),0(RF)       TEST ALREADY HAVE ESTHDR                     
         BE    HDR10                                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETEST                                                           
*                                                                               
HDR10    DS    0H                                                               
         L     R5,ADEST                                                         
         MVC   CGHCOMM,EDESC       ESTIMATE DESCRIPTION                         
*                                                                               
         CLI   QOPT1,C'Y'          WRITE TAPE DATA FOR THIS REQUEST?            
         BNE   HDRX                                                             
         L     R1,=A(OUTFILE)                                                   
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         DROP  R5                                                               
*                                                                               
         DROP  R3                                                               
HDRX     DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
DETOUT   NTR1                                                                   
*                                                                               
         LA    R3,OUTREC                                                        
         USING CGDATA,R3                                                        
*                                                                               
         LA    RE,CGDATA           CLEAR RECORD                                 
         LHI   RF,L'CGDATA                                                      
         SR    R0,R0                                                            
         L     R1,=X'40000000'     PAD CHAR IS SPACE                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   CGDRETY,DETAIL_RECORD                                            
         MVC   CGDVENN,=C'1252071'                                              
         MVC   CGDINUM(1),QMED                                                  
         MVI   CGDINUM+1,C'-'                                                   
         MVI   CGDINUM+4,C'-'                                                   
         MVC   CGDINUM+2(2),INVINVNO                                            
         MVC   CGDINUM+5(4),INVINVNO+2                                          
         MVC   CGDSBAR,=C'101'                                                  
         MVI   CGDSEP1,C'-'                                                     
         MVC   CGDSCCD,=C'US01'    SAP COMP CODE HARD CODEDD                    
*                                                                               
         EDIT  INVEST,(3,CGDDESP),ALIGN=LEFT,FILL=0,ZERO=NOBLANK                
         MVI   CGDDESP+3,C'_'                                                   
         MVC   CGDDESP+4(2),INVMOS+4                                            
         MVI   CGDDESP+6,C'/'                                                   
         MVC   CGDDESP+7(4),INVMOS                                              
*                                                                               
*                                  GET PRD USER FIELDS                          
*                                                                               
         XC    KEY,KEY             NOTE- WONT HAVE TO RESET SEQ                 
         MVC   KEY+1(1),INVAGMD                                                 
         MVC   KEY+2(2),INVCLT                                                  
         MVC   KEY+4(3),INVPRD                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETPRD                                                           
*                                                                               
         L     R5,ADPRD                                                         
         USING PRDHDRD,R5                                                       
         MVC   CGDSCCT,PUSER1      PRODUCT USER FIELD                           
         DROP  R5                                                               
*                                                                               
*                                  GET EST USER FIELDS                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),INVAGMD                                                 
         MVC   KEY+2(2),INVCLT                                                  
         MVC   KEY+4(3),INVPRD                                                  
         MVC   KEY+7(1),INVEST                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETEST                                                           
*                                                                               
         L     R5,ADEST                                                         
         USING ESTHDRD,R5                                                       
         MVC   CGDSGAC,EUSER1      ESTIMATE USER FIELD                          
         CLC   CGDSGAC,=C'111052'  SPECIAL CASE...                              
         BNE   *+10                                                             
         MVC   CGDSCCT,=C'000000000'                                            
*                                                                               
         MVI   CGDSIGN,C'+'                                                     
         CP    INVDNET,=P'0'                                                    
         BNM   *+8                                                              
         MVI   CGDSIGN,C'-'                                                     
*                                                                               
         AP    DSUBTOT,INVDNET     DETAIL REC SUB-TOTAL                         
         AP    GTDNET,INVDNET      SUMMING UP DETAIL NET AMOUNT                 
*                                                                               
         EDIT  INVDNET,(13,CGDDISL),FILL=0,ZERO=NOBLANK                         
         MVC   CGDSRIN,=C'Y&&R'                                                 
         GOTO1 MSUNPK,DMCB,INVMKSTA,CGDSPMKT,CGDSPSTA                           
         EDIT  INVSPTS,CGD#SPTS,FILL=0,ZERO=NOBLANK                             
*                                                                               
         CLI   QOPT1,C'Y'          WRITE TAPE DATA FOR THIS REQUEST?            
         BNE   DETX                                                             
         L     R1,=A(OUTFILE)                                                   
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         DROP  R5,R3                                                            
*                                                                               
DETX     DS    0H                                                               
         J     EXIT                                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
BILWRKD  DSECT                                                                  
         DS    0F                                                               
SORTREC  DS    XL(INVRECLQ)                                                     
         DS    0F                                                               
SVSORTR  DS    XL(INVRECLQ)                                                     
*                                                                               
KPRD     DS    X                                                                
ELCODE   DS    X                                                                
MOSFILT  DS    0XL4                                                             
STRTMOS  DS    XL2                                                              
ENDMOS   DS    XL2                                                              
DECADE   DS    X                                                                
YEARDIG  DS    X                                                                
*                                                                               
ERROR    DS    C                                                                
TAPEOPEN DS    C                                                                
*                                                                               
PROFB1   DS    CL16                                                             
PROFB1X  DS    CL16                                                             
*                                                                               
TSTINVC  DS    CL6                 INVOICE NUMBER FOR CREDITS - FROM            
*                                  HEADER'S INVINVNO                            
*                                                                               
TDCOUNT  DS    F                   TOTAL DETAIL RECORD COUNTER                  
THCOUNT  DS    F                   TOTAL HEADER RECORD COUNTER                  
TRCOUNT  DS    F                   TOTAL DETAIL & HEADER REC COUNTER            
*                                                                               
CURHAMT  DS    PL6                 CURRENT HEADER AMOUNT                        
DSUBTOT  DS    PL6                 DETAIL SUBTOTAL                              
GTHNET   DS    PL6                 GRAND TOTAL FOR HEADER NET AMOUNT            
GTDNET   DS    PL6                 GRAND TOTAL FOR DETAIL NET AMOUNT            
*                                                                               
OUTREC   DS    XL256               RECORD LENGTH IS 256                         
*                                                                               
STABREC  DS    2000C                                                            
         EJECT                                                                  
PLINED   DSECT                     PRINT LINE DSECT                             
PLSTART  DS    0X                                                               
*                                                                               
PHVENN   DS    CL7                 HEAD RECORD                                  
         DS    CL1                                                              
PHINUM   DS    CL10                                                             
         DS    CL1                                                              
PHOPID   DS    CL3                                                              
         DS    CL1                                                              
PHINCL   DS    CL1                                                              
         DS    CL1                                                              
PHCOMM   DS    CL12                                                             
         DS    CL1                                                              
PHINDT   DS    CL8                                                              
         DS    CL1                                                              
PHPYTM   DS    CL4                                                              
         DS    CL1                                                              
PHSDTY   DS    CL2                                                              
         DS    CL1                                                              
PHDUDT   DS    CL8                                                              
         DS    CL1                                                              
PHSIGN   DS    CL1                                                              
PHGRAM   DS    CL18                                                             
         DS    CL1                                                              
PHCUCD   DS    CL4                                                              
         DS    CL1                                                              
PHLGID   DS    CL4                                                              
         DS    CL1                                                              
PHENTY   DS    CL4                                                              
         DS    CL1                                                              
PHRETY   DS    CL1                                                              
*                                                                               
         ORG   PLSTART             DETAIL RECORD                                
*                                                                               
PDVENN   DS    CL7                                                              
         DS    CL1                                                              
PDINUM   DS    CL10                                                             
         DS    CL1                                                              
PDSBAR   DS    CL3                                                              
PDSEP1   DS    CL1                                                              
PDSGAC   DS    CL6                                                              
PDSCCT   DS    CL9                                                              
PDSCCD   DS    CL4                                                              
         DS    CL1                                                              
PDDESP   DS    CL18                                                             
         DS    CL1                                                              
PDSIGN   DS    CL1                                                              
PDDISL   DS    CL13                                                             
         DS    CL1                                                              
PDSRIN   DS    CL3                                                              
         DS    CL1                                                              
*                                                                               
PDMRKT   DS    CL4                                                              
         DS    CL1                                                              
PDSTAT   DS    CL5                                                              
         DS    CL1                                                              
PDSPTS   DS    CL5                                                              
         DS    CL1                                                              
PDRETY   DS    CL1                                                              
*                                                                               
         ORG   PLSTART             SUMMARY RECORD                               
PTREID   DS    CL20                                                             
         DS    CL1                                                              
PTCRDT   DS    CL8                                                              
         DS    CL1                                                              
PTCRTM   DS    CL6                                                              
         DS    CL1                                                              
PTTRCT   DS    CL10                                                             
         DS    CL1                                                              
PTHRCT   DS    CL10                                                             
         DS    CL1                                                              
PTGRAM   DS    CL19                                                             
         DS    CL1                                                              
PTDRCT   DS    CL10                                                             
         DS    CL1                                                              
PTDIAM   DS    CL19                                                             
         DS    CL1                                                              
PTRETY   DS    CL1                                                              
         EJECT                                                                  
*                                  DSECT FOR TABLE ENTRY                        
INVD     DSECT                                                                  
INVINVNO DS    CL6                 INVOICE NUMBER                               
INVAGMD  DS    XL1                 AGY/MED                                      
INVCLT   DS    XL2                 CLIENT                                       
INVPRD   DS    CL3                 PRODUCT                                      
INVEST   DS    XL1                 ESTIMATE                                     
INVMKSTA DS    0XL5                                                             
INVMKT   DS    XL2                 MARKET                                       
INVSTA   DS    XL3                 STATION                                      
INVTYPE  DS    CL1                 RECORD TYPE                                  
HEADER_RECORD  EQU C'1'                                                         
DETAIL_RECORD  EQU C'2'                                                         
SUMMARY_RECORD EQU C'9'                                                         
INVKEYLQ EQU   *-INVD                                                           
INVINVDT DS    CL8                 INVOICE DATE                                 
INVDUEDT DS    CL8                 DUE DATE                                     
INVHNET  DS    PL6                 HEADER RECORD NET AMOUNT                     
INVDNET  DS    PL6                 DETAIL RECORD NET AMOUNT                     
INVSPTS  DS    HL2                 NUMBER OF SPOTS                              
INVMOS   DS    CL8                 MONTH OF SERVICE (DETAIL REC ONLY)           
INVRECLQ EQU   *-INVD                                                           
         EJECT                                                                  
       ++INCLUDE DDCOLGATD         COLGATE RECORD DSECTS                        
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
         ORG   QAREA+49                                                         
QSTRTMOS DS    CL4                 MONTH OF SERVICE START                       
QENDMOS  DS    CL4                 MONTH OF SERVICE END                         
         EJECT                                                                  
       ++INCLUDE DDGETPROFD                                                     
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPREPC002 02/08/07'                                      
         END                                                                    
