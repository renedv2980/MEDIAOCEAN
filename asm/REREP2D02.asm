*          DATA SET REREP2D02  AT LEVEL 092 AS OF 11/29/00                      
*PHASE RE2D02C,*                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE REGENBUC                                                               
*INCLUDE REGENTL2                                                               
*INCLUDE DATCON                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE RECUP                                                                  
         PRINT NOGEN                                                            
         TITLE 'REREP2D02 - RE2D02 - DARE SPOT COUNT REPORT'                    
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP2D02 --- DARE SPOT COUNT REPORT                       *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 21APR97 SKU PLOP! HONEY, CAN YOU PICK THAT UP??                   *           
*                                                                   *           
* JAN23/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
RE2D02   CSECT                                                                  
         NMOD1 STOREX-STORED,**RE2D02,R7,RR=R5                                  
         USING STORED,RC                                                        
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     R9,FILEC                                                         
         USING FILED,R9                                                         
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    DOCONT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
NO       LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
EXIT     XMOD1                                                                  
         EJECT                                                                  
**********************************************************************          
* DOCONT                                                                        
*                                                                               
**********************************************************************          
DOCONT   DS    0H                                                               
         XC    CSTATION,CSTATION                                                
         XC    CADV,CADV                                                        
         XC    CAGENCY,CAGENCY                                                  
         XC    TOTCTR,TOTCTR                                                    
         XC    STACTR,STACTR                                                    
         XC    ADVCTR,ADVCTR                                                    
         XC    AGYCTR,AGYCTR                                                    
         XC    TOTSPT,TOTSPT                                                    
         XC    STASPT,STASPT                                                    
         XC    ADVSPT,ADVSPT                                                    
         XC    AGYSPT,AGYSPT                                                    
*                                                                               
         MVI   RCSUBPRG,0                                                       
*                                                                               
         XCEF  BUCKETS,400                                                      
         XCEF  TBUCKETS,400                                                     
         ZAP   TOTAL$,=P'0'        TOTAL DOLLARS                                
*                                                                               
         ZAP   DUB,=P'0'                                                        
         PACK  DUB,QSTART(2)                                                    
         CVB   R0,DUB                                                           
         STC   R0,STARTYM                                                       
*                                                                               
         ZAP   DUB,=P'0'                                                        
         PACK  DUB,QSTART+2(2)                                                  
         CVB   R0,DUB                                                           
         STC   R0,STARTYM+1                                                     
*                                                                               
         ZAP   DUB,=P'0'                                                        
         PACK  DUB,QEND(2)                                                      
         CVB   R0,DUB                                                           
         STC   R0,ENDYM                                                         
*                                                                               
         ZAP   DUB,=P'0'                                                        
         PACK  DUB,QEND+2(2)                                                    
         CVB   R0,DUB                                                           
         STC   R0,ENDYM+1                                                       
*                                                                               
         ZIC   RE,STARTYM+1        MONTH                                        
         MH    RE,=H'3'                                                         
         LA    RE,MONTHTAB-3(RE)                                                
         MVC   HEAD3+50(3),0(RE)                                                
         MVI   HEAD3+53,C'/'                                                    
         EDIT  (1,STARTYM),(2,HEAD3+54)                                         
         MVI   HEAD3+57,C'-'                                                    
         ZIC   RE,ENDYM+1          MONTH                                        
         MH    RE,=H'3'                                                         
         LA    RE,MONTHTAB-3(RE)                                                
         MVC   HEAD3+59(3),0(RE)                                                
         MVI   HEAD3+62,C'/'                                                    
         EDIT  (1,ENDYM),(2,HEAD3+63)                                           
         MVC   DATEHDR,HEAD3+50                                                 
         GOTO1 REPORT                                                           
*                                                                               
* PROCESS CONTRACTS                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING RCONDATP,R6                                                      
         XC    KEY,KEY             READ STATION RECORD                          
         MVI   RCONDATP,X'BD'                                                   
         MVC   RCONDARP,QREP                                                    
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'        REC NOT FOUND?                               
         BZ    DOCONT20                                                         
         DC    H'0'                                                             
*                                                                               
DOCONT10 DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'        REC NOT FOUND?                               
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DOCONT20 DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   DOCONTX                                                          
         GOTO1 =V(DATCON),DMCB,(2,RCONDAFS),(0,WORK)                            
         GOTO1 =V(GETBROAD),DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                   
         MVC   BFLTSTR,WORK+12                                                  
*                                                                               
         GOTO1 =V(DATCON),DMCB,(2,RCONDAFE),(0,WORK)                            
         GOTO1 =V(GETBROAD),DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                   
         MVC   BFLTEND,WORK+12                                                  
         DROP  R6                                                               
*                                                                               
         CLC   BFLTEND(4),QSTART                                                
         BL    DOCONT10                                                         
         CLC   BFLTSTR(4),QEND                                                  
         BH    DOCONT10                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,RCONREC,DMWORK                
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
* FILTER ON GROUP/SUBGROUP                                                      
*                                                                               
         CLC   QGROUP,RCONKGRP                                                  
         BNE   DOCONT10                                                         
         CLI   QSBGROUP,C' '                                                    
         BE    DOCONT30                                                         
         CLC   QSBGROUP,RCONKGRP+1                                              
         BNE   DOCONT10                                                         
*                                                                               
DOCONT30 DS    0H                                                               
         CLC   QSTATION,SPACES                                                  
         BE    DOCONT40                                                         
         CLC   QSTATION,RCONKSTA                                                
         BNE   DOCONT10                                                         
*                                                                               
DOCONT40 DS    0H                                                               
         CLC   QOFFICE,SPACES                                                   
         BE    DOCONT50                                                         
         CLC   QOFFICE,RCONKOFF                                                 
         BNE   DOCONT10                                                         
*                                                                               
DOCONT50 DS    0H                                                               
         CLC   QAGENCY,SPACES                                                   
         BE    DOCONT60                                                         
         CLC   QAGENCY,RCONKAGY                                                 
         BNE   DOCONT10                                                         
         CLC   QAGYOFF,SPACES                                                   
         BE    DOCONT60                                                         
         CLC   QAGYOFF,RCONKAOF                                                 
         BNE   DOCONT10                                                         
*                                                                               
DOCONT60 DS    0H                                                               
         CLC   QADV,SPACES                                                      
         BE    DOCONT70                                                         
         CLC   QADV,RCONKADV                                                    
         BNE   DOCONT10                                                         
*                                                                               
DOCONT70 DS    0H                                                               
         CLC   QPRODUCT,SPACES                                                  
         BE    DOCONT80                                                         
         CLC   QPRODUCT,RCONPRD                                                 
         BNE   DOCONT10                                                         
*                                                                               
DOCONT80 DS    0H                                                               
         CLC   QCATY,SPACES                                                     
         BE    DOCONT82                                                         
         CLC   QCATY,RCONCTGY                                                   
         BNE   DOCONT10                                                         
*                                                                               
DOCONT82 DS    0H                                                               
         CLC   QCONTYPE,SPACES                                                  
         BE    DOCONT90                                                         
         CLC   QCONTYPE,RCONTYPE                                                
         BNE   DOCONT10                                                         
*                                                                               
DOCONT90 DS    0H                                                               
         MVC   HEAD3+50(L'DATEHDR),DATEHDR                                      
*                                                                               
         OC    CSTATION,CSTATION                                                
         BNZ   DOCON100                                                         
         MVC   CSTATION,RCONKSTA                                                
         MVC   CADV,RCONKADV                                                    
         MVC   CAGENCY,RCONKAGY                                                 
         MVC   P(4),CAGENCY                                                     
         CLC   CAGENCY+4(2),SPACES                                              
         BE    DOCONT95                                                         
         LA    RE,P+3                                                           
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C'-'                                                       
         MVC   2(2,RE),CAGENCY+4                                                
DOCONT95 MVC   P+10(4),CADV                                                     
         MVC   P+18(5),CSTATION                                                 
         ZAP   ADVTOT$,=P'0'       TOTAL DOLLARS                                
         ZAP   AGYTOT$,=P'0'       TOTAL DOLLARS                                
         B     DOCON150                                                         
*                                                                               
DOCON100 DS    0H                                                               
         CLC   CAGENCY,RCONKAGY                                                 
         BE    DOCON110                                                         
         BAS   RE,TOTAL                                                         
         BAS   RE,ADVTOTAL                                                      
         BAS   RE,AGYTOTAL                                                      
         XC    AGYSPT,AGYSPT                                                    
         ZAP   AGYTOT$,=P'0'       TOTAL DOLLARS                                
         XC    ADVSPT,ADVSPT                                                    
         ZAP   ADVTOT$,=P'0'       TOTAL DOLLARS                                
         MVC   CAGENCY,RCONKAGY                                                 
         MVC   CADV,RCONKADV                                                    
         MVC   CSTATION,RCONKSTA                                                
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   P(4),CAGENCY                                                     
         CLC   CAGENCY+4(2),SPACES                                              
         BE    DOCON105                                                         
         LA    RE,P+3                                                           
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C'-'                                                       
         MVC   2(2,RE),CAGENCY+4                                                
DOCON105 MVC   P+10(4),CADV                                                     
         MVC   P+18(5),CSTATION                                                 
         B     DOCON150                                                         
*                                                                               
DOCON110 DS    0H                                                               
         CLC   CADV,RCONKADV                                                    
         BE    DOCON120                                                         
         BAS   RE,TOTAL                                                         
         BAS   RE,ADVTOTAL                                                      
         XC    ADVSPT,ADVSPT                                                    
         ZAP   ADVTOT$,=P'0'       TOTAL DOLLARS                                
         MVC   CADV,RCONKADV                                                    
         MVC   CSTATION,RCONKSTA                                                
         CLC   CAGENCY,RCONKAGY                                                 
         BE    DOCON115                                                         
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   P(4),CAGENCY                                                     
         CLC   CAGENCY+4(2),SPACES                                              
         BE    DOCON115                                                         
         LA    RE,P+3                                                           
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C'-'                                                       
         MVC   2(2,RE),CAGENCY+4                                                
DOCON115 MVC   P+10(4),CADV                                                     
         MVC   P+18(5),CSTATION                                                 
         B     DOCON150                                                         
*                                                                               
DOCON120 DS    0H                                                               
         CLC   CSTATION,RCONKSTA                                                
         BE    DOCON150                                                         
         BAS   RE,TOTAL                                                         
         MVC   CSTATION,RCONKSTA                                                
         CLC   CAGENCY,RCONKAGY                                                 
         BE    DOCON130                                                         
         MVC   P,SPACES            CLEAR OUT PRINT LINE                         
         MVC   P(4),CAGENCY                                                     
         CLC   CAGENCY+4(2),SPACES                                              
         BE    DOCON130                                                         
         MVI   P+4,C'-'                                                         
         MVC   P+5(2),CAGENCY+4                                                 
DOCON130 CLC   CADV,RCONKADV                                                    
         BE    *+10                                                             
         MVC   P+10(4),CADV                                                     
         MVC   P+18(5),CSTATION                                                 
*                                                                               
DOCON150 DS    0H                                                               
         L     RF,STACTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,STACTR                                                        
         L     RF,ADVCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ADVCTR                                                        
         L     RF,AGYCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,AGYCTR                                                        
         L     RF,TOTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,TOTCTR                                                        
*                                                                               
         BAS   RE,DOBUY                                                         
*                                                                               
         B     DOCONT10                                                         
*                                                                               
DOCONTX  DS    0H                                                               
         BAS   RE,TOTAL                                                         
         BAS   RE,ADVTOTAL                                                      
         BAS   RE,AGYTOTAL                                                      
         B     GTOTAL                                                           
         EJECT                                                                  
**********************************************************************          
* DOBUY -                                                                       
*                                                                               
**********************************************************************          
DOBUY    NTR1                                                                   
*        GOTO1 =V(HEXOUT),DMCB,RCONKCON,P,4                                     
*        GOTO1 REPORT                                                           
*                                                                               
         MVC   SVBDKEY,KEY                                                      
         XC    KEY,KEY                 BUILD BUYREC KEY                         
         MVI   KEY,X'0B'                                                        
         MVC   KEY+16(2),RREPKREP                                               
         MVC   FULL,RCONKCON           GET 9'S COMP REVERSED                    
         L     R0,=X'99999999'                                                  
         S     R0,FULL                                                          
         STCM  R0,15,FULL                                                       
         PACK  KEY+18(1),FULL+3(1)                                              
         PACK  KEY+19(1),FULL+2(1)                                              
         PACK  KEY+20(1),FULL+1(1)                                              
         PACK  KEY+21(1),FULL+0(1)                                              
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
*                                                                               
DOBUY10  DS    0H                                                               
         CLC   KEY(22),KEYSAVE                                                  
         BNE   DOBUYX              GOOD - CONTINUE ON                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',GETREC),REPFILE,KEY+28,RBUYREC,     X        
               DMWORK                                                           
*                                                                               
         MVC   WORK(4),GETBROAD                                                 
         MVC   WORK+4(4),GETDAY                                                 
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),DATCON                                                
         GOTO1 =V(REGENBUC),DMCB,RBUYREC,BLOCK,WORK                             
*                                                                               
* ADD BUCKETS TO RUNNING BUCKETS                                                
*                                                                               
         CLC   BLOCK(2),=H'2'      NONE?                                        
         BE    DOBUY80                                                          
*                                                                               
         LA    R8,BLOCK+2          1ST BUY BUCKET                               
         LH    R3,BLOCK                                                         
         LA    R3,BLOCK-1(R3)                                                   
*                                                                               
DOBUY20  LA    R6,BUCKETS-12       1ST BUCKET-14                                
         LH    R5,BUCKETS                                                       
         LA    R5,BUCKETS-1(R5)                                                 
         LA    R4,14                                                            
*                                                                               
DOBUY30  BXLE  R6,R4,DOBUY60       NEXT TOTAL BUCKET                            
*                                                                               
DOBUY40  XC    4(2,R8),4(R8)                                                    
         GOTO1 =V(RECUP),DMCB,(X'FF',BUCKETS),(R8),(R6)                         
*                                                                               
DOBUY50  LA    R8,14(R8)           INCREMENT INDEX                              
         CR    R8,R3                                                            
         BNH   DOBUY20                                                          
         B     DOBUY80                                                          
*                                                                               
DOBUY60  CLC   2(2,R8),2(R6)       SAME YR-MONTH?                               
         BH    DOBUY30                                                          
         BL    DOBUY40                                                          
*                                                                               
* ADD BUY BUCKET TO TOTAL BUCKET                                                
* ADD DOLLARS AND SPOTS                                                         
*                                                                               
         MVC   DUB(8),6(R8)        $ AND SPOTS                                  
         LM    RE,RF,DUB                                                        
         MVC   DUB(8),6(R6)                                                     
         A     RE,DUB                                                           
         A     RF,DUB+4                                                         
         STM   RE,RF,DUB                                                        
         MVC   6(8,R6),DUB                                                      
*                                                                               
         B     DOBUY50                                                          
*                                                                               
DOBUY80  DS    0H                                                               
         MVC   WORK(4),GETBROAD                                                 
         MVC   WORK+4(4),GETDAY                                                 
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),DATCON                                                
         GOTO1 =V(REGENBUC),DMCB,RBUYREC,BLOCK,WORK                             
*                                                                               
* ADD BUCKETS TO RUNNING BUCKETS                                                
* WE'RE DOING THIS AGAIN TO GET GRAND TOTALS LATER                              
*                                                                               
         CLC   BLOCK(2),=H'2'      NONE?                                        
         BE    DOBUY200                                                         
*                                                                               
         LA    R8,BLOCK+2          1ST BUY BUCKET                               
         LH    R3,BLOCK                                                         
         LA    R3,BLOCK-1(R3)                                                   
*                                                                               
DOBUY120 LA    R6,TBUCKETS-12       1ST BUCKET-14                               
         LH    R5,TBUCKETS                                                      
         LA    R5,TBUCKETS-1(R5)                                                
         LA    R4,14                                                            
*                                                                               
DOBUY130 BXLE  R6,R4,DOBUY160      NEXT TOTAL BUCKET                            
*                                                                               
DOBUY140 XC    4(2,R8),4(R8)                                                    
         GOTO1 =V(RECUP),DMCB,(X'FF',TBUCKETS),(R8),(R6)                        
*                                                                               
DOBUY150 LA    R8,14(R8)           INCREMENT INDEX                              
         CR    R8,R3                                                            
         BNH   DOBUY120                                                         
         B     DOBUY200                                                         
*                                                                               
DOBUY160 CLC   2(2,R8),2(R6)       SAME YR-MONTH?                               
         BH    DOBUY130                                                         
         BL    DOBUY140                                                         
*                                                                               
* ADD BUY BUCKET TO TOTAL BUCKET                                                
* ADD DOLLARS AND SPOTS                                                         
*                                                                               
         MVC   DUB(8),6(R8)        $ AND SPOTS                                  
         LM    RE,RF,DUB                                                        
         MVC   DUB(8),6(R6)                                                     
         A     RE,DUB                                                           
         A     RF,DUB+4                                                         
         STM   RE,RF,DUB                                                        
         MVC   6(8,R6),DUB                                                      
*                                                                               
         B     DOBUY150                                                         
*                                                                               
DOBUY200 DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'        REC NOT FOUND?                               
         BZ    DOBUY10                                                          
         DC    H'0'                                                             
*                                                                               
DOBUYX   DS    0H                                                               
         MVC   KEY(27),SVBDKEY                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         TM    DMCB+8,X'FD'                                                     
         BZ    EXIT                                                             
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         EJECT                                                                  
**********************************************************************          
* TOTAL -                                                                       
*                                                                               
**********************************************************************          
TOTAL    NTR1                                                                   
         ZAP   CONTOT$,=P'0'       TOTAL DOLLARS                                
*                                                                               
         GOTO1 =V(REGENTL2),DMCB,BUCKETS,BLOCK                                  
         LA    R8,BLOCK                                                         
         ZIC   R4,DMCB+4                                                        
*                                                                               
         LA    R8,BUCKETS+2                                                     
         LA    R4,20                                                            
         SR    R3,R3               TOTAL SPOTS                                  
*                                                                               
* CONVERT MONTHS TO OUTPUT                                                      
*                                                                               
TOTAL20  DS    0H                                                               
         CLC   2(2,R8),STARTYM                                                  
         BL    TOTAL30                                                          
         CLC   2(2,R8),ENDYM                                                    
         BH    TOTAL40                                                          
*                                                                               
         OC    10(4,R8),10(R8)                                                  
         BZ    TOTAL30             SKIP IF NO SPOTS                             
*                                                                               
         ZIC   RE,3(R8)            MONTH                                        
         MH    RE,=H'3'                                                         
         LA    RE,MONTHTAB-3(RE)                                                
         MVC   P+30(3),0(RE)                                                    
*                                                                               
         MVI   P+33,C'/'                                                        
         EDIT  (1,2(R8)),(2,P+34)                                               
*                                                                               
         EDIT  (4,6(R8)),(14,P+45),2,COMMAS=YES                                 
         ZICM  RE,6(R8),4                                                       
         CVD   RE,DUB                                                           
         AP    CONTOT$,DUB                                                      
*                                                                               
         EDIT  (4,10(R8)),(10,P+65),COMMAS=YES                                  
         ZICM  RE,10(R8),4                                                      
         AR    R3,RE                                                            
*                                                                               
         BAS   RE,PRTLINE                                                       
*        GOTO1 REPORT                                                           
*                                                                               
TOTAL30  DS    0H                                                               
         LA    R8,14(R8)                                                        
         BCT   R4,TOTAL20                                                       
*                                                                               
TOTAL40  DS    0H                                                               
         LTR   R3,R3               DON'T PRINT TOTAL IF NO SPOTS                
         BZ    TOTALX                                                           
         BAS   RE,PRTLINE                                                       
*        GOTO1 REPORT                                                           
         MVC   P+30(11),=C'*STA TOTAL*'                                         
         EDIT  (P8,CONTOT$),(16,P+43),2,COMMAS=YES                              
         EDIT  (R3),(10,P+65),COMMAS=YES,ZERO=NOBLANK                           
         BAS   RE,PRTLINE                                                       
         BAS   RE,PRTLINE                                                       
*        GOTO1 REPORT                                                           
*        GOTO1 REPORT                                                           
*                                                                               
TOTALX   DS    0H                                                               
         XC    STACTR,STACTR                                                    
         L     RE,TOTSPT                                                        
         AR    RE,R3                                                            
         ST    RE,TOTSPT                                                        
         L     RE,ADVSPT                                                        
         AR    RE,R3                                                            
         ST    RE,ADVSPT                                                        
         L     RE,AGYSPT                                                        
         AR    RE,R3                                                            
         ST    RE,AGYSPT                                                        
         AP    ADVTOT$,CONTOT$                                                  
         AP    AGYTOT$,CONTOT$                                                  
         AP    TOTAL$,CONTOT$                                                   
         XCEF  BUCKETS,400                                                      
         B     EXIT                                                             
*                                                                               
MONTHTAB DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
*                                                                               
         EJECT                                                                  
**********************************************************************          
* ADV TOTAL -                                                                   
*                                                                               
**********************************************************************          
ADVTOTAL NTR1                                                                   
         OC    ADVSPT,ADVSPT                                                    
         BZ    ADVTOTX                                                          
         MVC   P,SPACES            CLEAR OUTPUT LINE                            
         MVC   P+30(11),=C'*ADV TOTAL*'                                         
         EDIT  (P8,ADVTOT$),(16,P+43),2,COMMAS=YES                              
         EDIT  ADVSPT,(10,P+65),COMMAS=YES,ZERO=NOBLANK                         
         BAS   RE,PRTLINE                                                       
         BAS   RE,PRTLINE                                                       
*        GOTO1 REPORT                                                           
*        GOTO1 REPORT                                                           
*                                                                               
ADVTOTX  DS    0H                                                               
         XC    ADVCTR,ADVCTR                                                    
         XC    STACTR,STACTR                                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* AGY TOTAL -                                                                   
*                                                                               
**********************************************************************          
AGYTOTAL NTR1                                                                   
         OC    AGYSPT,AGYSPT                                                    
         BZ    AGYTOTX                                                          
         MVC   P,SPACES            CLEAR OUTPUT LINE                            
         MVC   P+30(11),=C'*AGY TOTAL*'                                         
         EDIT  (P8,AGYTOT$),(16,P+43),2,COMMAS=YES                              
         EDIT  AGYSPT,(10,P+65),COMMAS=YES,ZERO=NOBLANK                         
         BAS   RE,PRTLINE                                                       
         BAS   RE,PRTLINE                                                       
*        GOTO1 REPORT                                                           
*        GOTO1 REPORT                                                           
*                                                                               
AGYTOTX  DS    0H                                                               
         XC    AGYCTR,AGYCTR                                                    
         XC    ADVCTR,ADVCTR                                                    
         XC    STACTR,STACTR                                                    
*        MVC   HEAD3+50(L'DATEHDR),DATEHDR                                      
         OC    AGYSPT,AGYSPT                                                    
         BZ    EXIT                                                             
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GRAND TOTAL -                                                                 
*                                                                               
**********************************************************************          
GTOTAL   DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         GOTO1 =V(REGENTL2),DMCB,TBUCKETS,BLOCK                                 
         LA    R8,BLOCK                                                         
         ZIC   R4,DMCB+4                                                        
*                                                                               
         LA    R8,TBUCKETS+2                                                    
         LA    R4,20                                                            
         SR    R3,R3               TOTAL SPOTS                                  
*                                                                               
* CONVERT MONTHS TO OUTPUT                                                      
*                                                                               
GTOTAL20 DS    0H                                                               
         CLC   2(2,R8),STARTYM                                                  
         BL    GTOTAL30                                                         
         CLC   2(2,R8),ENDYM                                                    
         BH    GTOTAL30                                                         
         ZIC   RE,3(R8)            MONTH                                        
         MH    RE,=H'3'                                                         
         LA    RE,MONTHTAB-3(RE)                                                
         MVC   P+30(3),0(RE)                                                    
*                                                                               
         MVI   P+33,C'/'                                                        
         EDIT  (1,2(R8)),(2,P+34)                                               
*                                                                               
         EDIT  (4,6(R8)),(14,P+45),2,COMMAS=YES                                 
         ZICM  RE,6(R8),4                                                       
         CVD   RE,DUB                                                           
         AP    CONTOT$,DUB                                                      
*                                                                               
         EDIT  (4,10(R8)),(10,P+65),COMMAS=YES                                  
         ZICM  RE,10(R8),4                                                      
         AR    R3,RE                                                            
*                                                                               
         BAS   RE,PRTLINE                                                       
*        GOTO1 REPORT                                                           
*                                                                               
GTOTAL30 DS    0H                                                               
         LA    R8,14(R8)                                                        
         BCT   R4,GTOTAL20                                                      
*                                                                               
         BAS   RE,PRTLINE                                                       
*        GOTO1 REPORT                                                           
         MVC   P+30(13),=C'*GRAND TOTAL*'                                       
         EDIT  (P8,TOTAL$),(16,P+43),2,COMMAS=YES                               
         EDIT  TOTSPT,(10,P+65),COMMAS=YES                                      
         BAS   RE,PRTLINE                                                       
         BAS   RE,PRTLINE                                                       
*        GOTO1 REPORT                                                           
*        GOTO1 REPORT                                                           
         MVC   P+30(13),=C'*# CONTRACTS*'                                       
         EDIT  TOTCTR,(14,P+45),COMMAS=YES                                      
         BAS   RE,PRTLINE                                                       
*        GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
**********************************************************************          
* PRINT                                                                         
*                                                                               
**********************************************************************          
PRTLINE  NTR1                                                                   
         MVC   HEAD3+50(L'DATEHDR),DATEHDR                                      
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
* STORAGE AREAS FOR BETWEEN I/O DATA                                            
*                                                                               
         SPACE 2                                                                
RELO     DS    A                                                                
TEMP     DS    6F                                                               
TOTCTR   DS    F                                                                
STACTR   DS    F                                                                
ADVCTR   DS    F                                                                
AGYCTR   DS    F                                                                
TOTSPT   DS    F                                                                
STASPT   DS    F                                                                
ADVSPT   DS    F                                                                
AGYSPT   DS    F                                                                
BUCKETS  DS    200H                                                             
TBUCKETS DS    200H                                                             
DAREFLAG DS    C                   Y/N                                          
STARTYM  DS    XL2                                                              
ENDYM    DS    XL2                                                              
BFLTSTR  DS    CL6                                                              
BFLTEND  DS    CL6                                                              
SVBDKEY  DS    XL27                                                             
CSTATION DS    CL5                                                              
CADV     DS    CL4                                                              
CAGENCY  DS    CL6                                                              
AGYTOT$  DS    PL8                                                              
ADVTOT$  DS    PL8                                                              
CONTOT$  DS    PL8                                                              
TOTAL$   DS    PL8                                                              
DATEHDR  DS    CL15                                                             
         EJECT                                                                  
STORED   DSECT                                                                  
ELCODE   DS    C                                                                
BUCKETYR DS    CL2                 BUCKET YEAR                                  
BUCKETMN DS    CL2                 BUCKET MONTH                                 
         DS    0D                                                               
BLOCK    DS    3000C                                                            
STOREX   EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'092REREP2D02 11/29/00'                                      
         END                                                                    
