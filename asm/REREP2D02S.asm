*          DATA SET REREP2D02S AT LEVEL 033 AS OF 06/02/97                      
*PHASE RE2D02A,*                                                                
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
         XC    CONCTR,CONCTR                                                    
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
* PROCESS CONTRACTS                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING RCONDATP,R6                                                      
         XC    KEY,KEY                      READ STATION RECORD                 
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
         CLC   KEY(19),KEYSAVE                                                  
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
         BE    DOCONT84                                                         
         CLC   QCONTYPE,RCONTYPE                                                
         BNE   DOCONT10                                                         
*                                                                               
DOCONT84 DS    0H                                                               
*&&DO                                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'18'                                                     
         BAS   RE,GETEL                                                         
         BNE   DOCONT90                                                         
*                                                                               
         USING RCONDVEL,R6                                                      
         CLC   QDEVCTYP,SPACES                                                  
         BE    DOCONT86                                                         
         CLC   QDEVCTYP,RCONDVCT                                                
         BNE   DOCONT10                                                         
*                                                                               
DOCONT86 DS    0H                                                               
         CLC   Q2DEVSP,SPACES                                                   
         BE    DOCONT90                                                         
         CLC   Q2DEVSP,RCONDVSP                                                 
         BNE   DOCONT10                                                         
         DROP  R6                                                               
*&&                                                                             
*                                                                               
DOCONT90 DS    0H                                                               
         L     RF,CONCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
*                                                                               
         BAS   RE,DOBUY                                                         
*                                                                               
         B     DOCONT10                                                         
*                                                                               
DOCONTX  DS    0H                                                               
         B     TOTAL                                                            
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
*        MVC   P(10),=C'FOUND BUY!'                                             
*        EDIT  (1,KEY+25),(3,P+12)                                              
*        GOTO1 REPORT                                                           
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
*        EDIT  (R6),(8,P)                                                       
*        EDIT  (R4),(8,P+10)                                                    
*        GOTO1 REPORT                                                           
*                                                                               
DOBUY40  XC    4(2,R8),4(R8)                                                    
*        MVC   P(6),=C'RECUP!'                                                  
*        GOTO1 REPORT                                                           
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
****                                                                            
*        EDIT  (1,2(R8)),(4,P+10)                                               
*        EDIT  (1,3(R8)),(4,P+13)                                               
*        EDIT  (4,6(R8)),(14,P+20),2,COMMAS=YES                                 
*        EDIT  (4,10(R8)),(10,P+35),COMMAS=YES                                  
*        GOTO1 REPORT                                                           
****                                                                            
         B     DOBUY50                                                          
*                                                                               
DOBUY80  DS    0H                                                               
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
TOTAL    DS    0H                                                               
         GOTO1 REPORT                                                           
         ZIC   RE,STARTYM+1        MONTH                                        
         MH    RE,=H'3'                                                         
         LA    RE,MONTHTAB-3(RE)                                                
         MVC   P+50(3),0(RE)                                                    
         MVI   P+53,C'/'                                                        
         EDIT  (1,STARTYM),(2,P+54)                                             
         MVI   P+57,C'-'                                                        
         ZIC   RE,ENDYM+1          MONTH                                        
         MH    RE,=H'3'                                                         
         LA    RE,MONTHTAB-3(RE)                                                
         MVC   P+59(3),0(RE)                                                    
         MVI   P+62,C'/'                                                        
         EDIT  (1,ENDYM),(2,P+63)                                               
*                                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+13(16),=C'TOTAL CONTRACT $'                                    
         MVC   P+32(13),=C'TOTAL # SPOTS'                                       
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 =V(REGENTL2),DMCB,BUCKETS,BLOCK                                  
         LA    R8,BLOCK                                                         
         ZIC   R4,DMCB+4                                                        
*                                                                               
*OTAL10  MVC   P(80),0(R8)      TOTAL LINE                                      
*        GOTO1 REPORT                                                           
*        LA    R8,80(R8)                                                        
*        BCT   R4,TOTAL10                                                       
*                                                                               
         LA    R8,BUCKETS+2                                                     
         LA    R4,20                                                            
         ZAP   TOTAL$,=P'0'        TOTAL DOLLARS                                
         SR    R3,R3               TOTAL SPOTS                                  
*                                                                               
* CONVERT MONTHS TO OUTPUT                                                      
*                                                                               
TOTAL20  DS    0H                                                               
         CLC   2(2,R8),STARTYM                                                  
         BL    TOTAL30                                                          
         CLC   2(2,R8),ENDYM                                                    
         BH    TOTAL40                                                          
         ZIC   RE,3(R8)            MONTH                                        
         MH    RE,=H'3'                                                         
         LA    RE,MONTHTAB-3(RE)                                                
         MVC   P(3),0(RE)                                                       
*                                                                               
         MVI   P+3,C'/'                                                         
         EDIT  (1,2(R8)),(2,P+4)                                                
*                                                                               
         EDIT  (4,6(R8)),(14,P+15),2,COMMAS=YES                                 
         ZICM  RE,6(R8),4                                                       
         CVD   RE,DUB                                                           
         AP    TOTAL$,DUB                                                       
*                                                                               
         EDIT  (4,10(R8)),(10,P+35),COMMAS=YES                                  
         ZICM  RE,10(R8),4                                                      
         AR    R3,RE                                                            
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
TOTAL30  DS    0H                                                               
         LA    R8,14(R8)                                                        
         BCT   R4,TOTAL20                                                       
*                                                                               
TOTAL40  DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(7),=C'*TOTAL*'                                                 
         EDIT  (P8,TOTAL$),(16,P+13),2,COMMAS=YES                               
         EDIT  (R3),(10,P+35),COMMAS=YES                                        
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(13),=C'*# CONTRACTS*'                                          
         EDIT  CONCTR,(14,P+15),COMMAS=YES                                      
         GOTO1 REPORT                                                           
*                                                                               
TOTALX   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
MONTHTAB DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
*                                                                               
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
CONCTR   DS    F                                                                
BUCKETS  DS    200H                                                             
DAREFLAG DS    C                   Y/N                                          
STARTYM  DS    XL2                                                              
ENDYM    DS    XL2                                                              
BFLTSTR  DS    CL6                                                              
BFLTEND  DS    CL6                                                              
SVBDKEY  DS    XL27                                                             
TOTAL$   DS    PL8                                                              
         EJECT                                                                  
STORED   DSECT                                                                  
ELCODE   DS    C                                                                
BUCKETYR DS    CL2                 BUCKET YEAR                                  
BUCKETMN DS    CL2                 BUCKET MONTH                                 
         DS    0D                                                               
BLOCK    DS    3000C                                                            
STOREX   EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033REREP2D02S06/02/97'                                      
         END                                                                    
