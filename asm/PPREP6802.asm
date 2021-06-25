*          DATA SET PPREP6802  AT LEVEL 036 AS OF 05/01/02                      
*PHASE PP6802C,+0,NOAUTO           **** NOTE "C" PHASE                          
*INCLUDE HEXOUT                                                                 
*INCLUDE XSORT                                                                  
*                                                                               
************  CHANGE LOG  ************                                          
*                                                                               
* SMYE 12/13/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                       
*                                                                               
         TITLE 'PP6802 - OUTDOOR VENDOR REPORT'                                 
PP6802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP6802,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP68WRK,R8                                                       
         SPACE 2                                                                
         CLI   MODE,PROCREQ                                                     
         BNE   EXIT                                                             
         LA    RE,PP68WRK                                                       
         LA    RF,PP68WRKL                                                      
         XCEF                                                                   
         L     RF,=V(MKTAB)                                                     
         A     RF,RELO                                                          
         ST    RF,AMKTAB                                                        
         ST    RF,ANXTMKT                                                       
         MVI   0(RF),X'FF'                                                      
         XC    MKTCNT,MKTCNT                                                    
         MVC   SAVMAX,MAXLINES                                                  
         MVI   DASHES,C' '                                                      
         MVC   DASHES+1(L'DASHES-1),DASHES                                      
         MVC   P,SPACES                                                         
         CP    RCSPECNO,=P'1'                                                   
         BNE   *+10                                                             
         ZAP   RCSPECNO,=P'9999999'                                             
*                                                                               
         MVC   BEND,=3X'FF'                                                     
         CLI   QSTART,C' '                                                      
         BE    PR2                                                              
*        GOTO1 DTCNV,DMCB,QSTART,(1,BSTART)                                     
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
PR2      DS    0H                                                               
         CLI   QEND,C' '                                                        
         BE    PR2B                                                             
*        GOTO1 DTCNV,DMCB,QEND,(1,BEND)                                         
         GOTO1 DATCON,DMCB,(0,QEND),(3,BEND)                                    
PR2B     DS    0H                                                               
         CLI   QPUB,C'0'                                                        
         BL    PR4                                                              
         PACK  WORK(5),QPUB(9)     IGNORE ZONE, ED                              
         MVC   SVPUB(4),WORK                                                    
         MVI   SVPUB+5,X'FF'       ENSURE GET FIRST                             
PR4      DS    0H                                                               
         BAS   RE,NXTPUB                                                        
         OC    LASTPUB,LASTPUB                                                  
         BNZ   PR6                 FIRST TIME                                   
         CLI   PUBKEY,X'FF'                                                     
         BE    PR10                NO RECORDS FOUND                             
         BAS   RE,SORTIN                                                        
         B     PR4                                                              
*                                                                               
PR6      CLC   LASTPUB,PUBKPUB                                                  
         BNE   PR8                                                              
         BAS   RE,SORTIN                                                        
         B     PR4                                                              
*                                                                               
PR8      DS    0H                                                               
         CLI   PUBKEY,X'FF'                                                     
         BE    PR10                                                             
         MVC   SAVPUBDA,KEY+27     SAVE DISKADDR OF PUB THAT CAUSES             
*                                  PLANT BREAK                                  
         BAS   RE,SORTOUT          GO PRINT PLANT + ITS MKTS                    
         MVC   KEY+27(4),SAVPUBDA     MUST REREAD PUB THAT CAUSED               
         BAS   RE,GETPUB           PLANT BREAK                                  
         BAS   RE,SORTIN           PASS NEW PUB TO SORT                         
         B     PR4                                                              
*                                                                               
PR10     BAS   RE,SORTOUT          TO DO LAST PLANT                             
         B     EXIT                                                             
*                                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                  GET THIS/FIRST/NEXT PUB                      
         SPACE 2                                                                
NXTPUB   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(1),QMEDIA                                                    
         IC    R1,SVPUB+5                                                       
         LA    R1,1(R1)                                                         
         STC   R1,SVPUB+5                                                       
         MVC   KEY+1(6),SVPUB                                                   
         BAS   RE,HIPUB                                                         
         B     *+8                                                              
NXTPUB2  DS    0H                                                               
         BAS   RE,SEQPUB                                                        
         CLC   KEY(1),KEYSAVE                                                   
         BNE   NXTPUB9                                                          
         CLI   QPUB,C'0'                                                        
         BL    NXTPUB3                                                          
         CLC   KEY(5),KEYSAVE                                                   
         BNE   NXTPUB9                                                          
NXTPUB3  DS    0H                                                               
         CLI   KEY+6,0             BYPASS PASSIVE P + R POINTERS                
         BNE   NXTPUB2                                                          
         CLC   KEY+7(2),QAGENCY                                                 
         BNE   NXTPUB2                                                          
         CLI   KEY+9,X'81'                                                      
         BNE   NXTPUB2                                                          
         BAS   RE,GETPUB                                                        
         CLC   PUBKILL,BEND                                                     
         BL    NXTPUBX                                                          
         B     NXTPUB2                                                          
NXTPUB9  DS    0H                                                               
         MVC   PUBKEY(7),=7X'FF'                                                
NXTPUBX  DS    0H                                                               
         MVC   SVPUB,PUBREC+1                                                   
         B     EXIT                                                             
         SPACE 3                                                                
HIPUB    DS    0H                                                               
         LA    R0,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     DIRPUB                                                           
*                                                                               
SEQPUB   DS    0H                                                               
         LA    R0,DMRSEQ                                                        
         MVC   KEYSAVE,KEY                                                      
*                                                                               
DIRPUB   DS    0H                                                               
         ST    R0,DMCB                                                          
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,,PUBDIR,KEY,KEY                                     
         B     DMCHK                                                            
*                                                                               
GETPUB   DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,PUBREC,DMWORK                 
DMCHK    DS    0H                                                               
         LR    RE,R0                                                            
         TM    DMCB+8,X'FF'                                                     
         BZR   RE                                                               
         DC    H'0'                                                             
         EJECT                                                                  
SORTIN   DS    0H                                                               
         MVC   LASTPUB,PUBKPUB                                                  
         L     R4,ANXTMKT                                                       
         XC    0(20,R4),0(R4)                                                   
         OC    PUBKZON(2),PUBKZON                                               
         BZ    *+10                                                             
         MVC   0(20,R4),PUBZNAME     ZONE IS MARKET NAME                        
         MVC   20(4,R4),KEY+27     SAVE DISK ADDR                               
         LA    R4,24(R4)                                                        
         MVI   0(R4),X'FF'                                                      
         ST    R4,ANXTMKT                                                       
         L     R4,MKTCNT                                                        
         LA    R4,1(R4)                                                         
         ST    R4,MKTCNT                                                        
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
SORTOUT  NTR1                                                                   
         L     R5,MKTCNT                                                        
         L     R4,AMKTAB                                                        
         GOTO1 =V(XSORT),DMCB,(0,(R4)),(R5),24,20,0,RR=RELO                     
SORT1    CLI   0(R4),X'FF'                                                      
         BNE   SORT4                                                            
         L     R4,AMKTAB                                                        
         MVI   0(R4),X'FF'                                                      
         ST    R4,ANXTMKT                                                       
         XC    MKTCNT,MKTCNT                                                    
         B     SORTX                                                            
*                                                                               
SORT4    MVC   KEY+27(4),20(R4)                                                 
         BAS   RE,GETPUB                                                        
         BAS   RE,FMTPUB                                                        
*                                                                               
         LA    R4,24(R4)           GET NEXT REC                                 
         B     SORT1                                                            
SORTX    XIT1                                                                   
         EJECT                                                                  
*                                  FORMAT PUB INFO                              
         SPACE 2                                                                
FMTPUB   NTR1                                                                   
         SPACE 2                                                                
         CLC   LASTFPB,PUBKPUB                                                  
         BE    FMTMKT                                                           
         OC    LASTFPB,LASTFPB                                                  
         BZ    FP2                                                              
         CLI   PUBKEY,X'FF'                                                     
         BE    EXIT                                                             
         SP    RCSPECNO,=P'1'                                                   
         BP    *+6                                                              
         DC    H'0'                                                             
FP2      DS    0H                                                               
         MVC   LASTFPB,PUBKPUB                                                  
         MVI   MKTSW,C'N'                                                       
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         LA    R3,P+09                                                          
         MVC   0(20,R3),PUBNAME                                                 
         MVC   SAVPNAM,PUBNAME        SAVE PUB NAME FOR PRINTING                
*                             ON FUTURE PAGES                                   
         LA    RF,20(R3)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R3                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   132(0,R3),DASHES                                                 
         LA    R3,P+40                                                          
         MVC   0(12,R3),=C'VENDOR CODE-'                                        
*                                  SPECIAL PUB NO. FORMAT FOR OAAA              
         UNPK  WORK(3),PUBKPUB+1(2)                                             
         MVI   WORK+2,C'-'                                                      
         UNPK  WORK+3(5),PUBKPUB+2(3)                                           
         MVC   13(7,R3),WORK                                                    
         MVC   SAVPCODE,WORK          ALSO SAVE VENDOR NUMBER                   
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
*                                                                               
         LA    R3,P+09                                                          
         MVC   0(30,R3),PUBLINE1                                                
         MVC   132(30,R3),PUBLINE2                                              
         CLC   PUBEDTS(2),=C'  '                                                
         BE    FP2E                                                             
         CLC   PUBEDTS(2),=X'0000'                                              
         BE    FP2E                                                             
         MVC   P+40(36),RLINE                                                   
         CLC   PUBEDTS(2),=X'D900'                                              
         BE    FP2E                                                             
         MVC   P+40(36),PLINE                                                   
         CLC   PUBEDTS(2),=X'D700'                                              
         BE    FP2E                                                             
         MVC   P+40(36),PRLINE                                                  
*                                                                               
FP2E     MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
*                                                                               
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'11'                                                     
         BAS   RE,NXTEL                                                         
         BNE   FP5                                                              
         USING PUBSADEL,R2                                                      
         CLI   PUBATTN,C' '                                                     
         BNH   FP3                                                              
         MVC   0(5,R3),=C'ATTN-'                                                
         MVC   6(24,R3),PUBATTN                                                 
FP3      DS    0H                                                               
         CLI   PUBTEL,C' '                                                      
         BNH   FP4                                                              
         LA    R3,P+40                                                          
         MVC   0(10,R3),=C'TELEPHONE-'                                          
         MVC   11(12,R3),PUBTEL                                                 
*                                                                               
FP4      DS    0H                                                               
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
*                                                                               
FP5      DS    0H                                                               
         OC    PUBKZON(2),PUBKZON                                               
         BNZ   FP6                 DON'T DISPLAY 00 MARKET                      
         LA    R6,P+40             BUT DO SHOW COMMENTS                         
         BAS   RE,COMPRT                                                        
         BAS   RE,RPRT             SKIP                                         
FP6      MVC   P,MKTHEAD1                                                       
         BAS   RE,RPRT                                                          
         MVC   P,MKTHEAD2                                                       
         BAS   RE,RPRT                                                          
         MVC   P,MKTHEAD3                                                       
         BAS   RE,RPRT                                                          
*                                                                               
         XC    LSHPADR1(120),LSHPADR1                                           
*                                                                               
         SPACE 3                                                                
FMTMKT   DS    0H                                                               
         OC    PUBKZON(2),PUBKZON                                               
         BZ    EXIT                                                             
         MVI   MKTSW,C'Y'                                                       
FM2      DS    0H                                                               
         LA    R3,P                                                             
         USING LND,R3                                                           
*                                  FIRST SET UP RATE LINES                      
         XC    SAVDAT,SAVDAT                                                    
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'50'                                                     
         SR    R4,R4                                                            
         LA    R5,RATLINS                                                       
FM4      DS    0H                                                               
         BAS   RE,NXTEL                                                         
         BNE   FM5                                                              
         USING PUBRATEL,R2                                                      
         TM    PUBRTYP,X'08'       BYPASS 'SECRET' RATES                        
         BZ    FM4B                                                             
         CLI   QOPT1,C'S'          UNLESS OPT                                   
         BNE   FM4                                                              
FM4B     DS    0H                                                               
         CLC   PUBRSTRT,BSTART                                                  
         BL    FM4                                                              
         MVC   0(RATLINL,R5),SPACES                                             
         CLC   PUBRSTRT,SAVDAT                                                  
         BE    FM4F                                                             
FM4D     DS    0H                                                               
*        GOTO1 DTCNV,DMCB,(1,PUBRSTRT),(3,0(R5))                                
         GOTO1 DATCON,DMCB,(3,PUBRSTRT),(5,0(R5))                               
         MVC   SAVDAT,PUBRSTRT                                                  
FM4F     DS    0H                                                               
         GOTO1 OUTER,DMCB,(1,PUBRSPCE),(1,09(R5))                               
         TM    PUBRTYP,X'80'         X'80' MEANS SHOWING                        
         BZ    *+8                                                              
         MVI   13(R5),C'*'                                                      
         EDIT  (P5,PUBRATE),(10,25(R5)),2,COMMAS=YES                            
         MVC   36(2,R5),PUBDLTYP        2 CHAR DISC PLAN                        
         LA    R5,RATLINL(R5)                                                   
         LA    R4,1(R4)                                                         
         B     FM4                                                              
*                                                                               
FM5      DS    0H                                                               
         MVC   0(RATLINL,R5),SPACES                                             
         ST    R4,FULL                                                          
         LTR   R4,R4                                                            
         BNZ   FM5A                                                             
         MVI   FULL+3,X'01'       SET TO 1 LINE NEEDED                          
         MVC   12(22,R5),=C'ROTARY PAINT AVAILABLE'                             
         MVC   RATLINL(RATLINL,R5),SPACES                                       
*                                                                               
FM5A     DS    0H                                                               
*                                                                               
FM5A1    BAS   RE,CNTCOMS                                                       
*                                                                               
*                                  SET UP ADDR LINES                            
         XC    SADDR1(121),SADDR1                                               
         XC    SPUBCIR5(10),SPUBCIR5                                            
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'14'                                                     
FM5A2    BAS   RE,NXTEL                                                         
         BNE   FM5D                                                             
         CLC   2(3,R2),=3X'FF'                                                  
         BNE   FM5A2                                                            
         USING PUBREPEL,R2                                                      
         OC    PUBPAREP,PUBPAREP                                                
         BNZ   *+6                                                              
         DC    H'0'                NO PAY REP                                   
         OC    PUBTRREP,PUBTRREP                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   LNSUFX,C'.'                                                      
         MVC   LNSUFX+1(1),PUBTRREP+3                                           
         BAS   RE,GETTRREP                                                      
         MVC   SADDR1(90),PREPNAME                                              
         MVC   SADDR4,SPACES                                                    
         CLI   PREPATTN,C' '                                                    
         BNH   FM5B                                                             
         MVC   SADDR4(5),=C'ATTN-'                                              
         MVC   SADDR4+6(20),PREPATTN                                            
*                                                                               
FM5B     CLC   SADDR1(120),LSHPADR1                                             
         BNE   FM5C                                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,FULL+3                                                        
         IC    RF,LINE                                                          
         AR    RE,RF                                                            
         STC   RE,BYTE       SEE IF WILL BE ON NEW PAGE                         
         CLC   BYTE,MAXLINES       IF SO DON'T USE DITTOS                       
         BNL   FMT5X                                                            
         XC    SADDR1(120),SADDR1                                               
         MVC   SADDR1+11(2),=C''''''                                            
         B     FMT5X                                                            
*                                                                               
FM5C     CLI   FULL+3,X'04'                                                     
         BNL   FM5D                                                             
         MVI   FULL+3,X'04'        SET TO 4 LINES NEEDED                        
FM5D     MVC   LSHPADR1(120),SADDR1                                             
*                                                                               
FMT5X    DS    0H                                                               
FM6      DS    0H                                                               
         GOTO1 =V(HEXOUT),DMCB,PUBKPUB+4,LNMCOD,1,0,RR=RELO                     
         MVC   LNMNAM(2),PUBSTATE                                               
         CLI   PUBSTACD,C' '                                                    
         BNH   FM7                                                              
         CLI   PUBSTACD,C'0'                                                    
         BNL   FM7                                                              
         MVC   LNMNAM(2),PUBSTACD                                               
FM7      DS    0H                                                               
         MVI   LNMNAM+2,C','                                                    
         MVC   LNMNAM+4(20),PUBZNAME                                            
*                                                                               
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'20'                                                     
         BAS   RE,NXTEL                                                         
         BNE   FM8                                                              
         USING PUBGENEL,R2                                                      
         MVC   LNADI,PUBMCLAS                                                   
FM8      DS    0H                                                               
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'30'                                                     
         BAS   RE,NXTEL                                                         
         BNE   FM8B                                                             
         USING PUBCIREL,R2                                                      
         MVC   SPUBCIR5,PUBCIR5    SAVE MKT CODE                                
         MVC   SPUBCIR6,PUBCIR6                                                 
         EDIT  PUBCIR1,(10,LNCIRC),COMMAS=YES                                   
*                                                                               
FM8B     DS    0H                                                               
         LA    R5,RATLINS                                                       
         MVC   LNRINF,0(R5)                                                     
         CLC   0(RATLINL,R5),SPACES                                             
         BE    *+8                                                              
         LA    R5,RATLINL(R5)                                                   
         LA    R6,SADDR                                                         
         MVC   LNSHIP,0(R6)                                                     
         LA    R6,30(R6)                                                        
*                                                                               
         MVC   LNEED,FULL+3                                                     
         BAS   RE,RPRT                                                          
*                                                                               
         OC    SPUBCIR5(10),SPUBCIR5                                            
         BZ    FM9                                                              
         CP    SPUBCIR5,=P'0'                                                   
         BE    FM8D                                                             
         OI    SPUBCIR5+4,X'0F'                                                 
         UNPK  P+9(5),SPUBCIR5                                                  
*                                                                               
FM8D     CP    SPUBCIR6,=P'0'                                                   
         BE    FM8X                                                             
         MVC   P+16(7),=C'PART OF'                                              
         OI    SPUBCIR6+4,X'0F'                                                 
         UNPK  P+25(5),SPUBCIR6                                                 
*                                                                               
FM8X     DS    0H                                                               
FM9      DS    0H                                                               
         MVC   LNRINF,0(R5)                                                     
         CLC   0(RATLINL,R5),SPACES                                             
         BE    *+8                                                              
         LA    R5,RATLINL(R5)                                                   
*                                                                               
         CLI   0(R6),0                                                          
         BE    FM9X                                                             
         MVC   LNSHIP,0(R6)                                                     
         LA    R6,L'LNSHIP(R6)                                                  
FM9X     CLC   P,SPACES                                                         
         BE    FM10                                                             
         BAS   RE,RPRT                                                          
         B     FM9                                                              
FM10     DS    0H                                                               
         LA    R6,P+09                                                          
         BAS   RE,COMPRT                                                        
         MVI   LNEED,X'80'                                                      
         BAS   RE,RPRT                                                          
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         SPACE 3                                                                
         EJECT                                                                  
COMPRT   NTR1                      ROUTINE TO FIND AND PRINT PUB COMS           
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'66'                                                     
COMP1    BAS   RE,NXTEL                                                         
         BNE   COMPRTX                                                          
         CLI   1(R2),2             ZREO LENGTH COMMENT                          
         BE    COMP1                                                            
         SR    R5,R5                                                            
         IC    R5,1(R2)                                                         
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         EX    R5,MOVECOM                                                       
         B     *+10                                                             
MOVECOM  MVC   0(0,R6),2(R2)                                                    
         BAS   RE,RPRT                                                          
         B     COMP1                                                            
*                                                                               
COMPRTX  XIT1                                                                   
         EJECT                                                                  
         USING PUBREPEL,R2                                                      
GETTRREP NTR1                      ROUTINE TO GET IOA TRAF REP                  
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),PUBPAREP                                                
         MVC   KEY+8(1),PUBTRREP+3    SUFFIX                                    
         CLC   PREPREC(9),KEY                                                   
         BE    GETTRX              REP ALREADY THERE                            
         BAS   RE,RDHI                                                          
         CLC   KEYSAVE(9),KEY                                                   
         BNE   REPNFND             REP NOT ON FILE                              
         BAS   RE,GETREP                                                        
         B     GETTRX                                                           
*                                                                               
REPNFND  XC    PREPREC(20),PREPREC                                              
         XC    PREPNAME(110),PREPNAME      CLEAR REP NAME,ADDR                  
         MVC   PREPNAME(21),=C'** ADDRESS UNKNOWN **'                           
*                                                                               
*                                                                               
GETTRX   MVC   KEY,SAVEKEY         RESTORE KEY                                  
         XIT1                                                                   
*                                                                               
*                                                                               
RDHI     LA    R0,DMRDHI                                                        
         ST    R0,DMCB                                                          
         MVC   KEYSAVE,KEY                                                      
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,,PRTDIR,KEY,KEY                                     
         B     DMCHKR                                                           
*                                                                               
GETREP   DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,PREPREC,DMWORK                
*                                                                               
DMCHKR   LR    RE,R0                                                            
         TM    DMCB+8,X'FF'                                                     
         BZR   RE                                                               
         DC    H'0'                                                             
         DROP  R2                                                               
         EJECT                                                                  
CNTCOMS  NTR1                                                                   
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'66'                                                     
         L     R4,FULL                                                          
CNTC1    BAS   RE,NXTEL                                                         
         BNE   CNTCX                                                            
         LA    R4,1(R4)                                                         
         B     CNTC1                                                            
*                                                                               
CNTCX    ST    R4,FULL                                                          
         XIT1                                                                   
         EJECT                                                                  
*                                  PRINT CONTROL                                
         SPACE 2                                                                
RPRT     NTR1                                                                   
         SPACE 2                                                                
         CLI   FORCEHED,C'Y'                                                    
         BE    RP4                                                              
         CLI   LNEED,0                                                          
         BE    RP2                                                              
         TM    LNEED,X'80'                                                      
         BZ    RP1                                                              
         MVI   MAXLINES,99                                                      
         B     RP2                                                              
RP1      DS    0H                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LNEED                                                         
         IC    RF,LINE                                                          
         AR    RE,RF                                                            
         STC   RE,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BNL   RP4                                                              
*                                                                               
RP2      DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   MAXLINES,SAVMAX                                                  
         MVI   LNEED,0                                                          
         B     EXIT                                                             
RP4      DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         CLI   MKTSW,C'Y'                                                       
         BNE   RP2                                                              
         CLC   PAGE,=H'1'                                                       
         BE    RP6                                                              
         MVC   HEAD5+11(20),SAVPNAM                                             
         MVC   HEAD5+40(12),=C'VENDOR CODE-'                                    
         MVC   HEAD5+53(7),SAVPCODE                                             
*                                                                               
RP6      DS    0H                                                               
         MVC   HEAD7,MKTHEAD1                                                   
         MVC   HEAD8,MKTHEAD2                                                   
         MVC   HEAD9,MKTHEAD3                                                   
         B     RP2                                                              
         SPACE 3                                                                
*                                                                               
MKTHEAD1 DC    CL132'MKT                                          EFF. X        
                 SIZE   DISPLAYS                              '                 
MKTHEAD2 DC    CL132'CDE ST, MARKET NAME          ADI      POP.   DATE X        
                 SHOW  REG ILLUM       RATE DP SFX SHIPPING ADDRESS    X        
                         ADD''L CHANGES'                                        
MKTHEAD3 DC    CL132'--- ---------------          ---      ---- -------X        
               - ----  --- -----       ---- -- --- ----------------    X        
                         -------------'                                         
         SPACE 3                                                                
RLINE    DC    CL36'ROTARY PAINT AVAILABLE'                                     
PLINE    DC    CL36'PAINTED DISPLAY BULLETINS AVAILABLE'                        
PRLINE   DC    CL36'ROTARY AND PERMANENT PAINT AVAILABLE'                       
NXTEL    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NXTEL2                                                           
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NXTEL                                                            
NXTEL2   DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
MKTAB    CSECT                                                                  
         DS    CL4800              ROOM FOR 200 MKTS  200 X 24                  
MKTABX   DS    X'00'               20 CHAR ZONE + 4 CHAR DISK ADDR              
*                                                                               
PP68WRKD DSECT                                                                  
PP68WRK  DS    0C                                                               
DASHES   DS    CL30'-'                                                          
SVPUB    DS    CL6                                                              
SAVDAT   DS    XL3                                                              
SAVMAX   DS    X                                                                
LASTPUB  DS    XL4                                                              
LASTFPB  DS    XL4                                                              
MKTSW    DS    X                                                                
ELCODE   DS    X                                                                
LNEED    DS    X                                                                
SAVPNAM  DS    CL20                                                             
SAVPCODE DS    CL7                                                              
*                                                                               
LSHPADR1 DS    CL30                                                             
LSHPADR2 DS    CL30                                                             
LSHPADR3 DS    CL30                                                             
LSHPADR4 DS    CL30                                                             
*                                                                               
SPUBCIR5 DS    PL5                                                              
SPUBCIR6 DS    PL5                                                              
*                                                                               
SADDR    DS    0CL120                                                           
SADDR1   DS    CL30                                                             
SADDR2   DS    CL30                                                             
SADDR3   DS    CL30                                                             
SADDR4   DS    CL30                                                             
         DS    CL1                 NEEDED                                       
SADDRL   EQU   30                                                               
*                                                                               
RATLINS  DS    CL1520              40 X 38                                      
RATLINL  EQU   38                                                               
*                                                                               
SAVEKEY  DS    CL32                                                             
BSTART   DS    CL3                                                              
BEND     DS    CL3                                                              
*                                                                               
AMKTAB   DS    A                                                                
ANXTMKT  DS    A                                                                
MKTCNT   DS    F                                                                
SAVPUBDA DS    XL4                                                              
*                                                                               
PP68WRKL EQU   *-PP68WRK                                                        
         SPACE 3                                                                
LND      DSECT                                                                  
         DS    CL1                                                              
LNMCOD   DS    CL2                                                              
         DS    CL1                                                              
LNMNAM   DS    CL24                                                             
         DS    CL1                                                              
LNADI    DS    CL3                                                              
LNCIRC   DS    CL10                                                             
         DS    CL1                                                              
LNRINF   DS    CL38                                                             
         DS    CL2                                                              
LNSUFX   DS    CL2                                                              
         DS    CL1                                                              
LNSHIP   DS    CL30                                                             
         PRINT OFF                                                              
       ++INCLUDE PPWORKD                                                        
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036PPREP6802 05/01/02'                                      
         END                                                                    
