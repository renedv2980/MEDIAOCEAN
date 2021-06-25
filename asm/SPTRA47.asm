*          DATA SET SPTRA47    AT LEVEL 055 AS OF 07/13/11                      
*PHASE T21647C                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T21647 COMMERCIAL ACTIVITY LIST'                                
* LEV 14 MOVE OUT OF GEND SPARE                                                 
* LEV 15-17 ADD OPTION FIELD, SHOW DETAIL STATIONS, SHOW DELETED CMLS           
* LEV 19    MAR/20/86 ????????????                                              
* LEV 20-21 SEP29/87 ADD SORTER FOR OFFLINE, DFNY CO KILLED SIZE                
* LEV 22-23 OCT30/87 FIX SORTER BUG ONLINE                                      
* LEV 24    MAR15/88 FIX SORTER BUG (NOT CLOSED FOR NO RECORDS)                 
* LEV 25    MAR17/93 ADD 2 LINES OF COMML DESC                                  
* LEV 26    APR21/93 DO NOT ALLOW ALL CLIENTS                                   
* LEV 27    MAY31/94 STRAFFIC BUG                                               
* LEV 28    JUL20/94 CHANGE TO FILENAME                                         
* LEV 29 SM APR02/01 USE TRAFFIC OFFICE                                         
* LEV 31 BG NOV01/02 NEW INST RECAP RECS                                        
* LEV 32 SM AUG18/03 FIX BUMPING THROUGH NEW INST RECAP REC ELEM                
* LEV 33 BGRI JAN15/04 CHGE SVSPARE TO TO SVSPAREX                    *         
* LEV 34 BGRI OCT11/04 FIX FALSE EXCEPTIONS                           *         
* LEV 35 BGRI DEC08/04 FIX DETAIL LISTING OPTION                      *         
* LEV 36 BGRI JAN19/05 FIX MORE PTNS THAN STATIONS FOR EXCEPTION      *         
* LEV 37 MHER JUL/09   ADID SUPPORT                                             
* LEV 55 MNAS JUL13/11 ADID BUG FIX ON DETAIL REPORT                            
*                                                                               
* NOTE NEXT START CHANGES AT LRR, AND HAVE TO COMPARE FOR EQUAL ENTRIES         
*      AND ALSO BUILD TABLE FOR MARKETS/STATIONS (SORTER GET IS THERE)          
*                                                                               
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - MARKET RECORDS                                             
*             AIO3 - UNUSED                                                     
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - POINTER FOR STATION TABLE BUILT BY MARKET                         
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - ALSO CTR IN LR BUILD COMML TABLE RTN                   
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*              POINTER FOR COMML TABLE BUILT BY MARKET                          
*        R7 - UNUSED                                                            
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTRA00-T21600)                             
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
***********************************************************************         
         EJECT                                                                  
*======================================================================         
*                                                                               
* THIS PROGRAM READS THRU TRAFFIC INSTRUCTION RECAP RECORDS, AND                
* CREATES A STATION ENTRY FOR EACH COMML AND FIRST/LAST DATE PAIR.              
*                                                                               
*======================================================================         
                                                                                
T21647   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1647**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA             BASE SCREEN FOR SYSTEM + THIS PROG           
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR47RR                                                      
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       OFF-LINE ACTIVITY                            
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT,BCLT                                                        
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
*                                                                               
         LA    R2,TRAMKTH          MARKET                                       
         XC    BMKT,BMKT                                                        
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VK20                                                             
         GOTO1 VALIMKT                                                          
*                                                                               
VK20     LA    R2,TRACMLH          COMML ID                                     
         BAS   RE,VCML                                                          
         MVC   SVCMML,WORK         SAVE 8-CHAR CMML (PACKED OR ISCI)            
*                                                                               
VK30     LA    R2,TRAPERH          PERIOD                                       
         BAS   RE,VPER                                                          
*                                                                               
         LA    R2,TRAOPTH                                                       
         BAS   RE,VOPT            VALIDATE OPTIONS                              
*                                                                               
* NOW BUILD KEY                                                                 
*                                                                               
         XC    CMLSTIND,CMLSTIND                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A24'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+6(2),BMKT                                                    
         MVI   FIRSTSW,0                                                        
         B     EXIT                                                             
         EJECT                                                                  
*==============================================================                 
* ONLINE ACTIVITY LIST                                                          
*==============================================================                 
                                                                                
LR       LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
*                                   SET TO READ INSTRUCTION RECAP               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A24'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+6(2),BMKT                                                    
*                                                                               
LR02     DS   0H                                                                
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,RR=SPTR47RR                     
*                                                                               
LR04     XC    SORTCNT,SORTCNT                                                  
         GOTO1 HIGH                                                             
         B     LR12                                                             
*                                                                               
LR10     MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
LR12     MVC   SVKEY,KEY                                                        
         CLC   KEY(5),KEYSAVE     ID/AM/CLT                                     
         BNE   LR60                                                             
*                                                                               
         OC    BMKT,BMKT           WAS MARKET ENTERED                           
         BZ    LR14                                                             
         CLC   KEY+6(2),BMKT                                                    
         BNE   LR10                                                             
*                                                                               
LR14     L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         LR    R4,R6                                                            
         USING INSKEY,R4                                                        
         USING INSDTAEL,R6                                                      
*                                                                               
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL            FIND INST DATA ELEM                          
         BNE   LR10                                                             
*                                                                               
* CALC NO OF PATTERNS (SUBELS) IN ELEMENT *                                     
*                                                                               
LR20     SR    R0,R0                                                            
         LLC   R1,INSDTALN                                                      
         AHI   R1,-(INSPTTN-INSDTAEL)                                           
         D     R0,=A(INSSUBEL)                                                  
         LR    R0,R1               SET COUNT ON SUBELS                          
         LA    R2,INSPTTN                                                       
*                                                                               
LR22     BRAS  RE,PROCSUB                                                       
*                                                                               
         LA    R2,7(R2)                                                         
         BCT   R0,LR22                                                          
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,NEXTEL                                                        
         BE    LR20                                                             
         B     LR10                                                             
         EJECT                                                                  
PROCSUB  NTR1                                                                   
         CLC   PERSTART,5(R2)      LTD                                          
         BH    PROCSUBX                                                         
         CLC   PEREND,3(R2)        FTD                                          
         BL    PROCSUBX                                                         
*                                                                               
         OC    0(3,R2),0(R2)      HIATUS PATTERN IS ZERO                        
         BZ    PROCSUBX                                                         
*                                                                               
         CLC   0(3,R2),=X'FFFFFF' TBA PATTERN?                                  
         BE    PROCSUBX                                                         
*                                                                               
         MVC   SVTCDTS,3(R2)                                                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING PATKEY,R1                                                        
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,BAGYMD                                                    
         MVC   PATKCLT,SVBCLT                                                   
         MVC   PATKPRD(4),INSPRD1                                               
         MVC   PATKCODE,INSKCOPY                                                
         MVC   PATKREF,0(R2)                                                    
         DROP  R1                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PROCSUBX                                                         
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ADIDFLAG,C'N'                                                    
         TM    PATSTAT1-PATDTAEL(R6),PATSADID                                   
         BZ    *+8                                                              
         MVI   ADIDFLAG,C'Y'                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATCMLEL,R6                                                      
PROCS10  LLC   R3,PATCMLLN                                                      
         SRL   R3,4                DIVIDE BY 16 - DROP ODD                      
         LA    R2,2(R6)                                                         
*                                                                               
PROCS12  CLI   SVCMMLN,0           WAS COMML ENTERED                            
         BE    PROCS30             NO                                           
         CLI   ADIDFLAG,C'Y'                                                    
         BE    PROCS20                                                          
*                                                                               
         LLC   R1,SVCMMLN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),TRACML                                                   
         BE    PROCS30                                                          
*                                                                               
         OC    8(8,R2),8(R2)                                                    
         BZ    PROCS34                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),TRACML                                                   
         BE    PROCS30                                                          
         B     PROCS34                                                          
*                                                                               
PROCS20  GOTO1 VTRPACK,DMCB,(C'U',(R2)),WORK                                    
         LLC   R5,SVCMMLN                                                       
         BCTR  R5,0                                                             
         EX    R5,PROCSCLC                                                      
         BE    PROCS30                                                          
*                                                                               
         OC    8(8,R2),8(R2)                                                    
         BZ    PROCS34                                                          
         GOTO1 VTRPACK,DMCB,(C'U',8(R2)),WORK                                   
         EX    R5,PROCSCLC                                                      
         BE    PROCS30                                                          
         B     PROCS34                                                          
*                                                                               
PROCSCLC CLC   WORK(0),TRACML                                                   
*                                                                               
PROCS30  CLC   0(2,R2),=X'5C00'   DELETED CML                                   
         BE    PROCS34                                                          
         CLC   0(8,R2),HIATUS     BYPASS HIATUS                                 
         BE    PROCS34                                                          
*                                                                               
* BUILD SORT REC *                                                              
*                                                                               
         MVC   CMLMKT,INSKMKT                                                   
         MVC   CMLDTS,SVTCDTS                                                   
         MVC   CMLCML(16),0(R2)                                                 
         MVC   CMLSTA,INSKSTA                                                   
*                                                                               
         TM    SVOPTSW,OPTTRCE     DO A TRACE                                   
         BZ    PROCS32                                                          
*                                                                               
         MVC   P+1(3),=C'PUT'                                                   
         MVC   P+5(4),=C'MKT='                                                  
         SR    RE,RE                                                            
         ICM   RE,3,CMLMKT                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+9(4),DUB                                                       
         MVC   P+15(4),=C'SDT='                                                 
         GOTO1 DATCON,DMCB,(2,CMLFTD),(5,P+19)                                  
         MVC   P+30(4),=C'LTD='                                                 
         GOTO1 DATCON,DMCB,(2,CMLLTD),(5,P+34)                                  
         MVC   P+50(4),=C'STA='                                                 
         XC    DUB(2),DUB                                                       
         MVC   DUB+2(3),CMLSTA                                                  
         GOTO1 MSUNPK,DMCB,DUB,BLOCK+4,BLOCK+8                                  
         MVC   P+54(5),BLOCK+8                                                  
         MVC   P+60(4),=C'CML='                                                 
         MVC   P+64(8),CMLCML                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BAS   RE,CKPRT            SEE IF HDHK PRINTED MKT                      
*                                                                               
PROCS32  DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',CMLENT                                   
*                                                                               
         L     R5,SORTCNT                                                       
         LA    R5,1(R5)                                                         
         ST    R5,SORTCNT                                                       
*                                                                               
PROCS34  LA    R2,16(R2)           NEXT CML PR                                  
         BCT   R3,PROCS12                                                       
*                                                                               
PROCSUBX XIT1                                                                   
*                                                                               
* TABLE (IF ANY) BUILT *                                                        
*                                                                               
LR60     OC    SORTCNT,SORTCNT     WAS ANYTHING FOUND                           
         BNZ   LR70                                                             
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=CL30'* NOTE * NO RECORDS SELECTED *'                
         GOTO1 ERREX2                                                           
*                                                                               
LR70     MVC   SVKEY,KEY                                                        
*                                                                               
         CLI   MODE,PRINTREP       OFF-LINE ACTIVITY                            
         BE    LRR                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* OFFLINE ACTIVITY LIST *                                                       
*                                                                               
LRR      DS   0H                                                                
*                                                                               
         L     RE,VADUMMY                                                       
         MVC   0(8,RE),=CL8'*CMLTAB*'                                           
         LA    RE,8(,RE)                                                        
         ST    RE,CMLTADR                                                       
         AHI   RE,20000                                                         
         ST    RE,CMLTADRX                                                      
         MVC   0(8,RE),=CL8'*STATAB*'                                           
         LA    RE,8(,RE)                                                        
         ST    RE,STATADR                                                       
         AHI   RE,20000                                                         
         ST    RE,STATADRX                                                      
*                                                                               
         XC    SVHP1,SVHP1                                                      
         XC    SVHP2,SVHP2                                                      
         XC    SVHP3,SVHP3                                                      
         XC    SVHP4,SVHP4                                                      
*                                                                               
         BAS   RE,GSRT             GO GET SORT RECORD                           
*                                                                               
         MVC   CMLENT,SVCMLENT                                                  
*                                                                               
         OC    CMLENT,CMLENT                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVDTS,CMLDTS        SAVE DATES                                   
         MVC   SVMKT,CMLMKT                                                     
*                                                                               
LRR10    DS   0H                                                                
*                                                                               
         OC    CMLENT,CMLENT                                                    
         BZ    LRR80                                                            
*                                                                               
         ICM   R0,3,CMLMKT                                                      
         CVD   R0,DUB                                                           
         UNPK  QMKT,DUB                                                         
         OI    QMKT+3,X'F0'                                                     
         BAS   RE,FMKT                                                          
*                                                                               
         MVC   PMKT,QMKT                                                        
         MVC   PMKTNM,MKTNM                                                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BAS   RE,CKPRT            SEE IF HDHK PRINTED MKT                      
*                                                                               
         L     R0,CMLTADR                                                       
         L     R1,CMLTADRX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,STATADR                                                       
         L     R1,STATADRX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         SR    R4,R4              STATION CTR                                   
         SR    R5,R5              CML CTR                                       
*                                                                               
LRR20    L     R6,CMLTADR                                                       
         MVC   SVMKT,CMLMKT                                                     
*                                                                               
         L     R3,STATADR                                                       
         USING STATBL,R3                                                        
*                                                                               
* BUILD  STATIONS IN THIS MARKET FOR THESE TELECAST DATES & CMMLS *             
*                                                                               
LRR21    DS   0H                                                                
         OC    STATSTA,STATSTA     EMPTY ENTRY                                  
         BZ    LRR23                                                            
*                                                                               
         CLC   CMLSTA,STATSTA      EQ STA                                       
         BNE   LRR22                                                            
         CLC   CMLDTS,STATDTS      EQ DTS                                       
         BNE   LRR22                                                            
         CLC   CMLCMLS,STATCMLS                                                 
         BE    LRR24                                                            
*                                                                               
LRR22    DS    0H                                                               
         LA    R3,STATNEXT                                                      
         C     R3,STATADRX                                                      
         BL    LRR21                                                            
         DC    H'0'                                                             
*                                                                               
LRR23    DS    0H                                                               
         MVC   STATSTA,CMLSTA                                                   
         MVC   STATCMLS,CMLCMLS                                                 
         MVC   STATDTS,CMLDTS                                                   
*                                                                               
         BCTR  R4,0                                                             
*                                                                               
         USING CMLTBL,R6                                                        
         SR    R0,R0                                                            
*                                                                               
LRR24    DS   0H                                                                
         OC    CMLTENT,CMLTENT     EMPTY ENTRY                                  
         BZ    LRR26                                                            
*                                                                               
         CLC   CMLCMLS,CMLTCMLS    EQ CMLS                                      
         BNE   LRR25                                                            
         CLC   CMLDTS,CMLTDTS      EQ DATES                                     
         BNE   LRR25                                                            
         IC    R0,CMLTSTCT                                                      
         AHI   R0,1                                                             
         STC   R0,CMLTSTCT                                                      
         B     LRR28                                                            
*                                                                               
LRR25    DS   0H                                                                
         LA    R6,CMLTNEXT                                                      
         C     R6,CMLTADRX                                                      
         BL    LRR24                                                            
         DC    H'0'                                                             
*                                                                               
LRR26    DS   0H                                                                
         MVC   CMLTCMLS,CMLCMLS                                                 
         MVC   CMLTDTS,CMLDTS                                                   
         MVI   CMLTSTCT,1                                                       
         BCTR  R5,0                                                             
*                                                                               
LRR28    DS   0H                                                                
         OC    CMLENT,CMLENT       AT END                                       
         BZ    LRR80                YES                                         
*                                                                               
         BAS   RE,GSRT             GO GET SORT RECORD                           
*                                                                               
         MVC   CMLENT,SVCMLENT                                                  
*                                                                               
         CLC   SVDTS,CMLDTS        SAME DATES?                                  
         BNE   LRR29                NO, DO THIS LOT                             
*                                                                               
         CLC   SVMKT,SVCMLENT      SAME MKT                                     
         BE    LRR20                                                            
*                                                                               
* SAVE CML ENTRY COUNT *                                                        
*                                                                               
LRR29    DS   0H                                                                
         MVC   SVDTS,CMLDTS        SAVE DATES                                   
*                                                                               
         LPR   R5,R5                                                            
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R5,CMLCT                                                         
*                                                                               
* SORT CMLS BY COMMLS, THEN BY COUNT IN DESCENDING ORDER *                      
*                                                                               
         L     R6,CMLTADR                                                       
*                                                                               
* SORT BY DATES & COMMLS *                                                      
*                                                                               
         GOTO1 XSORT,DMCB,(R6),(R5),L'CMLTENT,20,0,0                            
*                                                                               
         MVI   MKTCOMCT,0          SET COMMON ENTRY COUNT ZERO                  
         TM    SVOPTSW,OPTDETL                                                  
         BO    LRR29C                                                           
*                                                                               
* NOW SORT CMLS BY STATION COUNT IN DESCENDING ORDER                            
*                                                                               
         GOTO1 XSORT,DMCB,(1,(R6)),(R5),L'CMLTENT,1,20,0                        
*                                                                               
         MVC   MKTCOMCT,CMLTSTCT   SAVE COMMON ENTRY COUNT                      
*                                                                               
* STATION COUNT - SORT BY DTS, COMMLS, & STATIONS *                             
*                                                                               
LRR29C   DS   0H                                                                
         LPR   R4,R4                                                            
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R4,STACT                                                         
         L     R3,STATADR                                                       
*                                                                               
         GOTO1 XSORT,DMCB,(R3),(R4),L'STATENT,L'STATCMP,0,0                     
*                                                                               
* PRINT INDIVIDUAL COMMLS *                                                     
*                                                                               
LRR30    DS    0H                                                               
         TM    SVOPTSW,OPTDETL                                                  
         BO    LRR40                                                            
*                                                                               
         MVC   WORK(8),CMLTCML                                                  
*                                                                               
         BAS   RE,FCML                                                          
         MVC   PCMML(12),WORK                                                   
*                                                                               
LRR31    CLI   SVCMLDEL,0          FLAG DELETED COMMERCIALS                     
         BE    *+12                                                             
         MVI   PCMML-1,C'*'                                                     
         MVI   PCMML+8,C'*'                                                     
*                                                                               
         MVC   PCMMLT,SVCMLDS1                                                  
         LA    R1,PCMMLT+132                                                    
         OC    SVCMLDS2,SVCMLDS2   SECOND LINE OF DESC                          
         BZ    *+14                                                             
         MVC   0(20,R1),SVCMLDS2                                                
         LA    R1,132(,R1)                                                      
*                                                                               
         OC    SVCMLDS3,SVCMLDS3   THIRD LINE OF DESC                           
         BZ    *+14                                                             
         MVC   0(20,R1),SVCMLDS3                                                
         LA    R1,132(,R1)                                                      
*                                                                               
         OC    SVCMLCLT,SVCMLCLT   CLIENT COMMERCIAL #                          
         BZ    *+10                                                             
         MVC   0(20,R1),SVCMLCLT                                                
*                                                                               
         GOTO1 DATCON,DMCB,(2,CMLTFTD),(4,PACTDTES)                             
         MVI   PACTDTES+5,C'-'                                                  
         GOTO1 (RF),(R1),(2,CMLTLTD),(5,PACTDTES+6)                             
*                                                                               
         OC    CMLTCML2,CMLTCML2   PIGGYBACK                                    
         BZ    LRR32                NO                                          
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BAS   RE,CKPRT            SEE IF HDHK PRINTED MKT                      
*                                                                               
         MVC   PCMML-5(3),=C'P/B'                                               
*                                                                               
         MVC   WORK(8),CMLTCML2                                                 
         BAS   RE,FCML                                                          
         MVC   PCMML(12),WORK                                                   
*                                                                               
         CLI   SVCMLDEL,0          FLAG DELETED COMMERCIALS                     
         BE    *+12                                                             
         MVI   PCMML-1,C'*'                                                     
         MVI   PCMML+8,C'*'                                                     
*                                                                               
         MVC   PCMMLT,SVCMLDS1                                                  
         LA    R1,PCMMLT+132                                                    
         OC    SVCMLDS2,SVCMLDS2   SECOND LINE OF DESC                          
         BZ    *+14                                                             
         MVC   0(20,R1),SVCMLDS2                                                
         LA    R1,132(,R1)                                                      
*                                                                               
         OC    SVCMLDS3,SVCMLDS3   THIRD LINE OF DESC                           
         BZ    *+14                                                             
         MVC   0(20,R1),SVCMLDS3                                                
         LA    R1,132(,R1)                                                      
*                                                                               
         OC    SVCMLCLT,SVCMLCLT   CLIENT COMMERCIAL #                          
         BZ    *+10                                                             
         MVC   0(20,R1),SVCMLCLT                                                
*                                                                               
LRR32    MVI   SPACING,2                                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BAS   RE,CKPRT            SEE IF HDHK PRINTED MKT                      
*                                                                               
         LA    R6,CMLTNEXT                                                      
*                                                                               
         OC    CMLTENT,CMLTENT     AT END OF COMMLS                             
         BZ    LRR70                YES                                         
*                                                                               
         CLC   MKTCOMCT,CMLTSTCT   COMPARE MKT COMMON TO THIS                   
         BE    LRR30                                                            
*                                                                               
* PRINT ANY EXCEPTION STATIONS (WITH OPTION DETAIL ALL ARE EXCEPTION) *         
*                                                                               
LRR40    DS   0H                                                                
*        LLC   R0,CMLTSTCT                                                      
         L     R3,STATADR                                                       
         TM    SVOPTSW,OPTDETL                                                  
         BO    LRR44                                                            
*                                                                               
LRR42    DS    0H                                                               
         CLC   CMLTDTS(20),STATCMP COMPARE DATES & COMMLS                       
         BE    LRR44                                                            
*                                                                               
         LA    R3,STATNEXT                                                      
*                                                                               
         OC    STATENT,STATENT                                                  
         BNZ   LRR42                                                            
         B     LRR48                                                            
*                                                                               
LRR44    DS    0H                                                               
         TM    SVOPTSW,OPTDETL                                                  
         BO    *+10                                                             
         MVC   PMKTNM+9(9),=C'EXCEPTION'                                        
*                                                                               
         MVC   DUB(2),SVMKT                                                     
         MVC   DUB+2(3),STATSTA                                                 
         BAS   RE,FMTSTA                                                        
         MVC   PSTA,STAPRNT                                                     
         OC    STANET,STANET                                                    
         BZ    *+10                                                             
         MVC   PSTA(8),STANET                                                   
*                                                                               
         MVC   WORK(8),CMLTCML                                                  
         BAS   RE,FCML                                                          
         MVC   PCMML(12),WORK                                                   
*                                                                               
         CLI   SVCMLDEL,0          FLAG DELETED COMMERCIALS                     
         BE    *+12                                                             
         MVI   PCMML-1,C'*'                                                     
         MVI   PCMML+8,C'*'                                                     
*                                                                               
         MVC   PCMMLT,SVCMLDS1                                                  
         LA    R1,PCMMLT+132                                                    
         OC    SVCMLDS2,SVCMLDS2   SECOND LINE OF DESC                          
         BZ    *+14                                                             
         MVC   0(20,R1),SVCMLDS2                                                
         LA    R1,132(,R1)                                                      
*                                                                               
         OC    SVCMLDS3,SVCMLDS3   THIRD LINE OF DESC                           
         BZ    *+14                                                             
         MVC   0(20,R1),SVCMLDS3                                                
         LA    R1,132(,R1)                                                      
*                                                                               
         OC    SVCMLCLT,SVCMLCLT   CLIENT COMMERCIAL #                          
         BZ    *+10                                                             
         MVC   0(20,R1),SVCMLCLT                                                
*                                                                               
         GOTO1 DATCON,DMCB,(2,CMLTFTD),(4,PACTDTES)                             
         MVI   PACTDTES+5,C'-'                                                  
         GOTO1 DATCON,DMCB,(2,CMLTLTD),(5,PACTDTES+6)                           
*                                                                               
         OC    CMLTCML2,CMLTCML2   PIGGYBACK                                    
         BZ    LRR46                NO                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BAS   RE,CKPRT            SEE IF HDHK PRINTED MKT                      
*                                                                               
         MVC   PCMML-5(3),=C'P/B'                                               
         MVC   PCMML,CMLTCML2                                                   
*                                                                               
         MVC   WORK(8),CMLTCML2                                                 
         BAS   RE,FCML                                                          
*                                                                               
         CLI   SVCMLDEL,0          FLAG DELETED COMMERCIALS                     
         BE    *+12                                                             
         MVI   PCMML-1,C'*'                                                     
         MVI   PCMML+8,C'*'                                                     
*                                                                               
         MVC   PCMMLT,SVCMLDS1                                                  
         LA    R1,PCMMLT+132                                                    
         OC    SVCMLDS2,SVCMLDS2   SECOND LINE OF DESC                          
         BZ    *+14                                                             
         MVC   0(20,R1),SVCMLDS2                                                
         LA    R1,132(,R1)                                                      
*                                                                               
         OC    SVCMLDS3,SVCMLDS3   THIRD LINE OF DESC                           
         BZ    *+14                                                             
         MVC   0(20,R1),SVCMLDS3                                                
         LA    R1,132(,R1)                                                      
*                                                                               
         OC    SVCMLCLT,SVCMLCLT   CLIENT COMMERCIAL #                          
         BZ    *+10                                                             
         MVC   0(20,R1),SVCMLCLT                                                
*                                                                               
LRR46    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BAS   RE,CKPRT            SEE IF HDHK PRINTED MKT                      
*                                                                               
         LA    R3,STATNEXT                                                      
*                                                                               
         OC    STATENT,STATENT                                                  
         BNZ   LRR42                                                            
*                                                                               
*        BCT   R0,LRR42                                                         
*                                                                               
LRR48    DS    0H                                                               
         LA    R6,CMLTNEXT                                                      
*                                                                               
         OC    CMLTENT,CMLTENT                                                  
         BNZ   LRR40                                                            
*                                                                               
LRR70    DS    0H                                                               
         CLC   SVMKT,CMLENT        SAME MKT?                                    
         BNE   LRR10                GO GET & DO NEXT MKT                        
*                                                                               
* NOW DEAL WITH NEW SET OF DATES                                                
*                                                                               
*                                                                               
         L     R6,CMLTADR                                                       
         LR    RE,R6                                                            
         L     RF,CMLTADRX                                                      
         SR    RF,RE                                                            
         XCEFL                                                                  
*                                                                               
         L     R0,STATADR                                                       
         L     R1,STATADRX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         SR    R4,R4               STATION CTR                                  
         SR    R5,R5               CML CTR                                      
*                                                                               
         B     LRR20               GO GET NEXT SET OF DATES                     
*                                                                               
LRR80    DS   0H                                                                
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE COMMERCIAL RECORD *                                                  
*                                                                               
VCML     NTR1                                                                   
         XC    WORK,WORK                                                        
         MVI   ADIDFLAG,C'A'       IF NO INPUT, SET DOING ADIDS                 
         CLI   5(R2),0                                                          
         BE    VCMLX                                                            
*                                                                               
         GOTO1 ANY                 MOVE INPUT TO WORK AND SPACE FILL            
         CLI   5(R2),12                                                         
         JH    BADLENER                                                         
                                                                                
* NEED TO REPLACE SPACES IN FIRST 8 CHARS WITH C'A'                             
                                                                                
         LA    R1,WORK+1                                                        
         LA    R0,7                                                             
*                                                                               
VCML62   CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         MVI   0(R1),C'A'                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,VCML62                                                        
*                                                                               
VCML64   MVC   WORK+12(12),WORK                                                 
         GOTO1 VTRPACK,DMCB,(C'P',WORK+12),WORK   SEE IF IT PACKS               
         BE    VCMLX                                                            
         MVI   ADIDFLAG,C'I'                      SET DOING ISCI                
*                                                                               
VCMLX    MVC   SVCMMLN,5(R2)       SAVE INPUT LENGTH                            
         J     EXIT                                                             
*                                                                               
BADLENER LHI   R0,NOT812                                                        
         STH   R0,GERROR                                                        
         GOTO1 VTRAERR                                                          
*                                                                               
* VALIDATE PERIOD                                                               
*                                                                               
VPER     NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         XC    PEREND,PEREND                                                    
         LA    R3,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R3),WORK                                            
         L     R5,DMCB             GET LENGTH OF FIELD                          
         LTR   R5,R5                                                            
         BZ    DATERR                                                           
         LA    R3,1(R5,R3)         POINT TO END DATE                            
         GOTO1 DATCON,(R1),(0,WORK),(2,PERSTART)                                
         CLM   R5,1,5(R2)          WAS ONLY 1 DATE ENTERED                      
         BE    EXIT                YES, ALL DONE                                
VPER10   GOTO1 DATVAL,DMCB,(R3),WORK                                            
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(2,PEREND)                                  
         CLC   PERSTART,PEREND                                                  
         BH    DATERR                                                           
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
*                                                                               
* VALID OPTIONS ARE  'DETAIL'    OPTDETL  (X80) PRINT DETAIL STATIONS           
*                                                                               
VOPT     NTR1                                                                   
*                                                                               
         MVI   SVOPTSW,0                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VOPT96              NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VOPTHLP             YES                                          
         CLI   5(R2),4                                                          
         BNH   VOPT02                                                           
         LA    R1,3                                                             
         B     VOPT04                                                           
VOPT02   LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
VOPT04   EX    R1,VOPTCLCJ         HELP                                         
         BE    VOPTHLP                                                          
*                                                                               
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
         GOTO1 SCANNER,DMCB,TRAOPTH,(7,(R4))                                    
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
VOPT10   LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,VOPTCLCA         DETAIL                                       
         BNE   VOPT30                                                           
         OI    SVOPTSW,OPTDETL                                                  
*                                                                               
         B     VOPT90                                                           
*                                                                               
VOPT30   EX    R1,VOPTCLCB         TRACE                                        
         BNE   VOPTHLP                                                          
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE, OKAY                             
         BE    *+12                                                             
*                                                                               
         CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL                          
         BNE   VOPTHLP                                                          
*                                                                               
         OI    SVOPTSW,OPTTRCE                                                  
*                                                                               
VOPT90   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         BCT   R3,VOPT10           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
VOPT96   B     EXIT                                                             
*                                                                               
VOPTHLP  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(20),=CL20'* OPTION IS DETAIL *'                          
         B     ERREXIT                                                          
*                                                                               
VOPTCLCA CLC   12(0,R4),=CL6'DETAIL'                                            
VOPTCLCB CLC   12(0,R4),=CL6'TRACE '                                            
VOPTCLCJ CLC   8(0,R2),=CL4'HELP'                                               
         EJECT                                                                  
* GET SORT RECORDS, ELIMINATING EQUALS *                                        
*                                                                               
GSRT     NTR1                                                                   
*                                                                               
GSRT10   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     RF,4(R1)                                                         
         LTR   RF,RF               CK EOF                                       
         BZ    GSRT20               YES                                         
*                                                                               
         MVI   FIRSTSW,1                                                        
         MVC   SVCMLENT,0(RF)                                                   
*                                                                               
         TM    SVOPTSW,OPTTRCE     DO A TRACE                                   
         BZ    EXIT                                                             
*                                                                               
         LR    R3,RF                                                            
         MVC   SVP1,P1                                                          
         MVC   P1,SPACES                                                        
*                                                                               
         MVC   P+1(3),=C'GET'                                                   
         MVC   P+5(4),=C'MKT='                                                  
         SR    RE,RE                                                            
         ICM   RE,3,SVCMLENT                                                    
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+9(4),DUB                                                       
         MVC   P+15(4),=C'SDT='                                                 
         GOTO1 DATCON,DMCB,(2,SVCMLENT+CMLFTD-CMLENT),(5,P+19)                  
         MVC   P+30(4),=C'LTD='                                                 
         GOTO1 DATCON,DMCB,(2,SVCMLENT+CMLLTD-CMLENT),(5,P+34)                  
         MVC   P+50(4),=C'STA='                                                 
         XC    DUB(2),DUB                                                       
         MVC   DUB+2(3),SVCMLENT+CMLSTA-CMLENT                                  
         GOTO1 MSUNPK,DMCB,DUB,BLOCK+4,BLOCK+8                                  
         MVC   P+54(5),BLOCK+8                                                  
         MVC   P+60(4),=C'CML='                                                 
         MVC   P+64(8),SVCMLENT+CMLCML-CMLENT                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BAS   RE,CKPRT            SEE IF HDHK PRINTED MKT                      
*                                                                               
         MVC   P1,SVP1                                                          
*                                                                               
         B     EXIT                                                             
*                                                                               
GSRT20   XC    SVCMLENT,SVCMLENT   SET FOR EOF                                  
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLI   FIRSTSW,0           TEST FIRST TIME                              
         BNE   EXIT                NO                                           
         MVC   P(16),=C'NO INPUT RECORDS'                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         ABEND 999                                                              
         EJECT                                                                  
* READ MARKET RECORD *                                                          
*                                                                               
FMKT     NTR1                                                                   
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
*                                                                               
         MVC   MKTNM,=CL24'**** UNKNOWN ****'                                   
         L     R1,AIO                                                           
         CLC   KEY(8),0(R1)                                                     
         BNE   EXIT                                                             
*                                                                               
         USING MKTRECD,R1                                                       
         MVC   MKTNM,MKTNAME                                                    
         B     EXIT                                                             
         DROP  R1                                                               
FMTSTA   NTR1                                                                   
*                                                                               
         XC    STANET,STANET                                                    
         GOTO1 MSUNPK,DMCB,(X'80',DUB),WORK,WORK+4                              
*                                                                               
         CLC   WORK+9(3),SPACES                                                 
         BE    FMTSTA10                                                         
         MVC   STANET(4),WORK+4                                                 
         MVI   STANET+4,C'/'                                                    
         MVC   STANET+5(3),WORK+9                                               
*                                                                               
FMTSTA10 CLI   WORK+8,C' '                                                      
         BNE   *+8                                                              
         MVI   WORK+8,C'T'                                                      
*                                                                               
* FORMAT STATION FOR PRINTING *                                                 
*                                                                               
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),WORK+4                                                
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),WORK+8                                                   
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    EXIT                                                             
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    EXIT                                                             
         MVI   3(RE),C' '                                                       
         B     EXIT                                                             
         EJECT                                                                  
* FIND COMMERCIAL TITLE *                                                       
*                                                                               
FCML     NTR1                                                                   
         XC    SVCMLDS2,SVCMLDS2                                                
         XC    SVCMLDS3,SVCMLDS3                                                
         MVI   SVCMLDEL,0                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,SVBCLT                                                   
         MVC   CMLKCML,WORK                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    FCML2                                                            
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY(2),=X'0AC1'     TRY FOR ADID                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FCML2    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVC   WORK(8),5(R6)       MOVE IN CASE NO ADID EL                      
         MVC   WORK+8(4),SPACES                                                 
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         MVC   SVCMLDS1(L'CMLTITLE),CMLTITLE                                    
         MVI   SVCMLDS1+L'CMLTITLE,0                                            
         MVC   SVCMLCLT,CMLCLTNO                                                
         TM    CMLSTAT,X'80'       DELETED COMML                                
         BZ    *+8                                                              
         MVI   SVCMLDEL,C'*'                                                    
*                                                                               
         MVI   ELCODE,X'30'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   FCML10                                                           
         MVC   SVCMLDS2,3(R6)                                                   
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   FCML10                                                           
         MVC   SVCMLDS3,3(R6)                                                   
*                                                                               
FCML10   L     R6,AIO                                                           
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FCMLX                                                            
         MVC   WORK(12),CMLADID-CMLADIEL(R6)                                    
*                                                                               
FCMLX    B     EXIT                                                             
         EJECT                                                                  
CKPRT    NTR1                                                                   
         OC    SVHP1,SVHP1                                                      
         BZ    EXIT                                                             
*                                                                               
         MVC   P1,SVHP1                                                         
         MVC   P2,SVHP2                                                         
         MVC   P3,SVHP3                                                         
         MVC   P4,SVHP4                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    SVHP1,SVHP1                                                      
         XC    SVHP2,SVHP2                                                      
         XC    SVHP3,SVHP3                                                      
         XC    SVHP4,SVHP4                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
HDHK     NTR1                                                                   
         MVC   H2+10(1),QMED                                                    
         MVC   H2+15(L'MEDNM),MEDNM                                             
         MVC   H3+10(L'QCLT),QCLT                                               
         MVC   H3+15(L'CLTNM),CLTNM                                             
         MVC   H3+36(8),=C'PERIOD ='                                            
         GOTO1 DATCON,DMCB,(2,PERSTART),(5,H3+45)                               
         MVI   H3+53,C'-'                                                       
         GOTO1 DATCON,DMCB,(2,PEREND),(5,H3+54)                                 
*                                                                               
         CLC   QMKT,PMKT           IS THIS THE MARKET HEADER LINE               
         BNE   HDHK20               NO                                          
         CLC   MKTNM,PMKTNM                                                     
         BE    EXIT                                                             
*                                                                               
HDHK20   DS    0H                                                               
         MVC   SVHP1,P1                                                         
         MVC   SVHP2,P2                                                         
         MVC   SVHP3,P3                                                         
         MVC   SVHP4,P4                                                         
*                                                                               
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
*                                                                               
         MVC   PMKT,QMKT                                                        
         MVC   PMKTNM,MKTNM                                                     
*                                                                               
         B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
LSTSIZER TM    WHEN,X'40'          THIS NOW                                     
         BZ    LSTSIZPT            NO PRINT MESSAGE                             
         MVC   CONHEAD,LSTSIZMS                                                 
         B     ERREXIT                                                          
LSTSIZPT XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         MVC   P+20(33),=CL33'REQUEST REPORT FOR SMALLER PERIOD'                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
CLTREQ   MVC   CONHEAD,CLTREQMS                                                 
         B     ERREXIT                                                          
OLER     MVC   CONHEAD,OLERMS                                                   
ERREXIT  GOTO1 ERREX2                                                           
*                                                                               
CMLENER  MVI   ERROR,INVCMMLN                                                   
         B     TRAPERR                                                          
DATERR   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
BADCOMM  MVI   ERROR,INVCOMM                                                    
         B     TRAPERR                                                          
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
INSBSCEL EQU   15                                                               
INSSUBEL EQU   7                                                                
         EJECT                                                                  
LSTSIZMS DC    CL60'* ERROR * LIST TOO LARGE FOR ONLINE, RUN SMALLER ORC        
                OV *'                                                           
CLTREQMS DC    CL60'* ERROR * CLIENT REQUIRED FOR ONLINE LIST *'                
OLERMS   DC    CL60'* ERROR * ONLINE LIST NOT SUPPORTED *'                      
SORTCARD DC    CL80'SORT FIELDS=(1,25,A),FORMAT=BI '                            
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=25 '                                   
HIATUS   DC    CL6'HIATUS',XL2'00'                                              
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,37,C'COMMERCIAL ACTIVITY REPORT'                              
         SSPEC H2,37,C'--------------------------'                              
         SSPEC H2,77,AGYNAME                                                    
         SSPEC H3,77,AGYADD                                                     
         SSPEC H4,77,RUN                                                        
         SSPEC H5,77,REQUESTOR                                                  
         SSPEC H5,101,PAGE                                                      
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H3,3,C'CLIENT'                                                   
         SSPEC H8,3,C'MARKET'                                                   
         SSPEC H9,3,C'------'                                                   
         SSPEC H8,13,C'MARKET-NAME'                                             
         SSPEC H9,13,C'------------------------'                                
         SSPEC H8,40,C'STATION'                                                 
         SSPEC H9,40,C'-------'                                                 
         SSPEC H8,53,C'COMMERCIAL/TITLE'                                        
         SSPEC H9,53,C'-----------------'                                       
         SSPEC H8,91,C'ACTIVE DATES'                                            
         SSPEC H9,91,C'------------'                                            
*                                                                               
         DC    X'00'               END MARKER FOR SSPECS                        
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPTRINST                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
         DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
* INCLUDED DSECTS                                                               
* INCLUDE DDSPOOLD                                                              
* INCLUDE DDSPLWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRADDD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
* INCLUDED DSECTS                                                               
* INCLUDE SPTRAWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
*                                                                               
* OFFLINE PRINT LINE LAYOUT *                                                   
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL6                                                              
PMKTNM   DS    CL24                                                             
         DS    CL3                                                              
PSTA     DS    CL7                                                              
         DS    CL6                                                              
PCMML    DS    CL12                                                             
         DS    CL1                                                              
PCMMLT   DS    CL15                                                             
         DS    CL10                                                             
PACTDTES DS    CL14                                                             
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
VTRPACK  DS    A                                                                
*                                                                               
CMLTADR  DS    A                                                                
CMLTADRX DS    A                                                                
STATADR  DS    A                                                                
STATADRX DS    A                                                                
*                                                                               
ASVSTOR  DS    A                                                                
*                                                                               
STDCT    DS    CL1                 STANDARD COMML COUNT                         
*                                                                               
CMLCT    DS    F                                                                
STACT    DS    F                                                                
SORTCNT  DS    F                                                                
*                                                                               
SVTCDTS  DS    XL4                 SAVED DATES FROM INST RECAP REC              
*                                                                               
SVMKT    DS    XL2                                                              
SVDTS    DS    XL4                                                              
*                                                                               
SVCMLCMP DS    XL22                                                             
*                                                                               
SVCMLENT DS    XL25                                                             
*                                                                               
SVCMML   DS    CL8                                                              
SVCMMLN  DS    XL1                                                              
SVCMLDS1 DS    CL15                                                             
SVCMLDS2 DS    CL20                                                             
SVCMLDS3 DS    CL20                                                             
SVCMLCLT DS    CL20                                                             
SVCMLDEL DS    CL1                                                              
*                                                                               
SVOPTSW  DS    XL1                                                              
OPTDETL  EQU   X'80'              PRINT ALL STATIONS                            
OPTTRCE  EQU   X'40'              TRACE SORT RECS                               
FIRSTSW  DS    XL1                                                              
*                                                                               
SPTR47RR DS    F                                                                
CMLSTIND DS    F                                                                
*                                                                               
PERSTART DS    XL2                                                              
PEREND   DS    XL2                                                              
*                                                                               
SVBCLT   DS    XL2                                                              
FLDH     DS    XL8                                                              
FLD      DS    XL40                                                             
*                                                                               
MKTCOMCT DS    XL1                                                              
*                                                                               
*MLEQCT  DS    XL1                                                              
*MLEQDT  DS    XL132                                                            
*                                                                               
SVP1     DS    CL132                                                            
SVHP1    DS    CL132                                                            
SVHP2    DS    CL132                                                            
SVHP3    DS    CL132                                                            
SVHP4    DS    CL132                                                            
*                                                                               
CMLSLEN  EQU   CMLNEXT-CMLENT      COMML SORT REC LEN                           
*                                                                               
* CML LIST *                                                                    
*                                                                               
CMLENT   DS   0XL25                                                             
CMLCMP   DS   0XL22                                                             
CMLMKT   DS    XL2                                                              
CMLDTS   DS   0XL4                                                              
CMLFTD   DS    XL2                                                              
CMLLTD   DS    XL2                                                              
CMLCMLS  DS   0CL16                                                             
CMLCML   DS    CL8                                                              
CMLCML2  DS    CL8                                                              
CMLSTA   DS    XL3                                                              
CMLNUMB  DS    XL1                                                              
CMLNEXT  EQU   *                                                                
*                                                                               
* CML TABLE *                                                                   
*                                                                               
CMLTBL   DSECT                                                                  
CMLTENT  DS   0XL(CMLTNEXT-CMLTCMP)                                             
CMLTCMP  DS   0XL20                                                             
CMLTDTS  DS   0XL4                                                              
CMLTFTD  DS    XL2                                                              
CMLTLTD  DS    XL2                                                              
CMLTCMLS DS   0CL16                                                             
CMLTCML  DS    CL8                                                              
CMLTCML2 DS    CL8                                                              
CMLTSTCT DS    XL1                                                              
CMLTNO   DS    XL1                                                              
CMLTNEXT EQU   *                                                                
*                                                                               
* STA TABLE *                                                                   
*                                                                               
STATBL   DSECT                                                                  
STATENT  DS   0XL24                                                             
STATCMP  DS   0XL20                                                             
STATDTS  DS   0XL4                                                              
STATFTD  DS    XL2                                                              
STATLTD  DS    XL2                                                              
STATCMLS DS   0CL16                                                             
STATCML  DS    CL8                                                              
STATCML2 DS    CL8                                                              
STATSTA  DS    XL3                                                              
         DS    XL1                                                              
STATNEXT EQU   *                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055SPTRA47   07/13/11'                                      
         END                                                                    
