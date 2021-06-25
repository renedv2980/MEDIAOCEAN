*          DATA SET PPREP7902  AT LEVEL 039 AS OF 07/09/14                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 044155.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE PP7902A                     **** NOTE "C" PHASE                          
*INCLUDE PPGETSPC                                                               
*INCLUDE PPGETADR                                                               
         TITLE 'PP7902 - SHIPPING LIST'                                         
*   CHANGE LOG                                                                  
*                                                                               
*  BPLA  2/14    CHANGE FOR MEDIA L - SOCIAL                                    
*                                                                               
*  BPLA  9/98    CHANGE FOR MEDIA I - INTERACTIVE                               
*                                                                               
*  SMYE 4/16/97  USE CALL TO PPGETADR TO GET ADDRESSES (IN GETPUB)              
*                                                                               
*  BPLA 2/21/97  FIX BUG - IF INCLUDING OVERAGE                                 
*                AND ROUNDED RESULT IS THE SAME - RESET                         
*                DUB BEFORE GOING TO LBLADR                                     
*                NOTE - DUB IS NORMALLY SET BY THE EDIT MACRO                   
*                                                                               
*  SMYE 12/13/95 CHANGED DTCNV TO DATCON WITH NEW PARAM'S                       
*                                                                               
*  BPLA 9/94     CHANGES FOR GETTING OVERAGE PCT. FROM THE NEW                  
*                SPACE RECORD  (USING PPGETSPC)                                 
*                SHIPPING LIST ELEMENT EXPANDED TO CARRY                        
*                OVERAGE PERCENTAGE                                             
*                                                                               
*  SMUR 6/20/94  SPACE DESCRIPTION ( LINES X COLS /INCHES                       
*                                    PREM= ) AND RPT=                           
*                                                                               
*  BPLA 2/4/93   MOVE NUMBER OF POSTER OVER 3 BYTES                             
*                SO REF ELEM DATA CAN BE DISPLAYED                              
*                ALSO ADD QOPT4 Y=SPACE BETWEEN BUYS                            
*                                                                               
*  BPLA 3/31/92  QOPT1 - NEW VALUES                                             
*                        OVERAGE IS NOW A PERCENTAGE 0-100 (BINARY)             
*                        N = NO OVERAGE                                         
*                        Y = 10 PCT OVERAGE                                     
*                                                                               
*   BPLA 1/4/91  FIX THE SEARCH TO USE MOST SPECIFIC CLIENT= REP OR             
*                ADDR,TRA - AFTER FIRST CHECKING FOR ADDR,SHIP                  
*                                                                               
*   BPLA 11/1/91 LOOK FIRST FOR SHIPPING ADDRESS                                
*                IF FOUND ALWAYS USE - IF NOT SEARCH AS I DID BEFORE            
*                                                                               
*   BPLA 3/4/91 ADD NEW OPTIONS FOR 15 AND 20 PCT OVERAGE                       
*                                                                               
*    QOPT1  Y = INCLUDE 10 PCT OVERAGE                                          
*           1 = INCLUDE 15 PCT OVERAGE                                          
*           2 = INCLUDE 20 PCT OVERAGE                                          
*                                                                               
*    QOPT2  Y = SUPPRESS LABELS                                                 
*                                                                               
*    QOPT3  A = TRAFFIC ADDRESS SORT (MARKET SORT WITHIN TRA ADDR)              
*           M = MARKET SORT (ILLOGICAL IF QOPT6=N)                              
*                                                                               
*    QOPT4  Y = DOUBLE SPACE BETWEEN BUYS                                       
*                                                                               
*    QOPT6  Y = SHOW MKTS/VENDORS                                               
*                                                                               
*    QOPT7  Y = TEST RUN (DON'T MARK FILE)                                      
*                                                                               
PP7902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP7902,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         SPACE 2                                                                
         LA    R7,1(RB)                                                         
         LA    R7,4095(R7)                                                      
         USING PP7902+4096,R7                                                   
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP79WRKD,R8                                                      
*                                                                               
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         MVC   ACONIO1,ACONIO      GET AND STORE (A)PCONREC IN ACONIO1          
         DROP  RF                                                               
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    FIRST                                                            
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
         CLI   MODE,UNSRTREC                                                    
         BNE   EXIT                                                             
         MVC   SAVPARS,DMCB+4                                                   
         LM    R2,R3,SAVPARS                                                    
         B     TLBRTAB(R2)                                                      
*                                                                               
TLBRTAB  B     TLFIRST                                                          
         B     TLINPUT                                                          
         B     TLOUTPUT                                                         
         B     TLLAST                                                           
         SPACE 3                                                                
TLFIRST  DS    0H                                                               
*                                                                               
*                                                                               
         MVC   P,SPACES                                                         
         MVI   CMSW,0                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         XC    OLDKEY,OLDKEY                                                    
         XC    OLDJOB,OLDJOB                                                    
*                                                                               
         ZAP   MKTTOTS,=P'0'        POSTER OR INSERTION TOTALS                  
         ZAP   ADRTOTS,=P'0'                                                    
         ZAP   JOBTOTS,=P'0'                                                    
*                                                                               
         ZAP   MKTOTOTS,=P'0'       TOTALS INCLUDING OVERAGE                    
         ZAP   ADROTOTS,=P'0'       PERCISION IS 2 DECIMALS                     
         ZAP   JOBOTOTS,=P'0'       WILL BE ROUNDED WHEN PRINTED                
*                                                                               
         CLI   QOPT1,C' '                                                       
         BNE   *+8                                                              
         MVI   QOPT1,C'N'          SET DEFAULT                                  
*                                                                               
         XC    SVPL1(120),SVPL1                                                 
*                                  IS SORT NECESSARY?                           
         CLC   QCLIENT,SPACES                                                   
         BE    TLF6                                                             
         CLC   QPRODUCT,SPACES                                                  
         BE    TLF6                                                             
         CLC   QJOB(5),=C'ALL  '                                                
         BE    TLF6                                                             
         CLI   QOPT3,C'A'          TRAFFIC ADDRESS SORT                         
         BE    TLF6                                                             
         CLI   QOPT3,C'M'          MARKET SORT                                  
         BE    TLF6                                                             
TLF1     DS    0H                                                               
*                                  NO SORT                                      
         MVI   RUNSW,X'20'                                                      
         XC    KEY,KEY                                                          
         XC    SAVPARS,SAVPARS                                                  
TLF2     DS    0H                                                               
         BAS   RE,TLIN                                                          
         CLI   SAVPARS+3,8            END                                       
         BNE   TLF2A                                                            
         MVI   PBUYREC,X'FF'                                                    
         MVC   PBUYREC+1(24),PBUYREC                                            
         MVC   PBDJOB,=6X'FF'                                                   
TLF2A    DS    0H                                                               
         BAS   RE,TLOUT                                                         
         CLI   PBUYREC,X'FF'                                                    
         BNE   TLF2                                                             
         XC    SAVPARS,SAVPARS                                                  
         B     EXIT                                                             
TLF6     DS    0H                                                               
         MVI   RUNSW,0                                                          
         MVC   SAVPARS+04(2),=C'92'   SORT REC LEN                              
         MVC   SAVPARS+06(2),=C'88'   SORT KEY LEN                              
         XC    KEY,KEY                                                          
         XC    PBUYREC(256),PBUYREC                                             
         XC    JADNO(73),JADNO         CLEAR JOB REC FIELDS                     
         XC    PNAME(110),PNAME       CLEAR PROD HOUSE FIELDS                   
         XC    SRTLIN,SRTLIN                                                    
         B     EXIT                                                             
         SPACE 3                                                                
TLINPUT  DS    0H                                                               
         BAS   RE,TLIN                                                          
         B     EXIT                                                             
         SPACE 3                                                                
TLOUTPUT DS    0H                                                               
         BAS   RE,TLOUT                                                         
         B     EXIT                                                             
         SPACE 3                                                                
TLLAST   DS    0H                                                               
         MVI   PBUYREC,X'FF'                                                    
         MVC   PBUYREC+1(24),PBUYREC                                            
         MVC   PBDJOB,=6X'FF'                                                   
         BAS   RE,TLOUT                                                         
         B     EXIT                                                             
EXIT     DS    0H                                                               
         MVC   DMCB+4(8),SAVPARS                                                
XIT      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                  INPUT                                        
TLIN     NTR1                                                                   
         SPACE 2                                                                
         OC    KEY,KEY                                                          
         BNZ   TLIN10                                                           
*                                       FIRST TIME                              
         XC    BSTART,BSTART                                                    
         MVC   BEND,=3X'FF'                                                     
         CLI   QSTART,C' '                                                      
         BE    TLIN2                                                            
*        GOTO1 DTCNV,DMCB,QSTART,(1,BSTART)                                     
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
*                                                                               
TLIN2    DS    0H                                                               
         CLI   QEND,C' '                                                        
         BE    TLIN4                                                            
*        GOTO1 DTCNV,DMCB,QEND,(1,BEND)                                         
         GOTO1 DATCON,DMCB,(0,QEND),(3,BEND)                                    
*                                                                               
TLIN4    DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,SVTODAY)                                    
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         LA    R5,WORK                                                          
         CLC   QPAY(6),SPACES      CONTROL DATE SPECIFIED                       
         BE    *+8                                                              
         LA    R5,QPAY                                                          
***      GOTO1 DTCNV,DMCB,(R5),(1,CONDAT)                                       
         GOTO1 DATCON,DMCB,(0,(R5)),(3,CONDAT)                                  
*                                                                               
         MVC   KEY(3),RCSVAGY                                                   
         MVI   KEY+3,X'20'                                                      
         CLC   QCLIENT,SPACES                                                   
         BE    TLIN6                                                            
         CLC   QCLIENT,=C'ALL'                                                  
         BE    TLIN6                                                            
         CLI   QCLIENT,C'*'                                                     
         BE    TLIN6                                                            
         MVC   KEY+4(3),QCLIENT                                                 
*                                                                               
         CLC   QPRODUCT,SPACES                                                  
         BE    TLIN6                                                            
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    TLIN6                                                            
         MVC   KEY+7(3),QPRODUCT                                                
*                                                                               
TLIN6    DS    0H                                                               
*                                                                               
TLIN8    DS    0H                                                               
         BAS   RE,MYHIGH                                                        
         B     TLIN10A                                                          
*                                                                               
TLIN10   DS    0H                                                               
         BAS   RE,MYSEQ                                                         
TLIN10A  DS    0H                                                               
         CLI   KEY+25,X'FF'                                                     
         BE    TLIN10                                                           
         CLC   KEY(4),KEYSAVE      A/M/R                                        
         BNE   TLIN90                                                           
         CLC   QCLIENT,SPACES                                                   
         BE    TLIN12                                                           
         CLC   QCLIENT,=C'ALL'                                                  
         BE    TLIN12                                                           
         CLI   QCLIENT,C'*'        TEST OFFICE REQ                              
         BNE   TLIN10C             NO                                           
         MVC   PBUYKEY,KEY                                                      
         BAS   RE,GETCLT                                                        
         CLC   QCLIENT+1(1),PCLTOFF                                             
         BNE   TLIN10                                                           
         B     TLIN12                                                           
TLIN10C  DS    0H                                                               
         CLC   QCLIENT,KEY+4                                                    
         BNE   TLIN90                                                           
*                                                                               
         CLC   QPRODUCT,SPACES                                                  
         BE    TLIN12                                                           
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    TLIN12                                                           
         CLC   KEY+7(3),QPRODUCT                                                
         BNE   TLIN90                                                           
*                                                                               
TLIN12   DS    0H                                                               
         OC    KEY+21(3),KEY+21    PASSIVE POINTER                              
         BNZ   TLIN10                                                           
         B     TLIN14                                                           
*                                                                               
TLIN14   DS    0H                                                               
         CLC   QEST,=C'ALL'                                                     
         BE    TLIN15                                                           
         CLC   QEST,SPACES                                                      
         BE    TLIN15                                                           
         MVC   HALF,KEY+19                                                      
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         CLC   WORK(3),QEST                                                     
         BE    TLIN15                                                           
         BL    TLIN10                                                           
         CLC   QESTEND,SPACES                                                   
         BE    TLIN10                                                           
         CLC   WORK(3),QESTEND                                                  
         BH    TLIN10                                                           
*                                                                               
TLIN15   DS    0H                                                               
         CLC   BSTART,KEY+16                                                    
         BH    TLIN10                                                           
         CLC   BEND,KEY+16                                                      
         BL    TLIN10                                                           
*                                                                               
TLIN16   DS    0H                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,MYAREC                                                        
         BAS   RE,GETR                                                          
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    TLIN10                                                           
         CLC   QJOB,=C'ALL   '                                                  
         BNE   TLIN16A                                                          
         OC    PBDJOB,PBDJOB          NO JOB NUMBER                             
         BZ    TLIN10                                                           
         B     TLIN18                                                           
*                                                                               
TLIN16A  CLC   QJOB,PBDJOB                                                      
         BE    TLIN18                                                           
         CLC   =C'NONE',QJOB                                                    
         BNE   TLIN10                                                           
         OC    PBDJOB,PBDJOB                                                    
         BNZ   TLIN10                                                           
*                                                                               
TLIN18   DS    0H                                                               
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ECODE,X'79'                                                      
         USING PSHIPELD,R2                                                      
TLIN18A  BAS   RE,NEXTEL                                                        
         BNE   TLIN19                                                           
         CLC   PBDJOB,PSHIJOB                                                   
         BNE   TLIN18A                                                          
         CLC   PSHIDATE,CONDAT     SEE IF IT WAS ON A PREVIOUS LIST             
         BH    TLIN18A                                                          
         B     TLIN10                                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
TLIN19   DS    0H                                                               
         MVC   SVBUYADR,KEY+27         SAVE DISKADDR                            
         CLI   RUNSW,0                                                          
         BNE   TLINX               NO SORT                                      
         L     R3,SAVPARS+4                                                     
         USING SORTRECD,R3                                                      
         XC    SORTREC,SORTREC                                                  
         MVC   SKCLT,KEY+4                                                      
*                                                                               
*                                                                               
TLIN22   DS    0H                                                               
         MVC   SKPRD,KEY+7                                                      
         MVC   SKJOB,PBDJOB                                                     
*                                                                               
*                                                                               
TLIN23   DS    0H                                                               
         MVC   SKPUB(6),KEY+10                                                  
         CLI   QOPT3,C'A'          TRAFFIC ADDRESS                              
         BE    TLIN23C                                                          
         CLI   QOPT3,C'M'          OR MARKET SORT                               
         BNE   TLIN24                                                           
TLIN23C  CLC   OLDPUB,SKPUB                                                     
         BE    TLIN23H                                                          
         MVC   OLDPUB,SKPUB                                                     
         BAS   RE,GETPUB                                                        
TLIN23H  DS    0H                                                               
         XC    SKPUB,SKPUB                                                      
         CLI   QOPT3,C'M'          SEE IF DOING MARKET SORT                     
         BE    TLIN23K             YES SKIP TRAFFIC ADDR                        
         MVC   SKPUB,PUBL1                                                      
         CLC   SKPUB(4),=C'THE '                                                
         BNE   *+10                                                             
         MVC   SKPUB(24),PUBL1+4                                                
         MVC   SKPUB1(10),PUBL2            TO S                                 
*                                                                               
TLIN23K  MVC   SKMKT,PUBL1+36          CITY,STATE OR ZONE                       
         MVC   SKPUB2,PUBKPUB                                                   
         CLI   QMEDIA,C'N'                                                      
         BE    TLIN24                                                           
         CLI   QMEDIA,C'O'                                                      
         BE    TLIN24                                                           
         MVC   SKMKT(6),PUBKPUB         FOR MAGS USE PUB NUMBER                 
         XC    SKMKT+6(14),SKMKT+6                                              
*                                                                               
*                                                                               
TLIN24   DS    0H                                                               
         MVC   SKDAT2,KEY+16                                                    
         LH    R1,SRTLIN                                                        
         LA    R1,1(R1)                                                         
         STH   R1,SRTLIN                                                        
         MVC   SKLIN,SRTLIN        PRESERVE ORIG SEQ                            
*                                                                               
         MVC   SRECDA,KEY+27                                                    
         B     TLINX                                                            
*                                                                               
TLIN90   DS    0H                                                               
         MVI   SAVPARS+3,8                                                      
         CLI   RUNSW,0                 NO SORT                                  
         BNE   TLINX                                                            
         XC    OLDPUB,OLDPUB       IF SORTING MUST CLEAR OLDPUB                 
         XC    OLDJOB,OLDJOB             AND OLDJOB                             
         B     TLINX                                                            
*                                                                               
TLINX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         SPACE 2                                                                
*                                  OUTPUT                                       
TLOUT    NTR1                                                                   
         SPACE 2                                                                
         USING SORTRECD,R3                                                      
         CLI   RUNSW,0                                                          
         BNE   TLOUT2                                                           
         CLI   PBUYREC,X'FF'                                                    
         BE    TLOUT2                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,MYAREC                                                        
         MVC   KEY+27(4),SRECDA                                                 
         BAS   RE,GETR                                                          
*                                                                               
TLOUT2   DS    0H                                                               
         CLC   PBDJOB,OLDJOB                                                    
         BNE   TLOUT2A                                                          
         CLC   PBUYKCLT(6),OLDCLT        CHECK CHANGE OF CLT/PRD                
         BE    TLOUT4                                                           
TLOUT2A  BAS   RE,PUBEND                                                        
         BAS   RE,ADDREND                                                       
         BAS   RE,JOBEND                                                        
         CLI   PBUYREC,X'FF'                                                    
         BE    TLOUTX          EOF REC                                          
         BAS   RE,MYGETJ                                                        
         BAS   RE,NEWJOB                                                        
*                                                                               
TLOUT4   CLC   PBUYKPUB(6),OLDPUB                                               
         BE    TLOUT6                                                           
         BAS   RE,PUBEND                                                        
         CLI   PBUYREC,X'FF'                                                    
         BE    TLOUTX          EOF REC                                          
         MVC   SAVEPLNS,PUBL1           SAVE OLD PUB LINES                      
         BAS   RE,GETPUB                                                        
         CLC   SVPL1,PUBL1           CHECK FOR DIFFERENT SHIPPING ADDR          
         BNE   TLOUT4E                                                          
         CLC   SVPL2,PUBL2                                                      
         BNE   TLOUT4E                                                          
         CLC   SVPL3,PUBL3                                                      
         BNE   TLOUT4E                                                          
         CLC   SVPL4,PUBL4                                                      
         BNE   TLOUT4E                                                          
*                 ONLY PRINT MKT AND PUB NUMBER                                 
         CLI   QOPT3,C'A'                                                       
         BNE   TLOUT4C                                                          
*                                                                               
         LA    R4,SAVEPLNS         SEE IF ALL OLD PUBLNS WERE PRINTED           
         LA    R5,4                                                             
TLOUT4A  CLI   0(R4),0                                                          
         BNE   TLOUT4B                                                          
         LA    R4,60(R4)                                                        
         BCT   R5,TLOUT4A                                                       
         B     TLOUT4C             YES - SO PRINT DITTOS                        
*                                                                               
TLOUT4B  MVC   PUBL1(240),SAVEPLNS   RESTORE OLD PUB LINES                      
         B     TLOUT6                                                           
*                                                                               
TLOUT4C  XC    PUBL1+1(29),PUBL1+1                                              
         XC    PUBL2+1(29),PUBL2+1                                              
         MVI   PUBL3,0                                                          
         MVI   PUBL4,0                                                          
         MVI   PUBL1,C' '                                                       
         MVC   PUBL1+10(2),=C''''''                                             
         CLI   QOPT3,C'A'                                                       
         BNE   TLOUT4D                                                          
         MVC   PUBL1+10(2),=C'  '                                               
         MVC   PUBL2+10(2),=C''''''    PUT DITTOS IN LINE 2                     
         CLI   QOPT6,C'N'          SEE IF SHOWING MKTS/VENDORS                  
         BNE   TLOUT4D             YES                                          
         XC    PUBL2+10(2),PUBL2+10    NO - CLEAR DITTOS                        
TLOUT4D  MVI   PUBL2,C' '                                                       
         MVI   LINENEED,3                                                       
         B     TLOUT6                                                           
*                                                                               
TLOUT4E  DS    0H                                                               
         MVC   SAVENEW,PUBL1       SAVE NEW PUB LINES                           
         MVC   PUBL1(240),SAVEPLNS    RESTORE OLD LINES FOR ADDREND             
         BAS   RE,ADDREND                                                       
         MVC   PUBL1(240),SAVENEW      RESTORE NEW PUB LINES                    
         MVC   SVPL1,PUBL1           SAVE SHIPPING ADDR                         
         MVC   SVPL2,PUBL2                                                      
         MVC   SVPL3,PUBL3                                                      
         MVC   SVPL4,PUBL4                                                      
TLOUT4X  MVI   LINENEED,4                                                       
*                                                                               
TLOUT6   DS    0H                                                               
*        GOTO1 DTCNV,DMCB,(1,PBUYKDAT),(3,P+63)                                 
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(5,P+63)                                
         XC    SAVREF,SAVREF                                                    
         LA    R2,PBUYREC+33                                                    
         MVI   ECODE,X'83'                                                      
         BAS   RE,NEXTEL                                                        
         BNE   TLOUT6A                                                          
         USING PBREFELD,R2                                                      
         MVC   SAVREF(4),=C'REF='                                               
         MVC   SAVREF+4(L'PBREFNO),PBREFNO                                      
*                                                                               
         DROP  R2                                                               
*                                                                               
TLOUT6A  XC    SAVRPT,SAVRPT       REPAINT                                      
         LA    R2,PBUYREC+33                                                    
         MVI   ECODE,X'85'                                                      
         BAS   RE,NEXTEL                                                        
         BNE   TLOUT6B                                                          
         USING PBRPTELD,R2                                                      
         MVC   SAVRPT(5),=C'RPT= '                                              
         EDIT  (P3,PBRPTNO),(3,SAVRPT+5),ALIGN=LEFT,ZERO=NOBLANK                
*                                                                               
         DROP  R2                                                               
*                                                                               
TLOUT6B  DS    0H                                                               
         MVI   BUYOVR,0               BUY'S OVERAGE  PCT.                       
         ZAP   BUYTOT,=P'0'                                                     
         ZAP   BUYOTOT,=P'0'                                                    
*                                                                               
         CLI   QMEDIA,C'O'            CHECK OUTDOOR                             
         BNE   TLOUT6H                                                          
         CLI   PBDSPACE,X'FF'                                                   
         BNE   TLOUT6J                                                          
         ZAP   BUYTOT,PBDREG                                                    
         AP    BUYTOT,PBDILLUM                                                  
         EDIT  (P4,BUYTOT),(4,P+80),0                                           
         AP    MKTTOTS,BUYTOT                                                   
*                                                                               
         ZAP   BUYOTOT,BUYTOT                                                   
         MP    BUYOTOT,=P'100' SINCE OVERAGE TOTALS ARE 2 DECIMALS              
         CLI   QOPT1,C'N'    SEE IF NO OVERAGE                                  
         BE    TLOUT6E                                                          
         CLI   QOPT1,0               O PCT OVERAGE REQUESTED                    
         BE    TLOUT6E                                                          
         BAS   RE,CALCOVR           GO CALCULATE OVERAGE                        
*                                   AND RETURN IN BUYOTOT (2 DECIMALS)          
*                                                                               
TLOUT6E  DS    0H                                                               
         AP    MKTOTOTS,BUYOTOT      ROLL TO OVERAGE TOTALS                     
         B     TLOUT6J               (2 DECIMALS)                               
*                                                                               
TLOUT6H  DS    0H                                                               
         AP    MKTTOTS,=P'1'         COUNT INSERTIONS                           
TLOUT6J  DS    0H                                                               
*                                                                               
         CLI   RCWRITE,C'N'        WRITE=NO                                     
         BE    *+8                                                              
         BAS   RE,MARKREC                                                       
*                                                                               
         BAS   RE,GETPLN                                                        
         BAS   RE,TLPRT                                                         
*                                                                               
         CLI   SAVREF,0    SEE IF I NEED TO PRINT REFERENCE NUMBER              
         BE    TLOUT7                                                           
         MVC   P+64(15),SAVREF                                                  
         BAS   RE,GETPLN                                                        
         BAS   RE,TLPRT                                                         
*                                                                               
TLOUT7   CLI   SAVRPT,0    SEE IF I NEED TO PRINT NUMBER OF REPAINT             
         BE    TLOUTX                                                           
         MVC   P+64(8),SAVRPT                                                   
         BAS   RE,GETPLN                                                        
         BAS   RE,TLPRT                                                         
*                                                                               
TLOUTX   DS    0H                                                               
         CLI   QOPT4,C'Y'      SEE IF DOUBLE SPACING                            
         BNE   TLOUTX5                                                          
         BAS   RE,GETPLN      SKIP A LINE                                       
         BAS   RE,TLPRT                                                         
*                                                                               
TLOUTX5  MVC   OLDKEY,PBUYKEY                                                   
         MVC   OLDJOB,PBDJOB                                                    
         B     XIT                                                              
         SPACE 3                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ECODE,0(R2)                                                      
         BER   RE                                                               
         B     NEXTEL+2                                                         
NEXTEL2  LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
JOBEND   NTR1                                                                   
         CP    JOBTOTS,=P'0'                                                    
         BE    JOBENDX                                                          
         MVC   P+72(5),=C'TOTAL'                                                
         EDIT  (P4,JOBTOTS),(6,P+78),0                                          
         MVC   P+85(7),=C'POSTERS'                                              
         CP    JOBTOTS,=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+91,C' '                                                        
         CLI   QMEDIA,C'O'                                                      
         BE    JOBEND2                                                          
         MVC   P+85(10),=C'INSERTIONS'                                          
         CP    JOBTOTS,=P'1'                                                    
         BNE   JOBEND2                                                          
         MVI   P+94,C' '                                                        
*                                                                               
JOBEND2  DS    0H                                                               
         CLI   QMEDIA,C'O'       SEE IF OUTDOOR                                 
         BNE   JOBEND2D                                                         
         CLI   QOPT1,C'N'                                                       
         BE    JOBEND2D                                                         
         CLI   QOPT1,0            ZERO PERCENTAGE                               
         BE    JOBEND2D                                                         
         ZAP   MYDUB,JOBOTOTS                                                   
         AP    MYDUB,=P'50'       FOR ROUNDING                                  
         MVN   MYDUB+6(1),MYDUB+7     MOVE SIGN                                 
         CP    JOBTOTS,MYDUB(7)                                                 
         BE    JOBEND2D            SEE IF EQUAL - NO OVERAGE                    
*                                                                               
         MVC   PSECOND+64(12),=C'PLUS OVERAGE'                                  
         LA    R4,PSECOND+78                                                    
         EDIT  (P7,MYDUB),(6,0(R4)),0                                           
         MVC   PSECOND+85(7),=C'POSTERS'  SHOULD ALWAYS BE MORE THAN 1          
*                                                                               
JOBEND2D MVC   LSW,MAXLINES                                                     
         MVI   MAXLINES,99             PRINT TOTAL ON SAME PAGE                 
         BAS   RE,TLPRT                                                         
         MVC   MAXLINES,LSW            RESTORE MAXLINES                         
*                                                                               
JOBENDX  ZAP   JOBTOTS,=P'0'                                                    
         ZAP   JOBOTOTS,=P'0'                                                   
         XIT1                                                                   
         EJECT                                                                  
PUBEND   NTR1                                                                   
         OC    OLDPUB,OLDPUB                                                    
         BE    PUBENDX                                                          
         ZAP   DUB,=P'0'                                                        
         CLI   QMEDIA,C'O'                                                      
         BNE   PUBEND4                                                          
         CP    MKTTOTS,=P'0'                                                    
         BE    PUBEND4                                                          
         JIF   QOPT6,=,C'N',AND,QOPT3,=,C'A',PUBENDX,JUMP=N                     
         JIF   QOPT6,=,C'N',AND,QOPT3,=,C'M',PUBENDX,JUMP=N                     
         CLI   QOPT3,C'A'          SORTING ON ADDR - -DON'T PRINT               
         BE    PUBEND4B             MARKET/PUB TOTALS                           
         EDIT  (P4,MKTTOTS),(5,P+79),0                                          
         MVI   P+84,C'*'                                                        
         CLI   QOPT1,C'N'          DON'T INCLUDE 10 PCT OVERAGE                 
         BE    PUBEND2                                                          
*                                                                               
         ZAP   DUB,MKTOTOTS                                                     
         AP    DUB,=P'50'      FOR ROUNDING                                     
         MVN   DUB+6(1),DUB+7                                                   
         CP    MKTTOTS,DUB(7)                                                   
         BNE   PUBEND1                                                          
         ZAP   DUB,MKTTOTS      RESET DUB FOR LBLADR                            
         B     PUBEND2                                                          
*                                                                               
PUBEND1  DS    0H                                                               
         MVC   P+87(10),=C'+ OVERAGE='                                          
         ZAP   MYDUB,MKTOTOTS                                                   
         AP    MYDUB,=P'50'     FOR ROUNDING                                    
         MVN   MYDUB+6(1),MYDUB+7      MOVE SIGN                                
         EDIT  (P7,MYDUB),(5,P+100),0                                           
*                                                                               
*                                                                               
PUBEND2  DS    0H                                                               
         BAS   RE,LBLADR                                                        
         BAS   RE,GETPLN                                                        
         CLI   LSW,1               SEE IF I HAVE SOMETHING TO PRINT             
         BNE   PUBEND4B                                                         
         MVC   LSW,MAXLINES        SAVE MAXLINES                                
         MVI   MAXLINES,99                                                      
         BAS   RE,TLPRT                                                         
         MVC   MAXLINES,LSW        RESTORE MAXLINES                             
*                                                                               
         B     PUBEND4B                                                         
PUBEND4  DS    0H                                                               
         BAS   RE,LBLADR                                                        
PUBEND4B DS    0H                                                               
         BAS   RE,GETPLN                                                        
         CLI   LSW,1                                                            
         BNE   PUBEND6                                                          
         BAS   RE,TLPRT                                                         
         B     PUBEND4B                                                         
*                                                                               
PUBEND6  DS    0H                                                               
         CLI   QOPT3,C'A'                                                       
         BE    PUBENDX             NO SKIPPING                                  
         BAS   RE,TLPRT       SKIP A LINE                                       
*                                                                               
PUBENDX  DS    0H                                                               
         AP    ADRTOTS,MKTTOTS        ROLL TO ADR TOTALS                        
         AP    ADROTOTS,MKTOTOTS      ROLL OVERAGE TOTALS                       
*                                    NOTE - STILL 2 DECIMALS                    
         ZAP   MKTTOTS,=P'0'                                                    
         ZAP   MKTOTOTS,=P'0'                                                   
         XIT1                                                                   
         EJECT                                                                  
ADDREND  NTR1                      END OF SHIPPING ADDRESS                      
         MVC   P,SPACES                                                         
         OC    SVPL1,SVPL1         FIRST TIME                                   
         BZ    ADDRX                                                            
         ZAP   DUB,=P'0'                                                        
         CLI   QOPT3,C'A'          SEE IF SORTING ON ADDR                       
         BNE   ADDR4B                                                           
         CLI   QMEDIA,C'O'                                                      
         BNE   ADDR4                                                            
         CP    ADRTOTS,=P'0'                                                    
         BE    ADDR4                                                            
         EDIT  (P4,ADRTOTS),(5,P+79),0                                          
         MVI   P+84,C'*'                                                        
         CLI   QOPT1,C'N'          DON'T INCLUDE 10 PCT OVERAGE                 
         BE    ADDR2                                                            
*                                                                               
         ZAP   DUB,ADROTOTS                                                     
         AP    DUB,=P'50'      FOR ROUNDING                                     
         MVN   DUB+6(1),DUB+7                                                   
         CP    ADRTOTS,DUB(7)       SEE IF THE SAME                             
         BNE   ADDR1                                                            
         ZAP   DUB,ADROTOTS        MUST RESET DUB FOR LBLADR                    
         B     ADDR2                                                            
*                                                                               
ADDR1    DS    0H                                                               
         MVC   P+87(11),=C'+ OVERAGE='                                          
         ZAP   MYDUB,ADROTOTS                                                   
         AP    MYDUB,=P'50'    FOR ROUNDING                                     
         MVN   MYDUB+6(1),MYDUB+7      MOVE SIGN                                
         EDIT  (P7,MYDUB),(5,P+100),0                                           
*                                                                               
*                                                                               
ADDR2    DS    0H                                                               
         BAS   RE,LBLADR                                                        
         BAS   RE,GETPLN                                                        
         CLI   LSW,1                                                            
         BNE   ADDR4B                                                           
         MVC   LSW,MAXLINES                                                     
         MVI   MAXLINES,99                                                      
         BAS   RE,TLPRT                                                         
         MVC   MAXLINES,LSW                                                     
         B     ADDR4B                                                           
*                                                                               
ADDR4    DS    0H                                                               
         BAS   RE,LBLADR                                                        
ADDR4B   DS    0H                                                               
         BAS   RE,GETPLN                                                        
         CLI   LSW,1                                                            
         BNE   ADDR6                                                            
         BAS   RE,TLPRT                                                         
         B     ADDR4B                                                           
*                                                                               
ADDR6    DS    0H                                                               
         MVI   SPACING,2                                                        
         BAS   RE,TLPRT            SKIP                                         
*                                                                               
ADDRX    DS    0H                                                               
         AP    JOBTOTS,ADRTOTS                                                  
         AP    JOBOTOTS,ADROTOTS                                                
         ZAP   ADRTOTS,=P'0'                                                    
         ZAP   ADROTOTS,=P'0'                                                   
         XIT1                                                                   
         EJECT                                                                  
*                                  WRITE LABEL ADDRESS REC                      
LBLADR   NTR1                                                                   
         SPACE 2                                                                
         CLI   QOPT2,C'Y'          Y=SUPPRESS LABELS                            
         BE    LBLADRX                                                          
         MVC   SLAREC,SPACES                                                    
         LA    RF,L'SLAREC+4                                                    
         SLL   RF,16                                                            
         ST    RF,SLAREC-4                                                      
         MVI   SLACOD,C'A'                                                      
         MVC   SLANAM(120),SVPL1                                                
******                                                                          
******  NOTE-DUB MAY HAVE BEEN SET BY EDIT MACRO IN CALLING ROUTINE             
******                                                                          
         ZAP   SLACOUNT,DUB                                                     
         DS    0H                                                               
         GOTO1 WORKER,DMCB,=C'ADD',PP79FLN,FILID,SLAREC-4                       
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
LBLADRX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
MYGETJ   NTR1         GET JOB REC AND PROD HOUSE                                
         MVC   SAVKEYS,KEY                                                      
         XC    JSPC,JSPC                                                        
         XC    JADNO(73),JADNO                                                  
         XC    PNAME(110),PNAME                                                 
         CLC   =C'NONE',QJOB                                                    
         BE    GETJX                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(3),RCSVAGY                                                   
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+4(3),PBUYKCLT                                                
         MVC   KEY+7(3),PBUYKPRD                                                
         MVC   KEY+10(6),PBDJOB                                                 
         BAS   RE,MYHIGH                                                        
         CLC   KEYSAVE(17),KEY                                                  
         BE    *+6                                                              
         DC    H'0'        JOB REC NOT ON FILE                                  
         LA    R0,PJOBREC                                                       
         ST    R0,MYAREC                                                        
         BAS   RE,GETR                                                          
         MVC   JADNO,PJOBKJOB                                                   
         MVC   JCOPY,PJOBCPY                                                    
         MVC   JCAP1,PJOBCAP1                                                   
         MVC   JCAP2,PJOBCAP2                                                   
*                                                                               
         CLC   PJOBSPC(17),SPACES   ANYTHING IN JOB DESC ?                      
         BH    SPCDSC4                                                          
         OC    PJOBUNTS,PJOBUNTS    ANY UNITS SPECIFIED ?                       
         BZ    PROD                 NO                                          
         CLI   PJOBUIND,X'89'                                                   
         BNE   SPCDSC1                                                          
         EDIT  (P3,PJOBUNTS),(7,JSPC),2,COMMAS=YES,ALIGN=LEFT                   
         MVC   JSPC+8(6),=C'INCHES'                                             
         B     SPCDSC3                                                          
*                                                                               
SPCDSC1  EDIT  (P3,PJOBUNTS),(6,JSPC),COMMAS=YES,ALIGN=LEFT                     
*                                                                               
         LA    R1,6                                                             
         LA    R2,JSPC                                                          
SPCDSC1C CLI   0(R2),C' '                                                       
         BNH   SPCDSC1F                                                         
         LA    R2,1(R2)                                                         
         BCT   R1,SPCDSC1C                                                      
*                                                                               
SPCDSC1F CLI   PJOBUIND,C'L'                                                    
         BNE   *+18                                                             
         MVC   1(5,R2),=C'LINES'                                                
         LA    R2,6(R2)                                                         
         B     SPCDSC3                                                          
*                                                                               
         CLI   PJOBUIND,C'P'                                                    
         BNE   *+18                                                             
         MVC   1(5,R2),=C'PAGES'                                                
         LA    R2,6(R2)                                                         
         B     SPCDSC3                                                          
*                                                                               
         CLI   PJOBUIND,C'I'                                                    
         BNE   *+18                                                             
         MVC   1(6,R2),=C'INCHES'                                               
         LA    R2,7(R2)                                                         
         B     SPCDSC3                                                          
*                                                                               
SPCDSC3  OC    PJOBCOLS,PJOBCOLS                                                
         BZ    PREM                                                             
         MVI   1(R2),C'X'                                                       
         EDIT  (P3,PJOBCOLS),(2,3(R2)),ALIGN=LEFT                               
         MVC   5(4,R2),=C'COLS'                                                 
         B     *+10                                                             
SPCDSC4  MVC   JSPC(L'PJOBSPC),PJOBSPC                                          
PREM     CLC   PJOBPRM(3),SPACES                                                
         BNH   PROD                                                             
*        BE    PROD                                                             
*        OC    PJOBPRM,PJOBPRM                                                  
*        BZ    PROD                                                             
*                                                                               
         LA    R2,JSPC+L'JSPC-1                                                 
         LA    R1,JSPC                                                          
LOOP1    CR    R1,R2                                                            
         BE    *+18                                                             
         CLC   0(1,R2),SPACES                                                   
         BH    *+8                                                              
         BCT   R2,LOOP1                                                         
*                                                                               
         LA    R2,2(R2)                                                         
         MVC   0(5,R2),=C'PREM='                                                
         MVC   5(3,R2),PJOBPRM                                                  
*                                                                               
*                                                                               
PROD     OC    PJOBPROD,PJOBPROD                                                
         BZ    GETJX        NO PROD HOUSE                                       
         XC    KEY,KEY                                                          
         MVC   KEY(3),RCSVAGY                                                   
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),PJOBPROD                                                
         BAS   RE,MYHIGH                                                        
         CLC   KEYSAVE(8),KEY                                                   
         BE    *+6                                                              
         DC    H'0'          REP NOT ON FILE                                    
         LA    R0,PREPREC                                                       
         ST    R0,MYAREC                                                        
         BAS   RE,GETR                                                          
         MVC   PNAME,PREPNAME                                                   
         MVC   PALN1,PREPLIN1      SAVE NAME AND ADDRESS FOR PRINTING           
         MVC   PALN2,PREPLIN2                                                   
         MVC   PATTN,PREPATTN                                                   
*                                                                               
GETJX    DS    0H                                                               
         BAS   RE,GETCLT                                                        
         BAS   RE,GETPRD                                                        
         MVC   KEY(64),SAVKEYS                                                  
         BAS   RE,MYHIGH                                                        
         XIT1                                                                   
         EJECT                                                                  
NEWJOB   NTR1          READ PROD RECORD AND PRINT COMMENTS                      
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'           RESET PAGE                                  
         XC    OLDPUB,OLDPUB                                                    
*                                       SET LABEL 'RETURN' REC                  
         MVI   SLRREC,C' '                                                      
         LA    R1,L'SLRREC-1                                                    
         MOVE  (SLRREC+1,(R1)),SLRREC                                           
*                                                                               
         LA    RF,L'SLRREC+4                                                    
         SLL   RF,16                                                            
         ST    RF,SLRREC-4                                                      
         MVI   SLRCOD,C'R'                                                      
         MVC   SLRNAM,PNAME                                                     
         MVC   SLRLIN1,PALN1                                                    
         MVC   SLRLIN2,PALN2                                                    
*                                  GET INSTRUCTION RECORD                       
         MVC   SAVKEYS,KEY                                                      
         CLC   =C'NONE',QJOB                                                    
         BE    NEWJX                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(16),PJOBREC                                                  
         MVC   KEY+16(6),=6X'FF'                                                
         BAS   RE,MYHIGH                                                        
         CLC   KEY(21),KEYSAVE                                                  
         BNE   NEWJX        NO INS REC                                          
         MVC   MYAREC,ACONIO1      MUST READ INTO CONTRACT AREA                 
         BAS   RE,GETR                                                          
*                                                                               
*                                  CHECK FOR SHIP= COMMENTS                     
         L     R2,ACONIO1          REALLY INSTRUCTION RECORD                    
         LA    R2,33(R2)          TO FIRST ELEMENT                              
         MVI   ECODE,X'66'                                                      
         CLI   0(R2),X'66'                                                      
         BE    NEWJ1AA                                                          
NEWJ1    BAS   RE,NEXTEL                                                        
         BNE   NEWJX                                                            
NEWJ1AA  CLC   2(5,R2),=C'SHIP='                                                
         BNE   NEWJ1AC                                                          
         SR    R6,R6                                                            
         IC    R6,1(R2)                                                         
         SH    R6,=H'8'                                                         
         BM    NEWJ1                                                            
         LA    R5,7(R2)                                                         
         CLI   0(R5),C'+'                                                       
         BNE   NEWJ1AB                                                          
         MVC   SPACING,1(R5)                                                    
         NI    SPACING,X'0F'                                                    
         CLI   SPACING,3                                                        
         BNH   *+8                                                              
         MVI   SPACING,3                                                        
         CLI   SPACING,0                                                        
         BNE   *+8                                                              
         MVI   SPACING,1                                                        
         MVI   P,0                                                              
         MVI   CMSW,1        SO COVER HEADING WILL PRINT                        
         BAS   RE,TLPRT                                                         
         SH    R6,=H'2'                                                         
         LA    R5,2(R5)                                                         
*                                                                               
NEWJ1AB  LTR   R6,R6                                                            
         BM    NEWJ1         ZERO LENGTH                                        
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   P+30(0),0(R5)                                                    
         MVI   CMSW,1        SO COVER PAGE HEADING WILL PRINT                   
         BAS   RE,TLPRT                                                         
         B     NEWJ1                                                            
*                                                                               
NEWJ1AC  DS    0H                                                               
         CLC   2(6,R2),=C'LABEL='                                               
         BNE   NEWJ1AF                                                          
         LA    R4,SLRCOM1                                                       
         LA    R0,3                                                             
NEWJ1AD  DS    0H                                                               
         CLI   0(R4),C' '                                                       
         BNH   NEWJ1AE                                                          
         LA    R4,L'SLRCOM1(R4)                                                 
         BCT   R0,NEWJ1AD                                                       
*                                                                               
         B     NEWJ1                                                            
NEWJ1AE  DS    0H                                                               
         SR    R6,R6                                                            
         IC    R6,1(R2)                                                         
         SH    R6,=H'9'                                                         
         BM    NEWJ1                                                            
         CH    R6,=H'34'                                                        
         BNH   *+8                                                              
         LA    R6,34                                                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),8(R2)                                                    
         B     NEWJ1                                                            
         SPACE 2                                                                
NEWJ1AF  CLC   2(4,R2),=C'COM='                                                 
         BNE   NEWJ1                                                            
         LA    R4,6(R2)                                                         
NEWJ1B0  LR    R5,R4                                                            
NEWJ1B   DS    0H                                                               
         CLI   0(R5),C','                                                       
         BE    NEWJ1C                                                           
         CLI   0(R5),0                                                          
         BE    NEWJ1C                                                           
         CLI   0(R5),X'66'                                                      
         BE    NEWJ1C                                                           
         LA    R5,1(R5)                                                         
         B     NEWJ1B                                                           
*                                                                               
NEWJ1C   DS    0H                                                               
         SR    R5,R4                                                            
         BNP   NEWJ1X                                                           
         CH    R5,=H'6'                                                         
         BH    NEWJ1X                                                           
         MVC   WORK,SPACES                                                      
         LA    R6,WORK+6                                                        
         SR    R6,R5                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R4)                                                    
*                                                                               
         XC    KEY,KEY         FIND SPECIAL COMMENT RECORD                      
         MVC   KEY(3),RCSVAGY                                                   
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),WORK                                                    
         BAS   RE,MYHIGH                                                        
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
NEWJ1X   DC    H'0'               INVALID OR NOT FOUND STND COMMENT             
*             GET NEXT COMMENT NUMBER                                           
         L     R0,ALISREC                                                       
         ST    R0,MYAREC                                                        
         BAS   RE,GETR                                                          
         BAS   RE,FNDSHIP        FIND SHIPPING COMMENTS                         
         AR    R4,R5                                                            
         LA    R4,1(R4)            SINCE R5 WAS BCTR'ED                         
         CLI   0(R4),X'66'                                                      
         BE    NEWJ1XX                                                          
         CLI   0(R4),0                                                          
         BE    NEWJ1XX                                                          
         LA    R4,1(R4)            BUMP PAST DELIMITER                          
         CLC   0(4,R4),=C'COM='                                                 
         BNE   NEWJ1XX                                                          
         LA    R4,4(R4)            PAST COM=                                    
         B     NEWJ1B0                                                          
*                                                                               
NEWJ1XX  MVI   ECODE,X'66'        RESET ECODE                                   
         B     NEWJ1                                                            
*                                                                               
NEWJX    DS    0H                                                               
         MVI   FORCEHED,C'Y'      SO LIST WILL PRINT ON NEW PAGE                
         XC    SVPL1(120),SVPL1                                                 
         MVC   KEY(64),SAVKEYS                                                  
         BAS   RE,MYHIGH                                                        
*                                                                               
         CLI   QOPT2,C'Y'          Y=SUPPRESS LABELS                            
         BE    NEWJXX                                                           
*                                  WRITE LABEL 'RETURN' REC                     
         GOTO1 WORKER,DMCB,=C'ADD',PP79FLN,FILID,SLRREC-4                       
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NEWJXX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
GETPUB   NTR1          GET PUB AND BUILD SHIPPING ADDRESS                       
*                                                                               
*                      FIRST LOOK FOR NEW SHIPPING ADDRESS ELEM                 
*                      ALWAYS USE IF FOUND                                      
*                                                                               
*                      GET PUB AND FIRST TRY FOR TO GET TRA REP                 
*                                  THEN TRY FOR TRAFFIC ADDR ELEM               
*                                  ELSE USE PUB NAME AND ADDR                   
         XC    PUBL1(240),PUBL1      CLEAR 4 60 BYTE ADDR LINES                 
         MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),PBUYKPUB                                                
         MVC   KEY+7(2),PAGYKAGY                                                
         MVI   KEY+9,X'81'                                                      
         CLC   PUBREC(9),KEY         HAVE ALREADY                               
         BE    GETP2X                                                           
         BAS   RE,MYHIGHP                                                       
         CLC   KEY(25),KEYSAVE                                                  
         BE    GETP2                                                            
         CLI   PAGYPROF+16,C'0'          SRDS DEFAULT                           
         BNE   *+8                                                              
         BAS   RE,NOPUB                                                         
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+8                                                              
         BAS   RE,NOPUB                                                         
         MVC   KEY+7(2),=C'ZZ'                                                  
         MVI   KEY+9,X'81'                                                      
         CLC   KEY(25),KEYSAVE                                                  
         BE    GETP2                                                            
         BAS   RE,MYHIGHP                                                       
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
*                                                                               
NOPUB    DC    H'0'                                                             
*                                                                               
GETP2    LA    R0,PUBREC                                                        
         ST    R0,MYAREC                                                        
         BAS   RE,GETP                                                          
         MVC   PUBKPUB(6),KEY+1    FOR PUBEDIT                                  
*                                                                               
GETP2X   DS    0H                  FIRST CHECK FOR SHIPPING ADDRESS             
*                                  IF FOUND - ALWAYS USE                        
*                                                                               
         MVI   ADRTYP,C'S'         SHIPPING                                     
*                                    FILL IN 7 BYTES OF CLTDATA                 
         MVC   CLTDATA(3),PAGYKAGY   CLIENT AGY/MED                             
         MVC   CLTCODE,PBUYKCLT      CLIENT CODE                                
         MVC   CLTOFF,PCLTOFF        CLIENT OFFICE                              
*                                                                               
         GOTO1 =V(PPGETADR),DMCB,(ADRTYP,CLTDATA),PUBREC,DATAMGR,0              
*                                                                               
         CLI   0(R1),X'FF'         ERROR IN CALL ?                              
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         MVC   CLTDATA(4),0(R1)    "RESULT"/ADDREC LEVEL(3)                     
*                                                                               
         L     R2,4(R1)            A(ADDRESS INFO FROM CALL)                    
         ST    R2,FULL             SAVE A(ADDRESS)                              
         CLI   CLTDATA,0           ADDRESS RECORD FOUND ?                       
         BE    GETP4               NO - LOOK FOR TRA REP                        
*                                  ADDRESS REC FOUND                            
         USING PGETADRD,R2                                                      
         MVC   PUBL1(30),PGADNAME                                               
         MVC   PUBL2(30),PGADLIN1                                               
         MVC   PUBL3(30),PGADLIN2                                               
         CLI   PGADATTN,0                                                       
         BE    GETP3                                                            
         CLI   PGADATTN,C' '                                                    
         BE    GETP3                                                            
**NEW 10/3/89                    ATTENTION NOW BEFORE ADDRESS                   
*                                SO ZIP WILL BE IN LAST LINE                    
         XC    PUBL2(30),PUBL2                                                  
         XC    PUBL3(30),PUBL3                                                  
         MVC   PUBL2(5),=C'ATTN-'                                               
         MVC   PUBL2+6(20),PGADATTN                                             
         MVC   PUBL3(30),PGADLIN1                                               
         MVC   PUBL4(30),PGADLIN2                                               
**NEW 10/3/89                                                                   
*                                                                               
GETP3    CLI   PGADKRCD,X'0B'      SHIPPING ADDRESS ?                           
         BE    GETP20              YES - FINISH UP                              
*                                  NO - I HAVE A TRAFFIC ADDRESS                
         DROP  R2                                                               
*                                                                               
GETP4    DS    0H                  TRY FOR A REP                                
         XC    WORK(3),WORK                                                     
         LA    R2,PUBREC+33                                                     
         MVI   ECODE,X'14'                                                      
         CLI   0(R2),X'14'                                                      
         BE    GETP6A                                                           
GETP6    BAS   RE,NEXTEL                                                        
         BNE   GETP14              SEE IF HAVE ADDRESS                          
GETP6A   CLC   2(3,R2),PBUYKCLT                                                 
         BE    GETP7                                                            
         CLC   2(3,R2),=3X'FF'                                                  
         BE    GETP7                                                            
         CLI   2(R2),X'FF'                                                      
         BNE   GETP6                                                            
         CLC   3(1,R2),PCLTOFF                                                  
         BE    GETP7                                                            
         B     GETP6                                                            
*                                                                               
         USING PUBREPEL,R2                                                      
*                                                                               
GETP7    DS    0H                                                               
         OC    PUBTRREP,PUBTRREP                                                
         BZ    GETP6                                                            
*                                                                               
         MVC   WORK(3),2(R2)         SAVE REP LEVEL FOUND                       
         CLI   CLTDATA,0           ADDRESS REC FOUND ?                          
         BE    GETP70A             NO - TRY FOR REP                             
         CLC   CLTDATA+1(3),WORK       CHECK ADDR VS. REP LEVEL                 
         BNH   GETP20              USE THE ADDRESS REC                          
*                                  USE REP IF FOUND                             
GETP70A  XC    KEY,KEY                                                          
         MVC   KEY(2),PUBKAGY        SO I CAN READ ZZ REPS                      
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),PUBTRREP                                                
         CLI   QMEDIA,C'O'                                                      
         BE    GETP7A1                                                          
GETP7AA  DS    0H                                                               
         CLC   PUBTRREP,=4C'0'                                                  
         BE    GETP14              SEE IF HAVE ADDRESS                          
         B     GETP7A                                                           
*                                  OUTDOOR                                      
GETP7A1  DS    0H                                                               
         CLC   PUBTRREP(3),=4C'0'                                               
         BNE   GETP7A                                                           
         CLC   PUBCNREP,=4C'0'                                                  
         BE    GETP7AA                                                          
         MVC   KEY+4(4),PUBCNREP                                                
         MVC   KEY+8(1),PUBTRREP+3 SUFFIX                                       
*                                                                               
GETP7A   CLC   PREPREC(9),KEY                                                   
         BE    GETP7B                                                           
         BAS   RE,MYHIGH                                                        
         CLC   KEY(9),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'        REP NOT ON FILE                                      
*                                                                               
         LA    R0,PREPREC                                                       
         ST    R0,MYAREC                                                        
         BAS   RE,GETR                                                          
*                                                                               
GETP7B   MVC   PUBL1(30),PREPNAME                                               
         MVC   PUBL2(30),PREPLIN1                                               
         MVC   PUBL3(30),PREPLIN2                                               
         CLI   PREPATTN,0                                                       
         BE    GETP20              FINISH UP                                    
         CLI   PREPATTN,C' '                                                    
         BE    GETP20              FINISH UP                                    
**NEW 10/3/89                         ATTENTION NOW BEFORE ADDRESS              
*                                     SO ZIP WILL BE IN LAST LINE               
         XC    PUBL2(30),PUBL2                                                  
         XC    PUBL3(30),PUBL3                                                  
         MVC   PUBL2(5),=C'ATTN-'                                               
         MVC   PUBL2+6(20),PREPATTN                                             
         MVC   PUBL3(30),PREPLIN1                                               
         MVC   PUBL4(30),PREPLIN2                                               
**NEW 10/3/89                                                                   
         B     GETP20              FINISH UP                                    
*                                                                               
         DROP  R2                                                               
*                                                                               
GETP14   DS    0H                                                               
         OC    PUBL1(20),PUBL1        SEE IF I HAVE AN ADDRESS                  
         BNZ   GETP20                                                           
*                                     IF NOT USE PUB NAME                       
         MVC   PUBL1(20),PUBNAME                                                
         MVC   PUBL2(20),PUBZNAME                                               
         LA    R5,PUBL3                                                         
         OC    PUBZNAME,PUBZNAME       NO ZONE                                  
         BNZ   *+8                                                              
         LA    R5,PUBL2                SO PRINT ADDR ON LINE 2                  
         MVC   0(30,R5),PUBLINE1                                                
         MVC   60(30,R5),PUBLINE2                                               
*                                                                               
GETP20   DS    0H                                                               
         CLI   QOPT6,C'N'          SUPRESS DETAILS                              
         BE    GETPX                                                            
         LA    R5,PUBL1+36          MAGS GET PUB NUMBER ONLY                    
         CLI   QMEDIA,C'M'                                                      
         BE    GETP21                                                           
         CLI   QMEDIA,C'S'                                                      
         BE    GETP21                                                           
         CLI   QMEDIA,C'T'                                                      
         BE    GETP21                                                           
         CLI   QMEDIA,C'I'          OR INTERACTIVE                              
         BE    GETP21                                                           
         CLI   QMEDIA,C'L'          OR SOCIAL                                   
         BE    GETP21                                                           
         LA    R5,PUBL2+37                                                      
         MVC   PUBL1+36(20),PUBZNAME                                            
         CLI   QMEDIA,C'O'        OUTDOOR                                       
         BE    GETP21                                                           
         LA    RF,PUBL1+36                                                      
         MVC   0(16,RF),PUBCITY                                                 
         LA    RF,16(RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C','                                                       
         MVC   3(2,RF),PUBSTATE                                                 
*                                                                               
GETP21   DS    0H                                                               
         IC    R4,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R4),PUBKPUB),(R5)                                 
*                                                                               
         CLI   QMEDIA,C'O'                                                      
         BE    GETP30                                                           
         CLI   QMEDIA,C'N'                                                      
         BNE   GETPX                                                            
*                                                                               
GETP30   DS    0H            BRACKET PUBNUMBER FOR OUTDOOR AND                  
         BCTR  R5,0          AND NEWSPAPERS                                     
         MVI   0(R5),C'('                                                       
         LA    R5,20(R5)                                                        
GETP31   CLI   0(R5),C' '                                                       
         BH    GETP32                                                           
         BCTR  R5,0                                                             
         B     GETP31                                                           
*                                                                               
GETP32   MVI   1(R5),C')'                                                       
*                                                                               
GETPX    DS    0H                                                               
         OI    PUBL1,C' '          SO LINES 1 AND 2 WILL ALWAYS PRINT           
         OI    PUBL2,C' '                                                       
         MVC   KEY(64),SAVKEYS                                                  
         BAS   RE,MYHIGH                                                        
         XIT1                                                                   
         EJECT                                                                  
FNDSHIP  NTR1                ROUTINE TO FIND AND PRINT SHIP=                    
*                            COMMENTS FROM A STANDARD COMMENT REC               
*                                                                               
         L     R2,ALISREC                                                       
         LA    R2,33(R2)                                                        
         CLI   0(R2),X'40'                                                      
         BE    FNDS4                                                            
*                                                                               
FNDS2    DS    0H                                                               
         MVI   ECODE,X'40'                                                      
         BAS   RE,NEXTEL                                                        
         BNE   FNDSX                                                            
*                                                                               
FNDS4    CLC   2(5,R2),=C'SHIP='                                                
         BNE   FNDS2                                                            
         SR    R5,R5                                                            
         IC    R5,1(R2)                                                         
         SH    R5,=H'8'                                                         
         LA    R4,7(R2)                                                         
         CLI   0(R4),C'+'                                                       
         BNE   FNDS6         NO SPACING CONTROL                                 
         MVC   SPACING,1(R4)                                                    
         NI    SPACING,X'0F'                                                    
         CLI   SPACING,3                                                        
         BNH   *+8                                                              
         MVI   SPACING,3                                                        
         CLI   SPACING,0                                                        
         BNE   *+8                                                              
         MVI   SPACING,1                                                        
         MVI   P,0                                                              
         MVI   CMSW,1                                                           
         BAS   RE,TLPRT                                                         
*                                                                               
         SH    R5,=H'2'                                                         
         LA    R4,2(R4)                                                         
*                                                                               
FNDS6    LTR   R5,R5                                                            
         BM    FNDS2        NO LENGHT SO GET NEXT ELEM                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P+30(0),0(R4)                                                    
         MVI   P,0                                                              
         MVI   CMSW,1                                                           
         BAS   RE,TLPRT                                                         
         B     FNDS2                                                            
*                                                                               
FNDSX    XIT1                                                                   
         EJECT                                                                  
GETPLN   NTR1                                                                   
         MVI   LSW,0                                                            
         LA    R2,PUBL1                                                         
         LA    R4,4                                                             
GETPL1   CLI   0(R2),0                                                          
         BE    NEXTLN                                                           
         MVC   P+3(60),0(R2)                                                    
         MVI   LSW,1                                                            
         MVI   0(R2),0                                                          
         B     GETPLNX                                                          
*                                                                               
NEXTLN   LA    R2,60(R2)                                                        
         BCT   R4,GETPL1                                                        
*                                                                               
GETPLNX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
MARKREC  NTR1                                                                   
*                                  REREAD PBUYREC BEFORE WRITING IT             
         CLI   QOPT7,C'Y'          SEE IF TEST RUN - DON'T MARK FILE            
         BE    MARKX                                                            
*                                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,MYAREC                                                        
         MVC   KEY+27(4),SVBUYADR                                               
         CLI   RUNSW,0                                                          
         BNE   *+10                                                             
         MVC   KEY+27(4),SRECDA       IF SORTING USE SORT DA                    
*                                                                               
         BAS   RE,GETR                                                          
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ECODE,X'79'                                                      
MKREC1   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   MKREC2                                                           
         USING PSHIPELD,R2                                                      
         OC    PSHIDATE,PSHIDATE                                                
         BNZ   MKREC1                                                           
         B     MKREC3                                                           
*                                                                               
MKREC2   DS    0H                                                               
         XC    WORK(25),WORK                                                    
         MVC   WORK(2),=X'7917'      WAS X'790B'                                
         GOTO1 RECUP,DMCB,(1,PBUYREC),WORK,(R2)                                 
*                                                                               
MKREC3   DS    0H                                                               
         MVC   PSHIDATE,SVTODAY                                                 
         MVC   PSHIJOB,PBDJOB                                                   
         MVC   PSHIOP,BUYOVR      SAVE OVERAGE PCT.                             
         MVC   PSHIPOST,BUYTOT   POSTERS/INSERTIONS                             
         MVC   PSHIOVRG,BUYOTOT  POSTERS INCLUDING OVERAGE (2 DECIMALS)         
*                                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,MYAREC                                                        
         BAS   RE,PUTR                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
MARKX    XIT1                                                                   
         EJECT                                                                  
         SPACE 2                                                                
*                                  PRINTING                                     
TLPRT    NTR1                                                                   
         SPACE 2                                                                
         BAS   RE,TLCKHD                                                        
*                                                                               
TLPRT2   DS    0H                                                               
         CLI   FORCEHED,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,TLHEAD                                                        
         GOTO1 REPORT                                                           
*                                                                               
         MVI   LINENEED,0                                                       
         MVI   CMSW,0                                                           
         B     XIT                                                              
         SPACE 3                                                                
TLHEAD   DS    0H                                                               
         OC    JADNO,JADNO                                                      
         BZ    TLHD0                                                            
         MVC   HEAD7+1(9),=C'AD NO.  ='                                         
         MVC   HEAD7+11(6),JADNO                                                
         MVC   HEAD8+1(9),=C'COPY    ='                                         
         MVC   HEAD8+11(17),JCOPY                                               
         MVC   HEAD9+1(9),=C'CAPTION ='                                         
         MVC   HEAD9+11(25),JCAP1                                               
         CLC   JCAP2(L'JCAP2),SPACES                                            
         BNH   NOCAP2                                                           
         MVC   HEAD10+11(25),JCAP2                                              
         MVC   HEAD11+1(9),=C'AD SPACE='                                        
         MVC   HEAD11+11(34),JSPC                                               
         B     TLHD0                                                            
NOCAP2   MVC   HEAD10+1(9),=C'AD SPACE='                                        
         MVC   HEAD10+11(34),JSPC                                               
TLHD0    OC    PNAME,PNAME        CHECK FOR PROD HOUSE                          
         BZ    TLHD1                                                            
         MVC   HEAD6+51(30),PNAME                                               
         MVC   HEAD7+51(30),PALN1                                               
         MVC   HEAD8+51(30),PALN2                                               
         CLI   PATTN,0                                                          
         BE    *+16                                                             
         MVC   HEAD9+51(5),=C'ATTN-'                                            
         MVC   HEAD9+57(20),PATTN                                               
*                                                                               
TLHD1    DS    0H                                                               
         MVI   RCSUBPRG,0                                                       
         CLI   CMSW,1      COMMENTS TO PRINT ON COVER PAGE                      
         BE    TLHDX                                                            
         MVI   RCSUBPRG,4                                                       
         CLI   QMEDIA,C'O'          OUTDOOR                                     
         BE    TLHDX                                                            
         MVI   RCSUBPRG,3                                                       
         CLI   QMEDIA,C'N'          NEWSPAPERS                                  
         BE    TLHDX                                                            
         MVI   RCSUBPRG,2            OTHERS                                     
TLHDX    DS    0H                                                               
         CLC   JCAP2(L'JCAP2),SPACES                                            
         BHR   RE                                                               
         CLI   RCSUBPRG,3                                                       
         BH    *+16                                                             
         BLR   RE                                                               
         MVI   RCSUBPRG,31                                                      
         BR    RE                                                               
         MVI   RCSUBPRG,41                                                      
         BR    RE                                                               
         SPACE 2                                                                
         SPACE 3                                                                
TLCKHD   DS    0H                                                               
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         IC    R0,LINE                                                          
         IC    RF,LINENEED                                                      
         AR    R0,RF                                                            
         IC    RF,SPACING                                                       
         AR    R0,RF                                                            
         STC   R0,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BLR   RE                                                               
         MVI   FORCEHED,C'Y'                                                    
         BR    RE                                                               
         SPACE 3                                                                
GETCLT   NTR1                                                                   
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(7),PBUYKEY                                                   
         MVI   KEY+3,2                                                          
         BAS   RE,MYREAD                                                        
         LA    R0,PCLTREC                                                       
         ST    R0,MYAREC                                                        
         BAS   RE,GETR                                                          
         MVC   KEY(64),WORK                                                     
         BAS   RE,MYHIGH                                                        
         B     XIT                                                              
         SPACE 3                                                                
GETPRD   NTR1                                                                   
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVI   KEY+3,6                                                          
         BAS   RE,MYREAD                                                        
         LA    R0,PPRDREC                                                       
         ST    R0,MYAREC                                                        
         BAS   RE,GETR                                                          
         MVC   KEY(64),WORK                                                     
         BAS   RE,MYHIGH                                                        
         B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
*                                  DATAMGR LINES                                
*                                                                               
MYHIGH   LA    R0,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     DIR                                                              
*                                                                               
MYSEQ    LA    R0,DMRSEQ                                                        
         B     DIR                                                              
*                                                                               
MYREAD   LA    R0,DMREAD                                                        
         MVC   KEYSAVE,KEY                                                      
*                                                                               
DIR      NTR1                                                                   
         ST    R0,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PRTDIR,KEY,KEY                                     
*                                                                               
         B     DMCHK                                                            
         SPACE 3                                                                
GETR     LA    R0,GETREC                                                        
         B     FILE                                                             
*                                                                               
PUTR     LA    R0,PUTREC                                                        
*                                                                               
FILE     NTR1                                                                   
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,(R0),PRTFILE,KEY+27,MYAREC,(0,DMWORK)               
*                                                                               
DMCHK    DS    0H                                                               
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         SPACE 3                                                                
MYHIGHP  LA   R0,DMRDHI                                                         
         MVC   KEYSAVE,KEY                                                      
DIRPUB   NTR1                                                                   
         SPACE 2                                                                
         GOTO1 DATAMGR,DMCB,(R0),PUBDIR,KEY,KEY                                 
*                                                                               
         B     DMCHK                                                            
         SPACE 3                                                                
GETP     NTR1                                                                   
         SPACE 2                                                                
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,MYAREC,(0,DMWORK)             
*                                                                               
         B     DMCHK                                                            
         EJECT                                                                  
*                                                                               
FIRST    DS    0H                                                               
*                                  ADD LOGO REC TO LABEL FILE                   
         LA    RF,221+1+4                                                       
         SLL   RF,16                                                            
         ST    RF,SLRREC-4                                                      
         MVI   SLRCOD,C'L'                                                      
         L     RF,LOGOC                                                         
         MVC   SLRCOD+1(221),0(RF)                                              
         XC    FILID,FILID                                                      
         MVC   DUB+00(2),RCDATE+6                                               
         MVC   DUB+02(2),RCDATE+0                                               
         MVC   DUB+04(2),RCDATE+3                                               
         MVC   MYTODAY,DUB                                                      
         LA    R4,FILID                                                         
         USING UKRECD,R4                                                        
*                                                                               
         MVC   UKUSRID,RCORIGID                                                 
         MVC   UKSYSPRG,=C'P79'                                                 
         PACK  FULL(2),MYTODAY+4(3)                                             
         MVC   UKDAY,FULL                                                       
         MVI   UKCLASS,C'R'                                                     
         DROP  R4                                                               
         L     RF,=A(WRKBUFF)                                                   
         A     RF,RELO                                                          
         ST    RF,PP79FLN                                                       
         GOTO1 WORKER,DMCB,=C'ADD',PP79FLN,FILID,SLRREC-4                       
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         SPACE 3                                                                
LAST     DS    0H                                                               
         GOTO1 WORKER,DMCB,=C'CLOSE',PP79FLN                                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                 CALCULATE OVERAGE AND RETURN IN DUB                           
*                                                                               
CALCOVR  NTR1                                                                   
         MVI   SEQSW,C'N'     RESET TO 'Y' IF I NEED TO RESET                   
*                             SEQUENCIAL READING                                
         CLI   QOPT1,C'Y'     SEE IF USING 10 PCT.                              
         BE    CALCO20                                                          
*                                                                               
         MVC   BUYOVR,QOPT1                                                     
*                                                                               
         ZIC   R0,QOPT1       MUST HAVE PERCENTAGE                              
CALCO10  CVD   R0,DUB                                                           
         MVC   MYP2,DUB+6                                                       
CALCO15  ZAP   DUB,BUYTOT                                                       
         MP    DUB,=P'100'                                                      
         ZAP   MYDUB,BUYTOT                                                     
         MP    MYDUB,MYP2                                                       
         AP    DUB,MYDUB      DUB SHOULD HAVE TOTAL PLUS OVERAGE                
         B     CALCX          WITH 2 DECIMALS                                   
*                                                                               
CALCO20  DS    0H             TRY TO FIND VALUE FROM SPACE RECORD               
         MVI   SEQSW,C'Y'                                                       
         GOTO1 =V(PPGETSPC),DMCB,(C'B',PBUYREC),DATAMGR,0                       
         CLI   DMCB,X'FF'     ERROR                                             
         BE    CALCO40             USE DEFAULT OF 10 PCT.                       
         CLI   DMCB+4,0                                                         
         BE    CALCO40             USE DEFAULT OF 10 PCT.                       
         CLI   DMCB+4,X'FF'        MEANS APPLY 0 PCT.                           
         BE    CALCO60                                                          
         ZIC   R0,DMCB+4                                                        
         MVC   BUYOVR,DMCB+4                                                    
         B     CALCO10                                                          
*                                                                               
CALCO40  ZAP   MYP2,=P'10'                                                      
         MVI   BUYOVR,10                                                        
         B     CALCO15                                                          
*                                                                               
CALCO60  ZAP   DUB,BUYTOT           NO OVERAGE-SET DUB TO 100 TIMES             
         MP    DUB,=P'100'          BUYTOT (2 DECIMALS)                         
         MVI   BUYOVR,0                                                         
*                                                                               
CALCX    DS    0H                                                               
         ZAP   BUYOTOT,DUB                                                      
         CLI   SEQSW,C'Y'                                                       
         BNE   CALCXX                                                           
         CLI   RUNSW,0              SEE IF SORTING                              
         BE    CALCXX                                                           
         BAS   RE,MYHIGH     NO-  RESET SEQUENCIAL READING                      
CALCXX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
         SPACE 3                                                                
PP79WRKD DSECT                                                                  
RUNSW    DS    X                                                                
SEQSW    DS    CL1                                                              
BSTART   DS    XL3                                                              
BEND     DS    XL3                                                              
MYBPUB   DS    XL6                                                              
*                                                                               
MYTODAY  DS    CL6                                                              
*                                                                               
CONDAT   DS    XL3                 CONTROL DATE TODAY IF NOT SPEC               
SVTODAY  DS    XL3                 SAVED TODAY'S DATE                           
OLDKEY   DS    0CL25                                                            
         DS    CL4                                                              
OLDCLT   DS    CL3                                                              
OLDPRD   DS    CL3                                                              
OLDPUB   DS    CL6                                                              
         DS    CL3                                                              
OLDEST   DS    CL2                                                              
         DS    CL4                                                              
OLDJOB   DS    CL6                                                              
*                                                                               
         DS    0F                                                               
SAVPARS  DS    0CL24                                                            
         DS    6F                                                               
ECODE    DS    X                                                                
BUYOVR   DS    X           OVERAGE PCT FOR BUY                                  
LINENEED DS    X                                                                
SRTLIN   DS    H                                                                
MYAREC   DS    A                                                                
ACONIO1  DS    A           SAVE ADDRESS OF CONIO (INS REC READ INTO)            
PP79FLN  DS    A                                                                
SAVREF   DS    CL15        SAVED REFERENCE NUMBER MESSAGE                       
SAVRPT   DS    CL8         SAVED NUMBER OF REPAINTS                             
SVBUYADR DS    F                                                                
SAVKEYS  DS    CL64                                                             
         DS    0D                                                               
JOBTOTS  DS    PL4                                                              
JOBOTOTS DS    PL4         (INCLUDES OVERAGE - 2 DECIMALS)                      
ADRTOTS  DS    PL4                                                              
ADROTOTS DS    PL4         (INCLUDES OVERAGE - 2 DECIMALS)                      
MKTTOTS  DS    PL4                                                              
MKTOTOTS DS    PL4         (INCLUDES OVERAGE - 2 DECIMALS)                      
*                                                                               
BUYTOT   DS    PL4          POSTER/INSERTION TOTALS                             
BUYOTOT  DS    PL4          POSTERS INCLUDING OVERAGE (2 DECIMALS)              
*                                                                               
MYP2     DS    PL2                                                              
MYDUB    DS    PL8                                                              
*                                                                               
LSW      DS    CL1                                                              
CMSW     DS    CL1                                                              
*                                                                               
TOTWRK   DS    CL16                                                             
TOTTOT   DS    CL8                                                              
*                                                                               
PUBL1    DS    CL60                                                             
PUBL2    DS    CL60                                                             
PUBL3    DS    CL60                                                             
PUBL4    DS    CL60                                                             
*                                                                               
SAVEPLNS DS    CL240                                                            
SAVENEW  DS    CL240                                                            
SVPL1    DS    CL30                                                             
SVPL2    DS    CL30                                                             
SVPL3    DS    CL30                                                             
SVPL4    DS    CL30                                                             
PNAME    DS    CL30                                                             
PALN1   DS    CL30                                                              
PALN2   DS    CL30                                                              
PATTN    DS    CL20                                                             
*                                                                               
JADNO    DS    CL6                                                              
JCOPY    DS    CL17                                                             
JCAP1    DS    CL25                                                             
JCAP2    DS    CL25                                                             
JSPC     DS    CL34                                                             
SPAC1    DS    CL20                                                             
SPAC2    DS    CL20                                                             
SPACX    DS    CL1                                                              
*                                                                               
FILID    DS    CL16                                                             
*                                                                               
ADRTYP   DS    CL1          TYPE OF ADDRESS REC - PAY, TRAFFIC, ETC.            
CLTDATA  DS    0CL7         USED TO PASS KEY INFO TO PPGETADR MODULE            
CLTAGY   DS    CL2                                                              
CLTMED   DS    CL1                                                              
CLTCODE  DS    CL3                                                              
CLTOFF   DS    CL1                                                              
*                                                                               
       ++INCLUDE PP79BRECS                                                      
*                                                                               
         SPACE 3                                                                
SORTRECD DSECT                                                                  
SORTREC  DS    0CL92                                                            
SORTKEY  DS    0CL88                                                            
SKCLT    DS    CL3                                                              
SKPRD    DS    CL3                                                              
SKEST    DS    XL2                                                              
SKJOB    DS    CL6                                                              
SKPUB    DS    CL30                                                             
SKPUB1   DS    CL10                                                             
SKMKT    DS    CL20                                                             
SKPUB2   DS    CL6                                                              
SKDAT1   DS    CL3                                                              
SKDAT2   DS    CL3                                                              
SKLIN    DS    CL2                 TO PRESERV ORIG SEQ                          
SRECDA   DS    CL4                                                              
         SPACE 3                                                                
*                                                                               
PSHIPELD DSECT                                                                  
       ++INCLUDE PSHIPEL                                                        
*                                                                               
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE DDLOGOD                                                        
*                                                                               
WRKBUFF  CSECT                                                                  
         DS    4096X                                                            
         SPACE 3                                                                
*                                                                               
*                                                                               
       ++INCLUDE PPMODEQU                                                       
*                                                                               
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
*                                                                               
PPWORKD  DSECT                                                                  
         ORG   QREGION                                                          
QJOB     DS    CL6                                                              
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
*                                                                               
         PRINT ON                                                               
         EJECT                                                                  
PBREFELD DSECT                                                                  
       ++INCLUDE PBREFEL                                                        
         EJECT                                                                  
PBRPTELD DSECT                                                                  
       ++INCLUDE PBRPTEL                                                        
         EJECT                                                                  
PGETADRD DSECT                                                                  
       ++INCLUDE PPGETADRD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039PPREP7902 07/09/14'                                      
         END                                                                    
